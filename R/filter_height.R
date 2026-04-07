# =============================================================================
# R/filter_height.R
# Package : dogtrack
# Purpose : Flag GPS fixes with implausible ellipsoidal HEIGHT values.
#
#   Dogs are ground-level animals. Fixes recorded above a site-specific
#   ceiling derived from static GPS tests cannot represent a ground-level
#   position and are flagged as positional artefacts (vertical scatter
#   from poor satellite geometry or multipath reflection).
#
#   HEIGHT in the Columbus P-10 Pro output is GPS-derived ellipsoidal
#   height (metres above the WGS84 ellipsoid), not altitude above sea
#   level. The two differ by the geoid undulation (~20--50 m in Uganda
#   and Chad). Thresholds are derived in ellipsoidal units from static
#   test data and must not be confused with topographic elevation.
#
#   flag_height = TRUE  -> HEIGHT exceeds site ceiling (implausible)
#   flag_height = FALSE -> HEIGHT within acceptable range
#   flag_height = NA    -> HEIGHT value missing
#
# Exported functions:
#   filter_height() -- appends flag_height column
# =============================================================================


#' Flag GPS fixes with implausible ellipsoidal height values
#'
#' Appends a `flag_height` column to the input tibble. For each fix, the
#' site-specific `height_threshold` from `thresholds` is looked up and
#' fixes with `HEIGHT > height_threshold` are flagged `TRUE`.
#'
#' @param dog_data A tibble as returned by [filter_angle()], containing
#'   at minimum columns `HEIGHT` (numeric) and `field_site` (character).
#' @param thresholds A tibble as returned by [derive_site_thresholds()],
#'   containing columns `field_site` and `height_threshold`.
#'
#' @return The input tibble with one additional column:
#'   - `flag_height`: logical -- `TRUE` if flagged, `FALSE` if acceptable,
#'     `NA` if `HEIGHT` is missing.
#'
#' @details
#' **Threshold derivation:** `height_threshold` is the 99.9th percentile
#' of ellipsoidal HEIGHT recorded during static GPS tests at the same
#' site. Since static fixes are recorded at ground level, this percentile
#' represents the upper bound of vertical GPS scatter under field
#' conditions. For sites without static tests, a manual override must be
#' provided to [derive_site_thresholds()].
#'
#' **Columbus P-10 Pro vertical bias:** this device exhibits a systematic
#' positive vertical bias of approximately +150 m relative to the true
#' ellipsoidal height. Thresholds derived from static tests automatically
#' absorb this bias -- no correction is applied.
#'
#' **Relationship to HDOP filter:** extreme HEIGHT values frequently
#' co-occur with high HDOP, as poor satellite geometry degrades both
#' horizontal and vertical accuracy simultaneously. The overlap between
#' `flag_height` and `flag_hdop` is printed to console for diagnostics.
#'
#' **Flag philosophy:** flags are independent and additive. `flag_height`
#' does not interact with other flag columns. Analysts combine flags at
#' analysis time.
#'
#' @examples
#' \dontrun{
#' meta       <- load_metadata("data/metadata.xlsx")
#' pfx_map    <- build_prefix_map(meta)
#' static_df  <- load_static_tests("data/", pfx_map)
#' thresholds <- derive_site_thresholds(static_df, pfx_map)
#' dog_data   <- load_dog_data("data/", meta)
#' dog_data   <- filter_hdop(dog_data)
#' dog_data   <- filter_speed(dog_data)
#' dog_data   <- filter_angle(dog_data, thresholds)
#' dog_data   <- filter_height(dog_data, thresholds)
#' }
#'
#' @export
filter_height <- function(dog_data, thresholds) {
  
  # -- Input validation --------------------------------------------------------
  required_data <- c("HEIGHT", "field_site")
  missing_data  <- setdiff(required_data, names(dog_data))
  if (length(missing_data) > 0L) {
    stop(
      "filter_height(): missing column(s) in dog_data: ",
      paste(missing_data, collapse = ", ")
    )
  }
  
  required_thr <- c("field_site", "height_threshold")
  missing_thr  <- setdiff(required_thr, names(thresholds))
  if (length(missing_thr) > 0L) {
    stop(
      "filter_height(): missing column(s) in thresholds: ",
      paste(missing_thr, collapse = ", ")
    )
  }
  
  sites_missing <- setdiff(
    unique(dog_data$field_site),
    thresholds$field_site
  )
  if (length(sites_missing) > 0L) {
    stop(
      "filter_height(): no height_threshold found for site(s): ",
      paste(sites_missing, collapse = ", "), "\n",
      "Add these sites to the thresholds tibble."
    )
  }
  
  # -- Attach height_threshold per fix and apply flag --------------------------
  result <- dog_data |>
    dplyr::left_join(
      thresholds |> dplyr::select(field_site, height_threshold),
      by = "field_site"
    ) |>
    dplyr::mutate(
      flag_height = dplyr::case_when(
        is.na(HEIGHT)                  ~ NA,
        HEIGHT > height_threshold      ~ TRUE,
        TRUE                           ~ FALSE
      )
    ) |>
    dplyr::select(-height_threshold)
  
  # -- Summary -----------------------------------------------------------------
  n_total   <- nrow(result)
  n_flagged <- sum(result$flag_height == TRUE, na.rm = TRUE)
  n_na      <- sum(is.na(result$flag_height))
  
  cat(sprintf(
    "filter_height(): %d fixes | flagged: %d (%.3f%%) | NA HEIGHT: %d\n",
    n_total, n_flagged, 100 * n_flagged / n_total, n_na
  ))
  
  # Per-site summary
  site_summary <- result |>
    dplyr::group_by(field_site) |>
    dplyr::summarise(
      threshold   = unique(
        thresholds$height_threshold[thresholds$field_site ==
                                      dplyr::first(field_site)]
      ),
      n_flagged   = sum(flag_height == TRUE, na.rm = TRUE),
      pct_flagged = round(100 * n_flagged / dplyr::n(), 3),
      .groups     = "drop"
    )
  cat("filter_height(): per-site summary:\n")
  print(site_summary, n = Inf)
  
  # Overlap with flag_hdop if present
  if ("flag_hdop" %in% names(result)) {
    n_both <- sum(
      result$flag_height == TRUE & result$flag_hdop == TRUE,
      na.rm = TRUE
    )
    n_height_only <- sum(
      result$flag_height == TRUE & result$flag_hdop == FALSE,
      na.rm = TRUE
    )
    cat(sprintf(
      "filter_height(): of %d height-flagged -- %d also HDOP-flagged, %d HDOP-clean\n",
      n_flagged, n_both, n_height_only
    ))
  }
  
  result
}
