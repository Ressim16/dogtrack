# =============================================================================
# R/pipeline.R
# Package : dogtrack
# Purpose : Top-level orchestration functions.
#
#   run_pipeline()  -- runs the full 4-stage quality-control pipeline in a
#                     single call, from raw data directory to flagged tibble
#   flag_summary()  -- produces a cumulative flag summary across all filters
# =============================================================================


#' Run the full GPS quality-control pipeline
#'
#' Orchestrates the complete `dogtrack` preprocessing pipeline in a single
#' call:
#' 1. Load metadata from Excel workbook
#' 2. Build prefix-to-site routing map
#' 3. Load static GPS test files and derive site-specific thresholds
#' 4. Load dog GPS CSV files and join with metadata
#' 5. Apply HDOP filter
#' 6. Apply displacement speed filter
#' 7. Apply turning angle filter
#' 8. Apply ellipsoidal height filter
#'
#' @param data_dir Character. Path to the directory containing dog GPS CSV
#'   files and static test CSV files (may be mixed or in sub-folders).
#' @param metadata_path Character. Path to the metadata `.xlsx` workbook.
#' @param hdop_threshold Numeric. HDOP ceiling. Fixes with
#'   `HDOP > hdop_threshold` are flagged. Default `4.9`.
#' @param speed_threshold Numeric. Displacement speed ceiling (m/s). Fixes
#'   exceeding this speed are flagged. Default `5.556` (Dürr & Ward, 2014).
#' @param angle_threshold Numeric. Turning angle ceiling (degrees). Fixes
#'   exceeding this angle are flagged. Default `150`.
#' @param height_overrides Named list. Manual `height_threshold` overrides
#'   for sites without static test files, keyed by `field_site`
#'   (e.g. `list("N'Djamena" = 450)`). Required for any site lacking
#'   static tests.
#' @param dist_noise_overrides Named list. Manual `dist_noise_m` overrides
#'   for sites without static test files. When absent, the Uganda Masaka
#'   empirical value (43 m) is used as a conservative fallback.
#'
#' @return A tibble with one row per GPS fix, containing all original device
#'   columns, joined metadata, and four flag columns:
#'   - `flag_hdop`   : logical
#'   - `flag_speed`  : logical
#'   - `flag_angle`  : logical
#'   - `flag_height` : logical
#'
#'   Also attaches a `thresholds` attribute (accessible via
#'   `attr(result, "thresholds")`) recording the site-specific thresholds
#'   used for `dist_noise_m` and `height_threshold`.
#'
#' @details
#' **Flag philosophy:** no fixes are deleted. Each flag column is
#' independent and additive. Analysts combine flags at analysis time:
#' ```r
#' clean <- dplyr::filter(
#'   result,
#'   !flag_hdop, !flag_speed, !flag_angle, !flag_height
#' )
#' ```
#'
#' **Processing order:** HDOP -> speed -> angle -> height. This order is
#' required: speed calculations on bad-HDOP fixes would produce artefactual
#' velocities; angle calculations on teleportation artefacts would produce
#' spurious turning angles.
#'
#' **Static tests:** if no static test files are found for a site, a
#' `height_override` must be provided for that site. `dist_noise_m`
#' falls back to 43 m (Uganda Masaka empirical value) with a warning.
#'
#' @references
#' Dürr, S., & Ward, M. P. (2014). Development of a novel rabies simulation
#' model for application in a non-endemic environment.
#' *PLOS Neglected Tropical Diseases*, 8(6), e2876.
#'
#' @examples
#' \dontrun{
#' result <- run_pipeline(
#'   data_dir      = "data/",
#'   metadata_path = "data/metadata.xlsx"
#' )
#'
#' # Sites without static tests require a height override
#' result <- run_pipeline(
#'   data_dir         = "data/",
#'   metadata_path    = "data/metadata.xlsx",
#'   height_overrides = list("N'Djamena" = 450)
#' )
#'
#' flag_summary(result)
#' }
#'
#' @export
run_pipeline <- function(data_dir,
                         metadata_path,
                         hdop_threshold       = 4.9,
                         speed_threshold      = 5.556,
                         angle_threshold      = 150,
                         height_overrides     = list(),
                         dist_noise_overrides = list()) {
  
  cat("=== dogtrack pipeline ===\n\n")
  
  # -- Step 1 : metadata -------------------------------------------------------
  cat("--- Step 1 / 5 : Loading metadata ---\n")
  metadata <- load_metadata(metadata_path)
  pfx_map  <- build_prefix_map(metadata)
  cat(sprintf("  %d dogs across %d sites: %s\n\n",
              nrow(metadata),
              nrow(pfx_map),
              paste(pfx_map$field_site, collapse = ", ")))
  
  # -- Step 2 : static tests & thresholds -------------------------------------
  cat("--- Step 2 / 5 : Deriving site thresholds from static tests ---\n")
  static_data <- load_static_tests(data_dir, pfx_map)
  
  # -- Step 3 : load dog GPS data ----------------------------------------------
  cat("\n--- Step 3 / 5 : Loading dog GPS data ---\n")
  dog_data <- load_dog_data(data_dir, metadata)
  cat("\n")
  
  # Derive thresholds now that dog_data is available for the height guard
  thresholds <- derive_site_thresholds(
    static_data          = static_data,
    prefix_map           = pfx_map,
    dog_data             = dog_data,
    hdop_threshold       = hdop_threshold,
    height_overrides     = height_overrides,
    dist_noise_overrides = dist_noise_overrides
  )
  
  # -- Step 4 : filters --------------------------------------------------------
  cat("--- Step 4 / 5 : Applying quality filters ---\n")
  dog_data <- filter_hdop(dog_data,   hdop_threshold  = hdop_threshold)
  dog_data <- filter_speed(dog_data,  speed_threshold = speed_threshold)
  dog_data <- filter_angle(dog_data,  thresholds      = thresholds,
                           angle_threshold = angle_threshold)
  dog_data <- filter_height(dog_data, thresholds      = thresholds)
  cat("\n")
  
  # -- Step 5 : cumulative summary ---------------------------------------------
  cat("--- Step 5 / 5 : Cumulative flag summary ---\n")
  print(flag_summary(dog_data), n = Inf)
  
  # Attach thresholds as an attribute for traceability
  attr(dog_data, "thresholds") <- thresholds
  
  cat("\n=== Pipeline complete ===\n")
  cat(sprintf("Output: %d fixes across %d dogs (%d sites)\n",
              nrow(dog_data),
              dplyr::n_distinct(dog_data$file_key),
              dplyr::n_distinct(dog_data$field_site)))
  
  invisible(dog_data)
}


#' Summarise cumulative flagging across all four filters
#'
#' Computes per-site and overall flag counts and percentages for each of
#' the four quality-control filters applied by [run_pipeline()].
#'
#' @param dog_data A tibble as returned by [run_pipeline()] or after
#'   sequential application of [filter_hdop()], [filter_speed()],
#'   [filter_angle()], and [filter_height()].
#'
#' @return A tibble with one row per field site plus one `"ALL"` row,
#'   containing:
#'   - `field_site`   : character
#'   - `n_total`      : integer -- total fixes
#'   - `n_hdop`       : integer -- fixes flagged by HDOP filter
#'   - `n_speed`      : integer -- fixes flagged by speed filter
#'   - `n_angle`      : integer -- fixes flagged by angle filter
#'   - `n_height`     : integer -- fixes flagged by height filter
#'   - `n_any`        : integer -- fixes flagged by at least one filter
#'   - `n_clean`      : integer -- fixes passing all filters
#'   - `pct_any`      : numeric -- percentage flagged by any filter
#'   - `pct_clean`    : numeric -- percentage passing all filters
#'
#' @examples
#' \dontrun{
#' result <- run_pipeline("data/", "data/metadata.xlsx")
#' flag_summary(result)
#' }
#'
#' @export
flag_summary <- function(dog_data) {
  
  required <- c("flag_hdop", "flag_speed", "flag_angle", "flag_height",
                "field_site")
  missing  <- setdiff(required, names(dog_data))
  if (length(missing) > 0L) {
    stop(
      "flag_summary(): missing column(s): ",
      paste(missing, collapse = ", "), "\n",
      "Run run_pipeline() or all four filter functions before calling ",
      "flag_summary()."
    )
  }
  
  .summarise_flags <- function(df) {
    df |>
      dplyr::summarise(
        n_total  = dplyr::n(),
        n_hdop   = sum(flag_hdop   == TRUE, na.rm = TRUE),
        n_speed  = sum(flag_speed  == TRUE, na.rm = TRUE),
        n_angle  = sum(flag_angle  == TRUE, na.rm = TRUE),
        n_height = sum(flag_height == TRUE, na.rm = TRUE),
        n_any    = sum(
          flag_hdop | flag_speed | flag_angle | flag_height,
          na.rm = TRUE
        ),
        n_clean  = sum(
          !flag_hdop & !flag_speed & !flag_angle & !flag_height,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        pct_any   = round(100 * n_any   / n_total, 3),
        pct_clean = round(100 * n_clean / n_total, 3)
      )
  }
  
  per_site <- dog_data |>
    dplyr::group_by(field_site) |>
    .summarise_flags()
  
  overall <- dog_data |>
    dplyr::mutate(field_site = "ALL") |>
    dplyr::group_by(field_site) |>
    .summarise_flags()
  
  dplyr::bind_rows(per_site, overall)
}
