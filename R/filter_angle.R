# =============================================================================
# R/filter_angle.R
# Package : dogtrack
# Purpose : Flag GPS fixes (point B of a triplet A->B->C) that produce
#           biologically implausible turning angles.
#
#   Two conditions must BOTH be true to flag point B:
#     (1) Both steps represent genuine displacement:
#           dist_AB > dist_noise_m  AND  dist_BC > dist_noise_m
#           Below this threshold a displacement is indistinguishable from
#           GPS noise and the turning angle is meaningless.
#     (2) The turning angle at B exceeds angle_threshold (default 150 deg)
#           Empirically derived from the bimodal distribution of evaluable
#           triplets -- biological peak at low angles, artefact spike near
#           180 deg (U-turn). Threshold sits at the base of the artefact
#           population. Pattern is consistent across all Columbus P-10 Pro
#           devices regardless of field site.
#
#   flag_angle = TRUE  -> turning angle exceeds threshold (artefact)
#   flag_angle = FALSE -> angle within biological range, or triplet not
#                        evaluable (steps too short)
#   flag_angle = NA    -> angle could not be computed (first 2 fixes per
#                        dog, or NA coordinates)
#
# Exported functions:
#   filter_angle() -- appends turning_angle_deg, dist_AB, dist_BC,
#                    both_moving, flag_angle columns
# =============================================================================


#' Flag GPS fixes with biologically implausible turning angles
#'
#' For each consecutive triplet A -> B -> C within a dog track, computes
#' the turning angle at B and flags point B when both steps exceed
#' `dist_noise_m` (genuine displacement) and the angle exceeds
#' `angle_threshold` (artefact U-turn).
#'
#' @param dog_data A tibble as returned by [filter_speed()], containing
#'   at minimum columns `file_key`, `datetime`, `lat`, `lon`,
#'   `field_site`.
#' @param thresholds A tibble as returned by [derive_site_thresholds()],
#'   containing columns `field_site` and `dist_noise_m`. Used to apply
#'   the correct site-specific distance gate.
#' @param angle_threshold Numeric. Turning angles exceeding this value
#'   (degrees) trigger the flag. Default `150`, empirically derived from
#'   the bimodal distribution of evaluable triplets across all Uganda
#'   field sites. Stable across Columbus P-10 Pro devices.
#'
#' @return The input tibble with five additional columns:
#'   - `dist_AB`          : numeric -- Haversine distance A->B (metres)
#'   - `dist_BC`          : numeric -- Haversine distance B->C (metres)
#'   - `both_moving`      : logical -- `TRUE` if both steps exceed
#'     `dist_noise_m` for the site
#'   - `turning_angle_deg`: numeric -- angle at B in \[0 deg, 180 deg\];
#'     `NA` for the first 2 fixes per dog
#'   - `flag_angle`       : logical -- `TRUE` if flagged, `FALSE`
#'     otherwise, `NA` for non-evaluable fixes
#'
#' @details
#' **Direction vectors** are projected onto a local plane at B's latitude
#' (`lat_B`), used as the single reference for both vectors AB and BC.
#' This ensures both vectors share the same coordinate space -- required
#' for the dot product to yield a geometrically correct angle.
#'
#' **Distance gate** (`dist_noise_m`): derived per site from static GPS
#' tests. Points with steps below this threshold are stationary within
#' GPS noise -- their turning angle carries no biological signal. These
#' receive `flag_angle = FALSE`, not `NA`.
#'
#' **Angle threshold (150 deg)**: validated on 65 dogs across Uganda sites.
#' The bimodal distribution shows a biological population (median ~47 deg)
#' and an artefact population rising from 150 deg to a spike at 175--180 deg.
#' The threshold sits at the valley between populations.
#'
#' **Performance:** triplet geometry is computed on a minimal column
#' subset using fully vectorised operations (no `group_by`) for speed on
#' large datasets.
#'
#' **Duplicate timestamps:** fixes with identical `datetime` within a dog
#' track (common in high-frequency recording mode) are deduplicated before
#' triplet computation. Duplicate fixes receive `flag_angle = FALSE`.
#'
#' @references
#' Bjorneraas, K., Van Moorter, B., Rolandsen, C. M., & Herfindal, I.
#' (2010). Screening GPS location data for errors using animal movement
#' characteristics. *Journal of Wildlife Management*, 74(6), 1361--1366.
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
#' }
#'
#' @export
filter_angle <- function(dog_data,
                         thresholds,
                         angle_threshold = 150) {
  
  # -- Input validation --------------------------------------------------------
  required_data <- c("file_key", "datetime", "lat", "lon", "field_site")
  missing_data  <- setdiff(required_data, names(dog_data))
  if (length(missing_data) > 0L) {
    stop(
      "filter_angle(): missing column(s) in dog_data: ",
      paste(missing_data, collapse = ", ")
    )
  }
  
  required_thr <- c("field_site", "dist_noise_m")
  missing_thr  <- setdiff(required_thr, names(thresholds))
  if (length(missing_thr) > 0L) {
    stop(
      "filter_angle(): missing column(s) in thresholds: ",
      paste(missing_thr, collapse = ", ")
    )
  }
  
  sites_missing <- setdiff(
    unique(dog_data$field_site),
    thresholds$field_site
  )
  if (length(sites_missing) > 0L) {
    stop(
      "filter_angle(): no dist_noise_m threshold found for site(s): ",
      paste(sites_missing, collapse = ", "), "\n",
      "Add these sites to the thresholds tibble."
    )
  }
  
  # -- Attach dist_noise_m per fix ---------------------------------------------
  dog_data <- dog_data |>
    dplyr::left_join(
      thresholds |> dplyr::select(field_site, dist_noise_m),
      by = "field_site"
    )
  
  # -- Deduplicate datetime within each dog track ------------------------------
  # Some devices (Columbus P-10 Pro in 1-second mode) record multiple fixes
  # with identical timestamps. Duplicate datetimes break the triplet join.
  # The first occurrence is kept; duplicates receive flag_angle = FALSE.
  # -- Sort once ---------------------------------------------------------------
  dog_data <- dog_data |>
    dplyr::arrange(file_key, datetime)
  
  # -- Handle duplicate timestamps ---------------------------------------------
  # Mark duplicates -- they cannot form a meaningful triplet and receive
  # flag_angle = FALSE at the end. Non-duplicates are used for computation.
  dog_data <- dog_data |>
    dplyr::group_by(file_key, datetime) |>
    dplyr::mutate(.is_dupe = dplyr::row_number() > 1L) |>
    dplyr::ungroup()
  
  n_dupes <- sum(dog_data$.is_dupe)
  if (n_dupes > 0L) {
    warning(
      "filter_angle(): ", n_dupes, " fix(es) with duplicate datetime within ",
      "a dog track -- these fixes receive flag_angle = FALSE."
    )
  }
  
  # -- Vectorised triplet computation on ALL rows ------------------------------
  # Boundary: TRUE where file_key changes OR where current row is a duplicate
  # (duplicates break the sequential triplet structure)
  is_dupe    <- dog_data$.is_dupe
  fkey       <- dog_data$file_key
  
  boundary_1 <- fkey != dplyr::lag(fkey, 1L, default = "") | is_dupe |
    dplyr::lag(is_dupe, 1L, default = FALSE)
  boundary_2 <- fkey != dplyr::lag(fkey, 2L, default = "") | is_dupe |
    dplyr::lag(is_dupe, 1L, default = FALSE) |
    dplyr::lag(is_dupe, 2L, default = FALSE)
  
  lat <- dog_data$lat;  lon <- dog_data$lon
  dns <- dog_data$dist_noise_m
  
  lat_A <- dplyr::lag(lat, 2L); lon_A <- dplyr::lag(lon, 2L)
  lat_B <- dplyr::lag(lat, 1L); lon_B <- dplyr::lag(lon, 1L)
  lat_C <- lat;                  lon_C <- lon
  
  lat_A[boundary_2] <- NA_real_;  lon_A[boundary_2] <- NA_real_
  lat_B[boundary_1] <- NA_real_;  lon_B[boundary_1] <- NA_real_
  lat_B[boundary_2] <- NA_real_;  lon_B[boundary_2] <- NA_real_
  
  dist_AB <- haversine_m(lat_A, lon_A, lat_B, lon_B)
  dist_BC <- haversine_m(lat_B, lon_B, lat_C, lon_C)
  
  lat_ref_rad <- lat_B * pi / 180
  dx_AB <- (lon_B - lon_A) * cos(lat_ref_rad) * 111320
  dy_AB <- (lat_B - lat_A) * 110540
  dx_BC <- (lon_C - lon_B) * cos(lat_ref_rad) * 111320
  dy_BC <- (lat_C - lat_B) * 110540
  
  cos_angle         <- (dx_AB * dx_BC + dy_AB * dy_BC) /
    (sqrt(dx_AB^2 + dy_AB^2) * sqrt(dx_BC^2 + dy_BC^2))
  cos_angle         <- pmax(-1, pmin(1, cos_angle))
  turning_angle_deg <- acos(cos_angle) * 180 / pi
  
  both_moving <- dist_AB > dns & dist_BC > dns
  
  flag_angle <- dplyr::case_when(
    is_dupe                             ~ FALSE,
    is.na(turning_angle_deg)            ~ NA,
    !both_moving                        ~ FALSE,
    turning_angle_deg > angle_threshold ~ TRUE,
    TRUE                                ~ FALSE
  )
  
  # -- Attach results directly -- no join needed --------------------------------
  result <- dog_data |>
    dplyr::mutate(
      dist_AB           = dist_AB,
      dist_BC           = dist_BC,
      both_moving       = both_moving,
      turning_angle_deg = turning_angle_deg,
      flag_angle        = flag_angle
    ) |>
    dplyr::select(-.is_dupe, -dist_noise_m)
  
  # -- Integrity check ---------------------------------------------------------
  if (nrow(result) != nrow(dog_data)) {
    stop(
      "filter_angle(): row count changed (",
      nrow(dog_data), " -> ", nrow(result), ")."
    )
  }
  
  # -- Summary -----------------------------------------------------------------
  n_total     <- nrow(result)
  n_evaluable <- sum(result$both_moving == TRUE, na.rm = TRUE)
  n_flagged   <- sum(result$flag_angle  == TRUE, na.rm = TRUE)
  
  cat(sprintf(
    paste0("filter_angle(): %d fixes | evaluable triplets: %d (%.1f%%) | ",
           "flagged: %d (%.3f%% of all, %.3f%% of evaluable) | ",
           "angle threshold: > %.0f deg\n"),
    n_total,
    n_evaluable, 100 * n_evaluable / n_total,
    n_flagged,
    100 * n_flagged / n_total,
    100 * n_flagged / n_evaluable,
    angle_threshold
  ))
  
  result
}
