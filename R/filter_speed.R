# =============================================================================
# R/filter_speed.R
# Package : dogtrack
# Purpose : Flag GPS fixes that produce biologically implausible displacement
#           speeds between consecutive fixes within the same dog track.
#
#   Speed is computed from Haversine distance and inter-fix time interval.
#   The device SPEED column (Doppler-derived) is NOT used -- it reflects
#   instantaneous velocity and is not suited for position-pair validation.
#
#   flag_speed = TRUE  -> displacement speed exceeds threshold (artefact)
#   flag_speed = FALSE -> speed within biological range
#   flag_speed = NA    -> speed cannot be computed (first fix, duplicate
#                        timestamp, or reversed timestamp)
#
#   Threshold : 5.556 m/s (20 km/h) -- Dürr & Ward (2014)
#
# Exported functions:
#   filter_speed() -- appends step_dist_m, speed_mps, flag_speed columns
# =============================================================================


#' Flag GPS fixes with biologically implausible displacement speeds
#'
#' Computes the Haversine displacement speed between every consecutive pair
#' of fixes within each dog track, then flags fixes where speed exceeds
#' `speed_threshold`. Appends three columns: `step_dist_m`, `speed_mps`,
#' and `flag_speed`.
#'
#' @param dog_data A tibble as returned by [filter_hdop()], containing at
#'   minimum columns `file_key`, `datetime`, `lat`, `lon`.
#' @param speed_threshold Numeric. Fixes where the displacement speed to
#'   the next fix exceeds this value (m/s) are flagged. Default `5.556`
#'   m/s (20 km/h), from Dürr & Ward (2014).
#'
#' @return The input tibble with three additional columns:
#'   - `step_dist_m`: numeric -- Haversine distance to the previous fix
#'     within the same dog track (metres); `NA` for the first fix.
#'   - `speed_mps`  : numeric -- displacement speed (m/s); `NA` for the
#'     first fix, duplicate timestamps, or reversed timestamps.
#'   - `flag_speed` : logical -- `TRUE` if flagged, `FALSE` if acceptable,
#'     `NA` if speed could not be computed.
#'
#' @details
#' **Speed computation:** speed is computed as `step_dist_m / delta_t_sec`
#' between point *i-1* and point *i*, grouped strictly within each dog
#' track (`file_key`). It is never computed across dogs. The data are
#' sorted by `datetime` within each dog before computation.
#'
#' **Edge cases:**
#' - First fix of each dog: `speed_mps = NA`
#' - `delta_t_sec <= 0` (duplicate or reversed timestamps): `speed_mps = NA`
#'
#' **Flag assignment:** `flag_speed = TRUE` is placed on the **arrival**
#' point (point *i*), not the departure (point *i-1*). The artefactual
#' position is more likely at the anomalous fix.
#'
#' **Relationship to HDOP filter:** speed is computed on all fixes
#' regardless of `flag_hdop`. The overlap between `flag_hdop` and
#' `flag_speed` is informative -- most speed-flagged fixes should also be
#' HDOP-flagged. Fixes that are speed-flagged but HDOP-clean warrant
#' manual investigation.
#'
#' **GPS noise floor:** after HDOP filtering (threshold 4.9), residual
#' positional error can reach ~43 m (p95, static tests). Two consecutive
#' fixes each displaced 43 m from their true position could produce an
#' apparent speed of up to 86/70 ≈ 1.2 m/s of pure noise at a 70-second
#' fix interval. The 5.556 m/s threshold sits well above this noise floor.
#'
#' @references
#' Dürr, S., & Ward, M. P. (2014). Development of a novel rabies simulation
#' model for application in a non-endemic environment.
#' *PLOS Neglected Tropical Diseases*, 8(6), e2876.
#'
#' @examples
#' \dontrun{
#' meta     <- load_metadata("data/metadata.xlsx")
#' dog_data <- load_dog_data("data/", meta)
#' dog_data <- filter_hdop(dog_data)
#' dog_data <- filter_speed(dog_data)
#'
#' # Inspect fixes flagged by speed but not HDOP
#' dog_data |>
#'   dplyr::filter(flag_speed == TRUE, flag_hdop == FALSE) |>
#'   dplyr::select(file_key, datetime, speed_mps, step_dist_m,
#'                 delta_t_sec, flag_hdop)
#' }
#'
#' @export
filter_speed <- function(dog_data, speed_threshold = 5.556) {
  
  required <- c("file_key", "datetime", "lat", "lon")
  missing  <- setdiff(required, names(dog_data))
  if (length(missing) > 0L) {
    stop(
      "filter_speed(): missing column(s): ",
      paste(missing, collapse = ", ")
    )
  }
  
  if (!is.numeric(speed_threshold) || length(speed_threshold) != 1L ||
      speed_threshold <= 0) {
    stop("filter_speed(): speed_threshold must be a single positive number.")
  }
  
  result <- dog_data |>
    dplyr::arrange(file_key, datetime) |>
    dplyr::group_by(file_key) |>
    dplyr::mutate(
      # Recompute delta_t_sec within each dog -- safe after any reordering
      delta_t_sec = as.numeric(
        difftime(datetime, dplyr::lag(datetime), units = "secs")
      ),
      
      step_dist_m = haversine_m(
        dplyr::lag(lat), dplyr::lag(lon),
        lat, lon
      ),
      
      speed_mps = dplyr::case_when(
        is.na(delta_t_sec)                          ~ NA_real_,
        delta_t_sec <= 0                            ~ NA_real_,
        # delta_t below noise floor: displacement indistinguishable from
        # GPS positional error (dist_noise_m / speed_threshold = 43/5.556 ≈ 8s)
        delta_t_sec < (43 / speed_threshold)        ~ NA_real_,
        TRUE                                        ~ step_dist_m / delta_t_sec
      ),
      
      flag_speed = dplyr::case_when(
        is.na(speed_mps)             ~ NA,
        speed_mps > speed_threshold  ~ TRUE,
        TRUE                         ~ FALSE
      )
    ) |>
    dplyr::ungroup()
  
  # -- Summary ----------------------------------------------------------------
  n_total    <- nrow(result)
  n_flagged  <- sum(result$flag_speed == TRUE,  na.rm = TRUE)
  n_na       <- sum(is.na(result$flag_speed))
  n_dt_noise <- sum(!is.na(result$delta_t_sec) &
                      result$delta_t_sec > 0 &
                      result$delta_t_sec < (43 / speed_threshold),
                    na.rm = TRUE)
  
  cat(sprintf(
    "filter_speed(): %d fixes | flagged: %d (%.3f%%) | NA speed: %d | delta_t below noise floor (< %.0fs): %d\n",
    n_total, n_flagged, 100 * n_flagged / n_total,
    n_na, 43 / speed_threshold, n_dt_noise
  ))
  
  # -- Overlap with flag_hdop if present --------------------------------------
  if ("flag_hdop" %in% names(result)) {
    overlap <- result |>
      dplyr::filter(flag_speed == TRUE) |>
      dplyr::summarise(
        also_hdop_flagged = sum(flag_hdop == TRUE, na.rm = TRUE),
        only_speed_flagged = sum(flag_hdop == FALSE, na.rm = TRUE)
      )
    cat(sprintf(
      "filter_speed(): of %d speed-flagged fixes -- %d also HDOP-flagged, %d HDOP-clean (investigate)\n",
      n_flagged,
      overlap$also_hdop_flagged,
      overlap$only_speed_flagged
    ))
  }
  
  result
}
