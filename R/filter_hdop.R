# =============================================================================
# R/filter_hdop.R
# Package : dogtrack
# Purpose : Flag GPS fixes with poor horizontal accuracy based on HDOP.
#
#   flag_hdop = TRUE  -> HDOP exceeds threshold (unreliable position)
#   flag_hdop = FALSE -> HDOP within acceptable range
#   flag_hdop = NA    -> HDOP value missing, cannot assess quality
#
#   Threshold : 4.9 (universal -- Columbus P-10 Pro hardware cap is 5.0;
#               empirically validated against static test data from Uganda)
#
# Exported functions:
#   filter_hdop() -- appends flag_hdop column to a dog GPS tibble
# =============================================================================


#' Flag GPS fixes with poor horizontal dilution of precision (HDOP)
#'
#' Appends a `flag_hdop` column to the input tibble. Fixes with `HDOP`
#' exceeding `hdop_threshold` are flagged `TRUE`. The original data are
#' never modified or deleted -- flagging is additive.
#'
#' @param dog_data A tibble as returned by [load_dog_data()], containing
#'   at minimum a numeric `HDOP` column.
#' @param hdop_threshold Numeric. Fixes with `HDOP > hdop_threshold` are
#'   flagged. Default `4.9`, which corresponds to the hardware reporting
#'   cap of the Columbus P-10 Pro (max recorded HDOP = 5.0) and is
#'   validated by static test data from Uganda field sites.
#'
#' @return The input tibble with one additional column:
#'   - `flag_hdop`: logical -- `TRUE` if flagged, `FALSE` if acceptable,
#'     `NA` if `HDOP` is missing.
#'
#' @details
#' **Why HDOP and not raw positional error?** HDOP is recorded at fix time
#' by the GPS receiver and reflects satellite geometry. It is the only
#' real-time accuracy indicator available in the Columbus P-10 Pro output.
#' Static test analysis confirms that fixes with `HDOP <= 4.9` have a
#' worst-case p95 positional error of ~43 m, acceptable for
#' household-scale contact modelling.
#'
#' **Flag philosophy:** flags are independent and additive. `flag_hdop`
#' does not interact with `flag_speed`, `flag_angle`, or `flag_height`.
#' Analysts combine flags at analysis time according to their needs.
#'
#' **Processing order:** HDOP filtering must precede speed, angle, and
#' height filtering. Speed calculations between a bad-HDOP fix and its
#' neighbour would produce artefactual velocities that contaminate
#' downstream filters.
#'
#' @references
#' Langley, R. B. (1999). Dilution of precision. *GPS World*, 10(5), 52--59.
#'
#' @examples
#' \dontrun{
#' meta     <- load_metadata("data/metadata.xlsx")
#' dog_data <- load_dog_data("data/", meta)
#' dog_data <- filter_hdop(dog_data)
#'
#' # Overall flag rate
#' mean(dog_data$flag_hdop, na.rm = TRUE)
#'
#' # Per-dog summary
#' dplyr::summarise(
#'   dplyr::group_by(dog_data, file_key),
#'   pct_flagged = round(100 * mean(flag_hdop, na.rm = TRUE), 1)
#' )
#' }
#'
#' @export
filter_hdop <- function(dog_data, hdop_threshold = 4.9) {
  
  if (!"HDOP" %in% names(dog_data)) {
    stop(
      "filter_hdop(): column 'HDOP' not found in dog_data.\n",
      "Columns present: ", paste(names(dog_data), collapse = ", ")
    )
  }
  
  if (!is.numeric(hdop_threshold) || length(hdop_threshold) != 1L ||
      hdop_threshold <= 0) {
    stop("filter_hdop(): hdop_threshold must be a single positive number.")
  }
  
  result <- dog_data |>
    dplyr::mutate(
      flag_hdop = dplyr::case_when(
        is.na(HDOP)            ~ NA,
        HDOP > hdop_threshold  ~ TRUE,
        TRUE                   ~ FALSE
      )
    )
  
  # -- Summary printed to console ---------------------------------------------
  n_total   <- nrow(result)
  n_flagged <- sum(result$flag_hdop == TRUE, na.rm = TRUE)
  n_na      <- sum(is.na(result$flag_hdop))
  
  cat(sprintf(
    "filter_hdop(): %d fixes | flagged: %d (%.2f%%) | NA HDOP: %d | threshold: HDOP > %.1f\n",
    n_total,
    n_flagged, 100 * n_flagged / n_total,
    n_na,
    hdop_threshold
  ))
  
  result
}
