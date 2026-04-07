# =============================================================================
# R/utils.R
# Package  : dogtrack
# Purpose  : Canonical low-level helpers shared across the pipeline.
#            - haversine_m()    : great-circle distance in metres
#            - preprocess_gps() : normalise Columbus P-10 Pro coordinate columns
#
# Both functions are vectorised and side-effect free.
# Do NOT duplicate these in other files -- import from here.
# =============================================================================


# -----------------------------------------------------------------------------
#' Haversine great-circle distance
#'
#' Computes the great-circle distance in metres between two geographic
#' coordinate pairs using the Haversine formula. Fully vectorised.
#'
#' @param lat1 Numeric. Latitude of the departure point (decimal degrees).
#' @param lon1 Numeric. Longitude of the departure point (decimal degrees).
#' @param lat2 Numeric. Latitude of the arrival point (decimal degrees).
#' @param lon2 Numeric. Longitude of the arrival point (decimal degrees).
#'
#' @return Numeric vector of distances in metres, same length as the inputs.
#'   Returns `NA` wherever any input is `NA`.
#'
#' @details
#' Uses the mean spherical Earth radius of 6 371 000 m. For the inter-fix
#' distances in dog GPS tracks (up to a few kilometres), the spherical
#' approximation introduces negligible error (< 0.01 %).
#'
#' This function is the **single canonical implementation** in the package.
#' All pipeline steps that require a distance calculation call it from here.
#'
#' @references
#' Sinnott, R. W. (1984). Virtues of the Haversine. *Sky and Telescope*,
#' 68(2), 159.
#'
#' @examples
#' # Approximate distance between two GPS fixes ~1 km apart near Masaka
#' haversine_m(-0.3136, 31.7339, -0.3046, 31.7339)  # ≈ 1000 m
#'
#' # Vectorised: compute step distances for a sequence of fixes
#' lats <- c(-0.31, -0.30, -0.29)
#' lons <- c(31.73, 31.74, 31.75)
#' haversine_m(head(lats, -1), head(lons, -1), tail(lats, -1), tail(lons, -1))
#'
#' @export
haversine_m <- function(lat1, lon1, lat2, lon2) {
  R    <- 6371000L                   # Earth mean spherical radius (metres)
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a    <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  2 * R * atan2(sqrt(a), sqrt(1 - a))
}


# -----------------------------------------------------------------------------
#' Normalise Columbus P-10 Pro GPS coordinate columns
#'
#' Parses the raw `LATITUDE N/S` and `LONGITUDE E/W` columns produced by the
#' Columbus P-10 Pro GPS logger. These columns store coordinates as character
#' strings with a trailing cardinal indicator (e.g. `"0.3136S"`, `"31.7339E"`).
#'
#' This function:
#' 1. Strips the cardinal suffix and applies the correct sign for southern
#'    and western hemispheres.
#' 2. Renames the columns to `lat` and `lon` -- the standard names used
#'    throughout the `dogtrack` pipeline.
#'
#' @param df A data frame containing at minimum the columns `LATITUDE N/S`
#'   and `LONGITUDE E/W` as character vectors, as produced by a Columbus
#'   P-10 Pro CSV export.
#'
#' @return The same data frame with:
#'   - `LATITUDE N/S` removed and replaced by `lat` (numeric, decimal degrees;
#'     negative in the southern hemisphere).
#'   - `LONGITUDE E/W` removed and replaced by `lon` (numeric, decimal degrees;
#'     negative in the western hemisphere).
#'
#' @details
#' The function stops with an informative error if either expected column is
#' absent -- this provides early detection of files from a different GPS device.
#'
#' Cardinal suffixes are matched case-insensitively (`N`, `S`, `E`, `W`,
#' `n`, `s`, `e`, `w`) to handle minor firmware variations.
#'
#' @examples
#' \dontrun{
#' raw <- readr::read_csv(
#'   "UMR001-01.CSV",
#'   col_types = readr::cols(`LATITUDE N/S` = readr::col_character(),
#'                           `LONGITUDE E/W` = readr::col_character(),
#'                           .default       = readr::col_guess())
#' )
#' clean <- preprocess_gps(raw)
#' # clean now has columns `lat` and `lon` instead of `LATITUDE N/S` / `LONGITUDE E/W`
#' }
#'
#' @export
preprocess_gps <- function(df) {

  required <- c("LATITUDE N/S", "LONGITUDE E/W")
  missing  <- setdiff(required, names(df))

  if (length(missing) > 0) {
    stop(
      "preprocess_gps(): expected column(s) not found: ",
      paste(missing, collapse = ", "), ".\n",
      "Are you using a Columbus P-10 Pro CSV export? ",
      "Other GPS devices may produce different column names.\n",
      "Columns present in df: ", paste(names(df), collapse = ", ")
    )
  }

  df |>
    dplyr::mutate(
      lat_sign = dplyr::if_else(stringr::str_detect(`LATITUDE N/S`,  "[Ss]$"), -1L, 1L),
      lon_sign = dplyr::if_else(stringr::str_detect(`LONGITUDE E/W`, "[Ww]$"), -1L, 1L),
      lat      = lat_sign * as.numeric(stringr::str_remove(`LATITUDE N/S`,  "[NSns]$")),
      lon      = lon_sign * as.numeric(stringr::str_remove(`LONGITUDE E/W`, "[EWew]$"))
    ) |>
    dplyr::select(-`LATITUDE N/S`, -`LONGITUDE E/W`, -lat_sign, -lon_sign)
}
