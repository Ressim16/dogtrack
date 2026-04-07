# =============================================================================
# R/globals.R
# Package : dogtrack
# Purpose : Suppress R CMD check NOTEs about "no visible binding for global
#           variable" caused by dplyr's non-standard evaluation (NSE).
#           All names listed here are data column names used inside
#           dplyr verbs (mutate, filter, summarise, etc.).
# =============================================================================

utils::globalVariables(c(
  # coordinate and device columns
  "LATITUDE N/S", "LONGITUDE E/W", "lat_sign", "lon_sign",
  "lat", "lon", "HEIGHT", "HDOP", "DATE", "TIME", "SPEED",
  "date_str", "time_str", "datetime",
  
  # file routing columns
  "file_key", "file_path", "base_key", "session", "keep",
  "prefix_2", "field_site", "sheet_name", "join_key",
  "static_test_id",
  
  # metadata columns
  "household_id", "dog_id",
  
  # computed geometry columns
  "centroid_lat", "centroid_lon", "dist_m", "dist_noise_m",
  "p95_dist", "hdop_bin", "height_threshold", "height_source",
  "median_dog_height",
  
  # flag columns
  "flag_hdop", "flag_speed", "flag_angle", "flag_height",
  ".is_dupe",
  
  # summary columns
  "n", "n_total", "n_any", "n_clean"
))

# Import stats functions used in dplyr::summarise contexts
#' @importFrom stats median quantile
#' @importFrom rlang :=
NULL
