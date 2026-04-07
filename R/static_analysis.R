# =============================================================================
# R/static_analysis.R
# Package : dogtrack
# Purpose : Load static GPS test files and derive site-specific thresholds.
#
#   Static tests = GPS collar placed motionless on the ground for an extended
#   period. Any positional variation is pure device + environmental noise.
#
#   Derived thresholds (per field site):
#     dist_noise_m     -- p95 Haversine distance from centroid (metres)
#                        used as the distance gate in filter_angle()
#     height_threshold -- p99.9 ellipsoidal HEIGHT + height_buffer_m
#                        used as the ceiling in filter_height()
#
# Exported functions:
#   load_static_tests()      -- reads all static_*.CSV files, tags by site
#   derive_site_thresholds() -- computes dist_noise_m + height_threshold per site
# =============================================================================


#' Load static GPS test files
#'
#' Scans a directory (recursively) for CSV files whose name starts with
#' `"static"` (case-insensitive). Each file is read, the site is identified
#' from the 2-character prefix immediately following `"static"` in the
#' filename, and that prefix is resolved to a `field_site` via the prefix
#' map built from the metadata.
#'
#' @param data_dir Character. Path to the directory containing the GPS data
#'   files (dog CSVs and static test CSVs may be mixed or in sub-folders).
#' @param prefix_map A tibble as returned by [build_prefix_map()], with
#'   columns `prefix_2` and `field_site`.
#'
#' @return A tibble with one row per GPS fix from all static test files,
#'   containing at minimum:
#'   - `static_test_id` : character -- base filename without extension
#'   - `field_site`     : character -- resolved from prefix map
#'   - `prefix_2`       : character -- 2-character site prefix
#'   - `datetime`       : POSIXct
#'   - `lat`            : numeric -- decimal degrees
#'   - `lon`            : numeric -- decimal degrees
#'   - `HEIGHT`         : numeric -- ellipsoidal height (metres, WGS84)
#'   - `HDOP`           : numeric
#'
#' @details
#' Files whose 2-character prefix is not found in `prefix_map` are skipped
#' with a warning -- this avoids silent data loss if a static file is
#' misnamed.
#' 
#' When `dog_data` is supplied, an automatic consistency check verifies that
#' the derived `height_threshold` exceeds the median GPS fix HEIGHT in the
#' actual dog data. A threshold below the median would flag the majority of
#' legitimate fixes, indicating that the static tests are not representative
#' of the deployment terrain.
#'
#' The Columbus P-10 Pro stores DATE and TIME either as human-readable
#' strings (`"2026-01-07"`, `"09:47:09"`) or as compact numerics without
#' separators (`260107`, `94709`). Both formats are handled automatically.
#'
#' @examples
#' \dontrun{
#' meta      <- load_metadata("data/metadata.xlsx")
#' pfx_map   <- build_prefix_map(meta)
#' static_df <- load_static_tests("data/", pfx_map)
#' dplyr::count(static_df, field_site, static_test_id)
#' }
#'
#' @export
load_static_tests <- function(data_dir, prefix_map) {
  
  if (!fs::dir_exists(data_dir)) {
    stop("load_static_tests(): directory not found: ", data_dir)
  }
  
  all_files    <- fs::dir_ls(data_dir, glob = "*.CSV", recurse = TRUE)
  static_files <- all_files[
    stringr::str_detect(
      stringr::str_to_lower(basename(all_files)), "^static"
    )
  ]
  
  if (length(static_files) == 0L) {
    warning(
      "load_static_tests(): no static test files found in '", data_dir, "'.\n",
      "Expected filenames beginning with 'static' (e.g. staticUM155.CSV).\n",
      "Site-specific thresholds cannot be derived empirically."
    )
    return(
      tibble::tibble(
        static_test_id = character(),
        field_site     = character(),
        prefix_2       = character(),
        datetime       = as.POSIXct(character()),
        lat            = double(),
        lon            = double(),
        HEIGHT         = double(),
        HDOP           = double()
      )
    )
  }
  
  cat("load_static_tests(): found", length(static_files),
      "static file(s).\n")
  
  purrr::map_dfr(static_files, function(file_path) {
    
    fname          <- tools::file_path_sans_ext(basename(file_path))
    # Extract prefix: 2 uppercase letters immediately after "static"
    prefix_2_raw   <- stringr::str_extract(
      stringr::str_to_upper(fname), "(?<=STATIC)[A-Z]{2}"
    )
    
    if (is.na(prefix_2_raw)) {
      warning(
        "load_static_tests(): cannot extract a 2-letter site prefix from '",
        fname, "' -- file skipped."
      )
      return(NULL)
    }
    
    site <- prefix_map$field_site[prefix_map$prefix_2 == prefix_2_raw]
    
    if (length(site) == 0L) {
      warning(
        "load_static_tests(): prefix '", prefix_2_raw,
        "' from file '", fname, "' not found in prefix_map -- file skipped.\n",
        "Known prefixes: ", paste(prefix_map$prefix_2, collapse = ", ")
      )
      return(NULL)
    }
    
    df <- readr::read_csv(
      file_path,
      col_types = readr::cols(
        INDEX           = readr::col_integer(),
        TAG             = readr::col_character(),
        DATE            = readr::col_character(),
        TIME            = readr::col_character(),
        `LATITUDE N/S`  = readr::col_character(),
        `LONGITUDE E/W` = readr::col_character(),
        HEIGHT          = readr::col_double(),
        SPEED           = readr::col_double(),
        HEADING         = readr::col_double(),
        SAT             = readr::col_integer(),
        HDOP            = readr::col_double()
      ),
      show_col_types = FALSE
    ) |>
      preprocess_gps() |>
      dplyr::mutate(
        static_test_id = fname,
        prefix_2       = prefix_2_raw,
        field_site     = site,
        date_str       = stringr::str_pad(
          as.character(DATE), width = 6,
          side = "left", pad = "0"),
        time_str       = stringr::str_pad(
          as.character(TIME), width = 6,
          side = "left", pad = "0"),
        datetime  = lubridate::parse_date_time(
          paste(date_str, time_str),
          orders = c("ymdHMS", "ymd HMS"),
          quiet  = TRUE)
      ) |>
      dplyr::select(-date_str, -time_str)
    
    df
  })
}


#' Derive site-specific thresholds from static GPS test data
#'
#' For each field site, computes two thresholds from the static test fixes:
#'
#' - `dist_noise_m`: the 95th percentile of the Haversine distance from the
#'   per-session centroid. Represents the worst-case positional error at
#'   acceptable HDOP values, used as the distance gate in [filter_angle()].
#'
#' - `height_threshold`: the 99.9th percentile of recorded ellipsoidal HEIGHT
#'   plus `height_buffer_m`. Any dog fix above this value cannot represent a
#'   ground-level position and is flagged by [filter_height()].
#'
#' @param static_data A tibble as returned by [load_static_tests()].
#' @param prefix_map A tibble as returned by [build_prefix_map()], used to
#'   identify which sites have no static test data.
#' @param hdop_threshold Numeric. HDOP ceiling applied to static test fixes' worst bin: 3-5,
#'   before computing `dist_noise_m`. Only fixes with `HDOP <= hdop_threshold`
#'   are used -- this ensures `dist_noise_m` represents the worst-case residual
#'   positional error on fixes that would pass the HDOP filter. Default `4.9`.
#' @param height_overrides A named list of manual `height_threshold` overrides,
#'   keyed by `field_site` (e.g. `list("N'Djamena" = 450)`). Used for sites
#'   without static test files. Ignored for sites that have static data.
#' @param dist_noise_overrides A named list of manual `dist_noise_m` overrides,
#'   keyed by `field_site`. Used for sites without static test files.
#' @param dog_data Optional tibble as returned by [load_dog_data()]. When
#'   provided, the derived `height_threshold` for each site is validated
#'   against the median dog fix HEIGHT. If the threshold falls below the
#'   median, the function stops with an informative error -- this indicates
#'   that the static tests were conducted at a different elevation than the
#'   dog deployment area. Default `NULL` (no validation).
#'
#' @return A tibble with one row per field site, containing:
#'   - `field_site`       : character
#'   - `dist_noise_m`     : numeric (metres)
#'   - `height_threshold` : numeric (metres, ellipsoidal)
#'   - `dist_source`      : character -- `"static_tests"` or `"override"`
#'   - `height_source`    : character -- `"static_tests"` or `"override"`
#'
#' @details
#' Sites present in `prefix_map` but absent from `static_data` will trigger
#' a warning. If neither static data nor an override is provided for a site,
#' the function stops with an informative error -- silent fallback to an
#' arbitrary default is not permitted.
#'
#' @examples
#' \dontrun{
#' meta       <- load_metadata("data/metadata.xlsx")
#' pfx_map    <- build_prefix_map(meta)
#' static_df  <- load_static_tests("data/", pfx_map)
#' thresholds <- derive_site_thresholds(
#'   static_df, pfx_map,
#'   height_overrides = list("N'Djamena" = 450)
#' )
#' print(thresholds)
#' }
#'
#' @export
derive_site_thresholds <- function(static_data,
                                   prefix_map,
                                   dog_data             = NULL,
                                   hdop_threshold       = 4.9,
                                   height_overrides     = list(),
                                   dist_noise_overrides = list()) {
  
  all_sites <- prefix_map$field_site
  
  # -- Sites with static data -------------------------------------------------
  if (nrow(static_data) > 0L) {
    
    empirical <- static_data |>
      dplyr::filter(!is.na(HDOP), HDOP <= hdop_threshold) |>
      dplyr::group_by(field_site, static_test_id) |>
      dplyr::mutate(
        centroid_lat = mean(lat, na.rm = TRUE),
        centroid_lon = mean(lon, na.rm = TRUE),
        dist_m       = haversine_m(lat, lon, centroid_lat, centroid_lon)
      ) |>
      dplyr::ungroup() |>
      # dist_noise_m = p95 of the worst-case HDOP bin (3--threshold) per site.
      # The worst-case bin drives the distance gate -- using the global p95
      # would underestimate positional error because low-HDOP fixes (small
      # errors, large counts) dominate the distribution and mask the tail.
      dplyr::mutate(
        hdop_bin = cut(HDOP,
                       breaks = c(0, 1, 2, 3, hdop_threshold, Inf),
                       labels = c("<=1", "1-2", "2-3",
                                  paste0("3-", hdop_threshold), "above"))
      ) |>
      dplyr::group_by(field_site, static_test_id, hdop_bin) |>
      dplyr::summarise(
        p95_dist = quantile(dist_m, 0.95, na.rm = TRUE),
        .groups  = "drop"
      ) |>
      # Take the maximum p95 across all bins and sessions per site
      dplyr::group_by(field_site) |>
      dplyr::summarise(
        dist_noise_m = max(p95_dist, na.rm = TRUE),
        .groups      = "drop"
      )
    
    # Join height_threshold from a separate summarise on the raw static data
    height_summary <- static_data |>
      dplyr::filter(!is.na(HEIGHT)) |>
      dplyr::group_by(field_site) |>
      dplyr::summarise(
        # p99.9 of static HEIGHT = observed vertical scatter ceiling at ground
        # level. No buffer added -- the static fixes ARE the ground truth.
        height_threshold = quantile(HEIGHT, 0.999, na.rm = TRUE),
        .groups = "drop"
      )
    
    empirical <- empirical |>
      dplyr::left_join(height_summary, by = "field_site") |>
      dplyr::mutate(
        dist_noise_m     = round(dist_noise_m),
        height_threshold = round(height_threshold),
        dist_source      = "static_tests",
        height_source    = "static_tests"
      )
    
  } else {
    empirical <- tibble::tibble(
      field_site       = character(),
      dist_noise_m     = double(),
      height_threshold = double(),
      dist_source      = character(),
      height_source    = character()
    )
  } # -- Guard: validate height_threshold against dog data if provided ----------
  # If dog_data is supplied, check that the derived height_threshold for each
  # site is above the median dog fix HEIGHT. If not, the static tests are not
  # representative of the dog deployment area (different elevation) and the
  # threshold would flag the majority of legitimate fixes.
  # -- Guard: validate height_threshold against dog data if provided ----------
  if (!is.null(dog_data)) {
    
    dog_height_summary <- dog_data |>
      dplyr::group_by(field_site) |>
      dplyr::summarise(
        median_dog_height = median(HEIGHT, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Apply height_overrides BEFORE checking -- a site whose static-derived
    # threshold is too low but has a valid override must not trigger the error.
    empirical_to_check <- empirical |>
      dplyr::mutate(
        height_threshold = dplyr::if_else(
          field_site %in% names(height_overrides),
          purrr::map_dbl(field_site, ~ {
            if (.x %in% names(height_overrides))
              as.double(height_overrides[[.x]])
            else
              NA_real_
          }),
          height_threshold
        )
      )
    
    check <- empirical_to_check |>
      dplyr::left_join(dog_height_summary, by = "field_site") |>
      dplyr::filter(!is.na(median_dog_height)) |>
      dplyr::filter(height_threshold < median_dog_height,
                    !field_site %in% names(height_overrides))
    
    if (nrow(check) > 0L) {
      problem_lines <- purrr::map_chr(seq_len(nrow(check)), function(i) {
        sprintf(
          "  '%s': height_threshold = %d m < median dog HEIGHT = %d m",
          check$field_site[i],
          as.integer(check$height_threshold[i]),
          as.integer(check$median_dog_height[i])
        )
      })
      stop(
        "derive_site_thresholds(): the height_threshold derived from static ",
        "tests is BELOW the median dog fix HEIGHT for the following site(s):\n",
        paste(problem_lines, collapse = "\n"), "\n\n",
        "This means the static tests were conducted at a different elevation ",
        "than the dog deployment area and cannot be used to derive a valid ",
        "height_threshold.\n\n",
        "Solution: provide a manual override:\n",
        paste(
          sprintf("  height_overrides = list('%s' = <value>)",
                  check$field_site),
          collapse = "\n"
        ), "\n",
        "Tip: use the median dog HEIGHT + 250 m as a starting point:\n",
        paste(
          sprintf("  '%s' = %d",
                  check$field_site,
                  as.integer(check$median_dog_height + 250)),
          collapse = "\n"
        )
      )
    }
  }
  
  sites_with_data <- empirical$field_site
  sites_missing   <- setdiff(all_sites, sites_with_data)
  
  # -- Sites without static data -- apply overrides or stop -------------------
  override_rows <- purrr::map_dfr(sites_missing, function(site) {
    
    has_height <- site %in% names(height_overrides)
    has_dist   <- site %in% names(dist_noise_overrides)
    
    if (!has_height) {
      stop(
        "derive_site_thresholds(): no static tests and no height_override ",
        "for site '", site, "'.\n",
        "Provide a manual override: height_overrides = list('", site,
        "' = <value>)\n",
        "Tip: local elevation (m a.s.l.) + ~150 m is a reasonable starting point."
      )
    }
    
    if (!has_dist) {
      warning(
        "derive_site_thresholds(): no static tests and no dist_noise_override ",
        "for site '", site, "'. ",
        "Using the Uganda Masaka empirical value as a conservative fallback ",
        "(dist_noise_m = 43 m). Collect static tests for '", site,
        "' to replace this."
      )
    }
    
    tibble::tibble(
      field_site       = site,
      dist_noise_m     = if (has_dist)
        as.double(dist_noise_overrides[[site]])
      else 43,
      height_threshold = as.double(height_overrides[[site]]),
      dist_source      = if (has_dist) "override" else "fallback_masaka",
      height_source    = "override"
    )
  })
  
  result <- dplyr::bind_rows(empirical, override_rows) |>
    dplyr::arrange(field_site)
  
  # Apply height_overrides to the final result -- overrides take precedence
  # over static-derived values for any site where they are explicitly provided.
  # This handles cases where static tests exist but are not representative
  # (e.g. conducted at a different elevation than the dog deployment area).
  if (length(height_overrides) > 0L) {
    result <- result |>
      dplyr::mutate(
        height_threshold = purrr::map_dbl(field_site, ~ {
          if (.x %in% names(height_overrides))
            as.double(height_overrides[[.x]])
          else
            height_threshold[field_site == .x]
        }),
        height_source = dplyr::if_else(
          field_site %in% names(height_overrides),
          "override",
          height_source
        )
      )
  }
  
  # -- Print summary ----------------------------------------------------------
  cat("\n--- Site thresholds ---\n")
  print(result, n = Inf)
  cat("\n")
  
  result
}
