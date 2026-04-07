# =============================================================================
# R/load_data.R
# Package : dogtrack
# Purpose : Scan a directory for dog GPS CSV files, apply session-A selection,
#           parse and normalise each file, then join with metadata.
#
# Exported functions:
#   load_dog_data() -- returns a single tidy tibble, one row per GPS fix,
#                     with metadata columns joined and field_site attached
# =============================================================================


#' Load and merge dog GPS CSV files with metadata
#'
#' Scans a directory recursively for Columbus P-10 Pro GPS CSV files,
#' excluding files whose name begins with `"static"`. Applies session-A
#' selection logic to handle dogs with multiple recording sessions, parses
#' datetime and coordinates, computes `delta_t_sec`, and joins the result
#' with the project metadata.
#'
#' @param data_dir Character. Path to the directory containing the dog GPS
#'   CSV files (static test files in the same folder are automatically
#'   excluded).
#' @param metadata A tibble as returned by [load_metadata()].
#'
#' @return A tibble with one row per GPS fix, containing all original device
#'   columns plus:
#'   - `file_key`     : character -- full filename without extension, unique
#'                      per CSV file (e.g. `"UMR001-01A"`)
#'   - `join_key`     : character -- base key used for metadata join
#'                      (e.g. `"UMR001-01"`, session suffix stripped)
#'   - `lat`          : numeric -- decimal degrees (S = negative)
#'   - `lon`          : numeric -- decimal degrees (W = negative)
#'   - `datetime`     : POSIXct -- parsed from DATE + TIME columns
#'   - `delta_t_sec`  : numeric -- seconds elapsed since previous fix
#'                      within the same dog track; `NA` for the first fix
#'   - `prefix_2`     : character -- 2-letter site prefix (e.g. `"UM"`)
#'   - `setting`      : character -- environment code extracted from filename
#'                      (e.g. `"R"` for rural, `"U"` for urban); `NA` if
#'                      absent
#'   - All metadata columns joined from [load_metadata()]
#'
#' @details
#' **Session selection:** some dogs have multiple CSV files for the same
#' deployment (suffixes `A`, `B`, `C` or `1`, `2` in the filename). When a
#' file with suffix `A` exists for a given base key, all other sessions
#' for that key are dropped. When no `A` suffix exists, the single
#' unsuffixed file is kept.
#'
#' **Unmatched files:** CSV files with no metadata match are excluded via
#' inner join and reported as a warning. This is a data quality flag --
#' investigate before proceeding.
#'
#' **Datetime parsing:** DATE and TIME may arrive as human-readable strings
#' (`"2026-01-07"`, `"09:47:09"`) or compact numerics (`260107`, `94709`).
#' Both are handled automatically.
#'
#' @examples
#' \dontrun{
#' meta     <- load_metadata("data/metadata.xlsx")
#' dog_data <- load_dog_data("data/", meta)
#' dplyr::count(dog_data, field_site)
#' }
#'
#' @export
load_dog_data <- function(data_dir, metadata) {
  
  if (!fs::dir_exists(data_dir)) {
    stop("load_dog_data(): directory not found: ", data_dir)
  }
  
  # -- Step 1 : scan for dog CSV files (exclude static tests) -----------------
  all_files  <- fs::dir_ls(data_dir, glob = "*.CSV", recurse = TRUE)
  dog_files  <- all_files[
    !stringr::str_detect(
      stringr::str_to_lower(basename(all_files)), "^static"
    )
  ]
  
  if (length(dog_files) == 0L) {
    stop(
      "load_dog_data(): no dog GPS CSV files found in '", data_dir, "'.\n",
      "Check that the path is correct and files have a .CSV extension."
    )
  }
  
  # -- Step 2 : parse filenames -> file_key, join_key, session -----------------
  file_df <- tibble::tibble(file_path = dog_files) |>
    dplyr::mutate(
      file_key = tools::file_path_sans_ext(basename(file_path)),
      
      # Normalise base join key -- three steps in order, mirroring script 00:
      # 1. strip letter session suffix (A/B/C), optionally preceded by , or _
      #    e.g. "UMR011-01A" -> "UMR011-01" | "UMU030-02,B" -> "UMU030-02"
      # 2. strip numeric session suffix (_1 / _2), preceded by _ only
      #    e.g. "UMR002_01_1" -> "UMR002_01"
      # 3. normalise remaining underscore separator to hyphen
      #    e.g. "UMU024_01" -> "UMU024-01"
      base_key = file_key |>
        stringr::str_remove("[,_]?[A-Za-z]$") |>
        stringr::str_remove("(_\\d)$")         |>
        stringr::str_replace("(_)(\\d{2})$", "-\\2"),
      
      # Session suffix for session-A selection logic
      session  = stringr::str_extract(
        file_key, "(?<=[,_]|(?<=[0-9]))[A-Z]$"),
      
      # Site prefix (2 letters) and setting (3rd letter: R/U)
      prefix_2 = stringr::str_extract(basename(file_path), "^[A-Z]{2}"),
      setting  = stringr::str_sub(
        stringr::str_extract(basename(file_path), "^[A-Z]{3}"),
        3L, 3L)
    ) |>
    dplyr::group_by(base_key) |>
    dplyr::mutate(
      has_session_A = any(session == "A", na.rm = TRUE),
      keep = dplyr::case_when(
        session == "A"                  ~ TRUE,   # explicit session A -> keep
        is.na(session) & !has_session_A ~ TRUE,   # no suffix, no A sibling -> keep
        TRUE                            ~ FALSE   # B/C/… or duplicate -> drop
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(keep)
  
  cat("load_dog_data(): found", nrow(file_df), "dog CSV file(s) after session selection.\n")
  
  # -- Step 3 : read and parse each CSV ---------------------------------------
  .read_one <- function(row) {
    df <- readr::read_csv(
      row$file_path,
      col_types = readr::cols(
        INDEX           = readr::col_guess(),
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
        file_key  = row$file_key,
        join_key  = row$base_key,
        prefix_2  = row$prefix_2,
        setting   = row$setting,
        date_str  = stringr::str_pad(as.character(DATE), 6, "left", "0"),
        time_str  = stringr::str_pad(as.character(TIME), 6, "left", "0"),
        datetime  = lubridate::parse_date_time(
          paste(date_str, time_str),
          orders = c("ymdHMS", "ymd HMS"),
          quiet  = TRUE)
      ) |>
      dplyr::select(-date_str, -time_str) |>
      dplyr::arrange(datetime) |>
      dplyr::mutate(
        delta_t_sec = as.numeric(
          difftime(datetime, dplyr::lag(datetime), units = "secs")
        )
      )
    
    df
  }
  
  dog_data_raw <- purrr::map_dfr(
    purrr::transpose(file_df),
    .read_one
  )
  
  cat("load_dog_data(): loaded", nrow(dog_data_raw), "GPS fixes from",
      dplyr::n_distinct(dog_data_raw$file_key), "dogs.\n")
  
  # -- Step 4 : inner join with metadata --------------------------------------
  # Inner join: only dogs present in BOTH the CSV folder AND the metadata
  # are retained. Unmatched CSVs are reported as a warning.
  unmatched_csv <- setdiff(
    unique(dog_data_raw$join_key),
    unique(metadata$join_key)
  )
  
  if (length(unmatched_csv) > 0L) {
    warning(
      "load_dog_data(): ", length(unmatched_csv),
      " CSV file(s) have no metadata match and are EXCLUDED:\n",
      paste(sort(unmatched_csv), collapse = "\n"),
      "\nVerify filename conventions and metadata completeness."
    )
  }
  
  dog_data <- dog_data_raw |>
    dplyr::inner_join(metadata, by = "join_key")
  
  # Overwrite prefix_2 and setting with the values parsed from the filename
  # (the metadata join may introduce a duplicate prefix_2 from metadata cols)
  dog_data <- dog_data |>
    dplyr::mutate(
      prefix_2 = stringr::str_extract(file_key, "^[A-Z]{2}"),
      setting  = stringr::str_extract(file_key, "^[A-Z]{3}") |>
        stringr::str_sub(3, 3)
    )
  
  cat("load_dog_data(): after metadata join --",
      dplyr::n_distinct(dog_data$file_key), "dogs,",
      nrow(dog_data), "GPS fixes retained.\n")
  cat("load_dog_data(): CSV files with no metadata match (excluded):",
      length(unmatched_csv), "\n")
  
  dog_data
}
