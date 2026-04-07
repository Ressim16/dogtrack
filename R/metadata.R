# =============================================================================
# R/metadata.R
# Package : dogtrack
# Purpose : Read all sheets from the project metadata workbook and build
#           the prefix-to-site routing table used by the pipeline.
#
# Exported functions:
#   load_metadata()    -- reads all Excel sheets, normalises columns, builds join_key
#   build_prefix_map() -- derives prefix_2 -> field_site lookup, validates uniqueness
# =============================================================================


# -----------------------------------------------------------------------------
# Internal helper -- NOT exported, not visible to users
# Scans the first `max_rows` rows of a sheet to find the row that contains
# the actual column headers (identified by the presence of "Household ID").
# Returns the 1-based row number, which is passed to read_excel(skip = ).
# This handles Excel files that have one or more title rows above the headers.
# -----------------------------------------------------------------------------
.find_header_row <- function(metadata_path, sheet,
                             target   = "Household ID|Maison ID",
                             max_rows = 20L) {
  
  raw <- readxl::read_excel(
    metadata_path, sheet     = sheet,
    col_names = FALSE, n_max = max_rows,
    col_types = "text"
  )
  
  hit <- which(apply(raw, 1, function(row) {
    any(stringr::str_detect(
      tidyr::replace_na(row, ""),
      stringr::regex(target, ignore_case = TRUE)
    ))
  }))
  
  if (length(hit) == 0L) {
    stop(
      "load_metadata(): cannot locate a row containing '", target,
      "' in sheet '", sheet, "' (searched first ", max_rows, " rows).\n",
      "Verify that the column header exists and is spelled correctly."
    )
  }
  
  hit[[1L]]
}

# Internal helper -- NOT exported
# Harmonises known column name variants (e.g. French) to the standard
# English names expected by the rest of the pipeline.
# Only renames a variant if the standard name is NOT already present.
.harmonise_columns <- function(df) {
  
  variants <- list(
    household_id = c("maison_id"),
    field_site   = c("ville"),
    dog_id       = c("nr_chien", "no_chien", "dog_number")
  )
  
  for (standard in names(variants)) {
    if (!standard %in% names(df)) {
      found <- intersect(variants[[standard]], names(df))
      if (length(found) > 0L) {
        df <- dplyr::rename(df, !!standard := !!found[[1L]])
      }
    }
  }
  
  # Normalise known field_site spelling variants to their canonical form.
  # Add new entries here whenever a new site variant is encountered.
  site_canonical <- c(
    "n'djamena" = "N'Djamena",
    "n djamena" = "N'Djamena",
    "ndjamena"  = "N'Djamena",
    "masaka"    = "Masaka",
    "arua"      = "Arua",
    "soroti"    = "Soroti"
  )
  
  if ("field_site" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        field_site = {
          key   <- stringr::str_to_lower(stringr::str_trim(field_site))
          canon <- site_canonical[key]
          # Keep original value where no canonical form is defined
          dplyr::if_else(!is.na(canon), canon, field_site)
        }
      )
  }
  
  df
}


# -----------------------------------------------------------------------------
#' Load and harmonise metadata from a multi-sheet Excel workbook
#'
#' Reads every sheet from the project metadata workbook. Each sheet represents
#' one field site and must follow the convention `{Country}_Location_ID`
#' (e.g. `Uganda_Location_ID`). Column names are normalised to `snake_case`
#' immediately after reading, so minor spelling differences across sheets are
#' handled automatically.
#'
#' @param metadata_path Character. Path to the metadata `.xlsx` file.
#'
#' @return A tibble with one row per dog deployment across all sites, containing
#'   at minimum:
#'   - `field_site`  : character -- value from the `Field site` column
#'   - `household_id`: character
#'   - `dog_id`      : character
#'   - `join_key`    : character -- `"{household_id}-{zero-padded dog_id}"`,
#'                     used to match CSV filenames to metadata rows
#'   - `prefix_2`    : character -- first two uppercase letters of `household_id`
#'                     (e.g. `"UM"` for Uganda Masaka), used for file routing
#'   - `sheet_name`  : character -- source Excel sheet, kept for traceability
#'
#' @details
#' Rows missing `household_id`, `dog_id`, or `field_site` are silently dropped
#' (these correspond to empty rows from Excel export artefacts). A warning is
#' issued if any rows are dropped.
#'
#' An informative error is raised if a sheet is missing any of the three
#' required columns, listing what was found instead.
#'
#' The function handles Excel workbooks that contain one or more title rows
#' above the actual column headers -- the header row is detected automatically
#' by locating the row containing `"Household ID"`.
#'
#' @examples
#' \dontrun{
#' meta <- load_metadata("data/metadata.xlsx")
#' dplyr::count(meta, field_site)
#' }
#'
#' @export
load_metadata <- function(metadata_path) {
  
  if (!fs::file_exists(metadata_path)) {
    stop("load_metadata(): file not found: ", metadata_path)
  }
  
  sheets <- readxl::excel_sheets(metadata_path)
  
  if (length(sheets) == 0) {
    stop("load_metadata(): the workbook at '", metadata_path, "' has no sheets.")
  }
  
  # Only process sheets following the convention "{Country}_Location_ID"
  # Other sheets (e.g. Collar_Coverage, Notes) are silently ignored.
  location_sheets <- sheets[stringr::str_detect(sheets, "_Location_ID$")]
  
  if (length(location_sheets) == 0L) {
    stop(
      "load_metadata(): no sheets matching '*_Location_ID' found in '",
      metadata_path, "'.\n",
      "Sheets present: ", paste(sheets, collapse = ", ")
    )
  }
  
  cat("load_metadata(): processing", length(location_sheets), "location sheet(s):",
      paste(location_sheets, collapse = ", "), "\n")
  
  purrr::map_dfr(location_sheets, function(sheet) {
    
    # -- Step 1 : detect the real header row ----------------------------------
    # Some Excel files have one or more title rows above the column headers.
    # .find_header_row() scans the first 20 rows for "Household ID" and
    # returns its 1-based row index.
    header_row <- .find_header_row(metadata_path, sheet)
    
    # -- Step 2 : read the sheet, skipping rows above the headers -------------
    df <- suppressMessages(
      readxl::read_excel(
        metadata_path, sheet = sheet,
        skip = header_row - 1L,      # skip = 0 when headers are on row 1
        col_types = "text"  
      )
    ) |>
      # Drop columns that are entirely NA (Excel export artefacts)
      dplyr::select(dplyr::where(~ !all(is.na(.)))) |>
      # Normalise column names to snake_case
      dplyr::rename_with(
        ~ .x |>
          stringr::str_to_lower() |>
          stringr::str_replace_all("[^a-z0-9]", "_") |>
          stringr::str_replace_all("_+", "_") |>
          stringr::str_remove("^_|_$")
      ) |>
      .harmonise_columns()
    
    # -- Step 3 : verify required columns are present -------------------------
    required <- c("household_id", "dog_id", "field_site")
    missing  <- setdiff(required, names(df))
    
    if (length(missing) > 0) {
      stop(
        "load_metadata(): sheet '", sheet, "' is missing column(s): ",
        paste(missing, collapse = ", "), ".\n",
        "Columns found: ", paste(names(df), collapse = ", "), ".\n",
        "Check that the column names match the expected convention ",
        "('Household ID', 'Dog ID', 'Field site')."
      )
    }
    
    n_raw <- nrow(df)
    
    # -- Step 4 : clean, derive join_key and prefix_2 -------------------------
    df <- df |>
      dplyr::filter(
        !is.na(household_id),
        !is.na(dog_id),
        !is.na(field_site)
      ) |>
      dplyr::mutate(
        household_id = stringr::str_trim(as.character(household_id)),
        dog_id       = stringr::str_trim(as.character(dog_id)),
        field_site   = stringr::str_trim(as.character(field_site)),
        sheet_name   = sheet,
        
        # join_key: if dog_id already contains "-" it is a full GPS ID
        # (e.g. "TNU001-01" in the Tchad sheet) -- use it directly.
        # Otherwise construct it from household_id + zero-padded dog number.
        join_key = dplyr::if_else(
          stringr::str_detect(dog_id, "-"),
          dog_id,
          paste0(
            household_id, "-",
            stringr::str_pad(
              stringr::str_remove(dog_id, "\\.0$"),
              width = 2, side = "left", pad = "0"
            )
          )
        ),
        
        # prefix_2: first 2 uppercase letters = country code + city code
        # e.g. "UMR001" -> "UM" | "TNA001" -> "TN"
        prefix_2 = stringr::str_extract(household_id, "^[A-Z]{2}")
      )
    
    n_dropped <- n_raw - nrow(df)
    if (n_dropped > 0) {
      warning(
        "load_metadata(): sheet '", sheet, "': dropped ", n_dropped,
        " row(s) with missing household_id, dog_id, or field_site."
      )
    }
    
    df
  })
}


# -----------------------------------------------------------------------------
#' Build a prefix-to-site routing table from loaded metadata
#'
#' Derives a lookup table mapping each 2-character filename prefix (e.g. `"UM"`,
#' `"TN"`) to its `field_site` (e.g. `"Masaka"`, `"N'Djamena"`). This table
#' is used internally by the pipeline to route GPS CSV files to their correct
#' site-specific thresholds without any hardcoding.
#'
#' @param metadata A tibble as returned by [load_metadata()], containing at
#'   minimum the columns `prefix_2` and `field_site`.
#'
#' @return A tibble with columns `prefix_2` and `field_site`. One row per
#'   unique prefix.
#'
#' @details
#' Stops with an informative error if any 2-character prefix maps to more than
#' one `field_site` -- this would make routing ambiguous and indicates that two
#' sites share a country + city code, which should not happen.
#'
#' @examples
#' \dontrun{
#' meta    <- load_metadata("data/metadata.xlsx")
#' pfx_map <- build_prefix_map(meta)
#' # prefix_2   field_site
#' # "UM"        "Masaka"
#' # "UA"        "Arua"
#' # "TN"        "N'Djamena"
#' }
#'
#' @export
build_prefix_map <- function(metadata) {
  
  map <- metadata |>
    dplyr::distinct(prefix_2, field_site) |>
    dplyr::filter(!is.na(prefix_2))
  
  # Guard: a prefix must map to exactly one site
  ambiguous <- map |>
    dplyr::count(prefix_2) |>
    dplyr::filter(n > 1L) |>
    dplyr::pull(prefix_2)
  
  if (length(ambiguous) > 0) {
    stop(
      "build_prefix_map(): the following prefix(es) map to more than one ",
      "field_site -- routing is ambiguous:\n",
      paste(ambiguous, collapse = ", "), "\n",
      "Ensure that each site has a unique country + city code combination."
    )
  }
  
  map
}
