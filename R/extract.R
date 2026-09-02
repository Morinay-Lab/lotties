#' Extract tables from a database and write to '.csv'.
#'
#' @param table str Table to extract data from. All columns will be exported.
#' @param con Database connection.
#' @param path Path to export to.
#'
#' @returns N/A nothing is returned, table is written to `/path/table.csv`.
#' @export
extract_table_to_csv <- function(table, con, path) {
  query <- paste0("SELECT * FROM ", table)
  df <- RSQLite::dbGetQuery(con, query)
  file_name <- paste0(path, tolower(table), ".csv")
  write.csv(df, file_name, row.names = FALSE)
  return(file_name)
}

#' Extract all specified tables.
#'
#' @param con Database connection.
#' @param tables list List of tables as strings to be extracted.
#' @param path Path to export files to.
#'
#' @returns A list of extracted/exported files.
#' @export
extract_all_tables <- function(con,
                               path,
                               tables = c("Conditions",
                                          "Composition",
                                          "Description",
                                          "Interactions",
                                          "GPS")) {
  csv_files <- list()
  for (table in tables) {
    file_name <- extract_table_to_csv(table, con, path)
    csv_files[[tolower(table)]] <- file_name
  }
  return(csv_files)
}


#' Compress a list of files.
#'
#' A list of CSV files are compressed to the specified 'path' in a file named 'extract_YYYYMMDD_HHmmss.zzzzz.zip'.
#'
#' @param csv_files list A named list of files to be compressed.
#' @param path str The location the files should be compressed to.
#'
#' @returns Nothing
#' @export
compress_files <- function(csv_files, path) {
  zip::zip(zipfile = paste0(path,
                            "/extract_",
                            lubridate::now() |>
                              stringr::str_replace_all(" ", "_")|>
                              stringr::str_replace_all(":", ""),
                            ".zip"),
           files = unlist(csv_files))
}
