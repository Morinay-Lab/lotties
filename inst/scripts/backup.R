## Filename    : backup.R
## Description : A short script that extracts all tables from a database then zips them up for archiving/snapshots.
##               Parameters are defined in the .Renviron file and loaded via Sys.getenv() should they need modifying.
library(DBI)
library(RSQLite)

## Load the lotties package
devtools::load_all(path=Sys.getenv("LOTTIES_PKG_DIR"))
## Change directory to inst/shiny so the LOTTIES_DB_PATH relative path works
setwd("inst/shiny")

## Set the database connection up
con <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("LOTTIES_DB_PATH"))
## Extract all tables
csv_files <- extract_all_tables(con=con,
                                path=Sys.getenv("LOTTIES_EXTRACT_DIR"))
## Compress all extracted files to the backup directory
compress_files(
    csv_files = csv_files,
    path = Sys.getenv("LOTTIES_BACKUP_DIR"),
    extract_dir= Sys.getenv("LOTTIES_EXTRACT_DIR"))
