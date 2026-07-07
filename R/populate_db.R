## Filename : populate_db.R
## Origin : N/A
## Description : Combines data from different seasons and populates SQLite database

library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(reshape)

## Combine data from years and perform additional cleaning
##
## - Make all date/time into date_time formatted as strings (SQLite doesn't have native date/time formats and stores as strings)
meta_clean <- rbind(data_2024_25$meta_clean, data_2025_26$meta_clean) |>
    dplyr::mutate(date_time_seen = lubridate::ymd_hms(paste0(date, " ", time_seen)),
                  date_time_lost = lubridate::ymd_hms(paste0(date, " ", time_lost)) ,
                  date_time_seen = format(date_time_seen, "%Y-%m-%d %H:%M:%S"),
                  date_time_lost = format(date_time_lost, "%Y-%m-%d %H:%M:%S")) |>
    dplyr::select(-date, -time_seen, -time_lost)
observations_clean <- rbind(data_2024_25$observations_clean, data_2025_26$observations_clean) |>
    dplyr::mutate(date_time_start = lubridate::ymd_hms(paste0(Date, " ", Time.start)),
                  date_time_end = lubridate::ymd_hms(paste0(Date, " ", Time.end)),
                  date_time_start = format(date_time_start, "%Y-%m-%d %H:%M:%S"),
                  date_time_end = format(date_time_end, "%Y-%m-%d %H:%M:%S")) |>
    dplyr::select(-Date, -Time.start, -Time.end)
flock_clean <- rbind(data_2024_25$flock_clean, data_2025_26$flock_clean) |>
    dplyr::mutate(date_time = lubridate::ymd_hms(paste0(date, " ", time)),
                  date_time = format(date_time, "%Y-%m-%d %H:%M:%S")) |>
    dplyr::select(-date, -time)
gps_clean <- gps$gps_clean |>
    dplyr::mutate(
        date_time = format(date_time, "%Y-%m-%d %H:%M:%S"),
        date = format(date, "%Y-%m-%d %H:%M:%S")
    )
locs <- rbind(data_2024_25$locs, data_2025_26$locs)
meta_clean_gps <- rbind(data_2024_25$meta_clean_gps, data_2025_26$meta_clean_gps) |>
    dplyr::mutate(
        date_time_seen = lubridate::ymd_hms(paste0(date, " ", time_seen)),
        date_time_lost = lubridate::ymd_hms(paste0(date, " ", time_lost)),
        date_time_seen = format(date_time_seen, "%Y-%m-%d %H:%M:%S"),
        date_time_lost = format(date_time_lost, "%Y-%m-%d %H:%M:%S")
    ) |>
    dplyr::select(-date, -time_seen, -time_lost)
all_flock_data <- rbind(data_2024_25$all_flock_data, data_2025_26$all_flock_data) |>
    dplyr::mutate(
        date_time_seen = lubridate::ymd_hms(paste0(date, " ", time_seen)),
        date_time_lost = lubridate::ymd_hms(paste0(date, " ", time_lost)),
        date_time_seen = format(date_time_seen, "%Y-%m-%d %H:%M:%S"),
        date_time_lost = format(date_time_lost, "%Y-%m-%d %H:%M:%S")
    ) |>
    dplyr::select(-date, -time_seen, -time_lost)


## Set whether to create db in memory or on disk
testing <- TRUE
if (testing) {
    db_path = ":memory:"
} else {
    db_path = "data/sqlite/lottie.sql"
}
## Set whether to overwrite existing data
overwrite <- TRUE

## Setup connection to SQLite database
con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

## Add data
##
## Meta (clean)
RSQLite::dbWriteTable(
    conn = con,
    name = "Meta",
    meta_clean,
    overwrite = overwrite
)
## Observations
RSQLite::dbWriteTable(
    conn = con,
    name = "Observations",
    observations_clean,
    overwrite = overwrite
)
## Flock
RSQLite::dbWriteTable(
    conn = con,
    name = "Flock",
    flock_clean,
    overwrite = overwrite
)
## Location
RSQLite::dbWriteTable(
    conn = con,
    name = "Locs",
    locs,
    overwrite = overwrite
)
## MetaGPS
RSQLite::dbWriteTable(
    conn = con,
    name = "MetaGPS",
    meta_clean_gps,
    overwrite = overwrite
)
## All Flock
RSQLite::dbWriteTable(
    conn = con,
    name = "AllFlock",
    all_flock_data,
    overwrite = overwrite
)
## GPS
RSQLite::dbWriteTable(
             conn = con,
             name = "GPS",
             gps_clean,
             overwrite = overwrite)
## GPS
RSQLite::dbWriteTable(
             conn = con,
             name = "GPSAdditional",
             gps$additional_clean,
             overwrite = overwrite)


## Create and add lookup tables. the dataframes themselves are defined in lookups.R so they can be used elsewhere, hence
## the need to source()
##
source("R/lookups.R")
## People
RSQLite::dbWriteTable(
             conn = con,
             name = "Person",
             person_df,
             overwrite = overwrite)

## Other Species
RSQLite::dbWriteTable(
             conn = con,
             name = "OtherSpecies",
             other_species_df,
             overwrite = overwrite)

## Rings
RSQLite::dbWriteTable(
             conn = con,
             name = "Rings",
             rings_df,
             overwrite = overwrite)

## Sites
RSQLite::dbWriteTable(
             conn = con,
             name = "Site",
             site_df,
             overwrite = overwrite)

## Extract information from the database (mainly as a reference for @ns-rse)!
summary_stats <- list()
## Count observations overall and by year
## RSQLite::dbGetQuery() returns a dataframe (with a single row and column) so we pull out the number using indexing
## (which starts a 1 in R)
summary_stats$meta <- list()
summary_stats$meta$all <- list()
summary_stats$meta$year_2024 <- list()
summary_stats$meta$year_2025 <- list()
summary_stats$meta$year_2026 <- list()
query <- "SELECT COUNT(*) FROM Meta"
summary_stats$meta$all <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM Meta WHERE strftime('%Y', date_time_seen) == '2024'"
summary_stats$meta$year_2024 <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM Meta WHERE strftime('%Y', date_time_seen) == '2025'"
summary_stats$meta$year_2025 <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM Meta WHERE strftime('%Y', date_time_seen) == '2026'"
summary_stats$meta$year_2026 <- RSQLite::dbGetQuery(con, query)[1,1]
summary_stats$meta_gps <- list()
summary_stats$meta_gps$all <- list()
summary_stats$meta_gps$year_2024 <- list()
summary_stats$meta_gps$year_2025 <- list()
summary_stats$meta_gps$year_2026 <- list()
query <- "SELECT COUNT(*) FROM MetaGPS"
summary_stats$meta_gps$all <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM MetaGPS WHERE strftime('%Y', date_time_seen) == '2024'"
summary_stats$meta_gps$year_2024 <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM MetaGPS WHERE strftime('%Y', date_time_seen) == '2025'"
summary_stats$meta_gps$year_2025 <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM MetaGPS WHERE strftime('%Y', date_time_seen) == '2026'"
summary_stats$meta_gps$year_2026 <- RSQLite::dbGetQuery(con, query)[1,1]
summary_stats$observations <- list()
summary_stats$observations$all <- list()
summary_stats$observations$year_2024 <- list()
summary_stats$observations$year_2025 <- list()
summary_stats$observations$year_2026 <- list()
query <- "SELECT COUNT(*) FROM Observations"
summary_stats$observations$all <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM Observations WHERE strftime('%Y', date_time_start) == '2024'"
summary_stats$observations$year_2024 <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM Observations WHERE strftime('%Y', date_time_start) == '2025'"
summary_stats$observations$year_2025 <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM Observations WHERE strftime('%Y', date_time_start) == '2026'"
summary_stats$observations$year_2026 <- RSQLite::dbGetQuery(con, query)[1,1]
summary_stats$flock <- list()
summary_stats$flock$all <- list()
summary_stats$flock$year_2024 <- list()
summary_stats$flock$year_2025 <- list()
summary_stats$flock$year_2026 <- list()
query <- "SELECT COUNT(*) FROM Flock"
summary_stats$flock$all <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM Flock WHERE strftime('%Y', date_time) == '2024'"
summary_stats$flock$year_2024 <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM Flock WHERE strftime('%Y', date_time) == '2025'"
summary_stats$flock$year_2025 <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM Flock WHERE strftime('%Y', date_time) == '2026'"
summary_stats$flock$year_2026 <- RSQLite::dbGetQuery(con, query)[1,1]
summary_stats$all_flock <- list()
summary_stats$all_flock$all <- list()
summary_stats$all_flock$year_2024 <- list()
summary_stats$all_flock$year_2025 <- list()
summary_stats$all_flock$year_2026 <- list()
query <- "SELECT COUNT(*) FROM AllFlock"
summary_stats$all_flock$all <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM AllFlock WHERE strftime('%Y', date_time_seen) == '2024'"
summary_stats$all_flock$year_2024 <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM AllFlock WHERE strftime('%Y', date_time_seen) == '2025'"
summary_stats$all_flock$year_2025 <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM AllFlock WHERE strftime('%Y', date_time_seen) == '2026'"
summary_stats$all_flock$year_2026 <- RSQLite::dbGetQuery(con, query)[1,1]
summary_stats$gps <- list()
summary_stats$gps$all <- list()
summary_stats$gps$year_2024 <- list()
summary_stats$gps$year_2025 <- list()
summary_stats$gps$year_2026 <- list()
query <- "SELECT COUNT(*) FROM GPS"
summary_stats$gps$all <- RSQLite::dbGetQuery(con, query)
query <- "SELECT COUNT(*) FROM GPS WHERE strftime('%Y', date_time) == '2024'"
summary_stats$gps$year_2024 <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM GPS WHERE strftime('%Y', date_time) == '2025'"
summary_stats$gps$year_2025 <- RSQLite::dbGetQuery(con, query)[1,1]
query <- "SELECT COUNT(*) FROM GPS WHERE strftime('%Y', date_time) == '2026'"
summary_stats$gps$year_2026 <- RSQLite::dbGetQuery(con, query)[1,1]
summary_stats$person <- list()
summary_stats$person$all <- list()
query <- "SELECT COUNT(*) FROM Person"
summary_stats$person$all <- RSQLite::dbGetQuery(con, query)
summary_stats$other_species <- list()
summary_stats$other_species$all <- list()
query <- "SELECT COUNT(*) FROM OtherSpecies"
summary_stats$other_species$all <- RSQLite::dbGetQuery(con, query)
summary_stats$rings <- list()
summary_stats$rings$all <- list()
query <- "SELECT COUNT(*) FROM Rings"
summary_stats$rings$all <- RSQLite::dbGetQuery(con, query)
summary_stats$locs <- list()
summary_stats$locs$all <- list()
query <- "SELECT COUNT(*) FROM Locs"
summary_stats$locs$all <- RSQLite::dbGetQuery(con, query)


## Build dataframe of summary counts
summary_df <- data.frame(
    period = c("All", "2024", "2025", "2026"),
    meta = unlist(c(summary_stats$meta$all,
                    summary_stats$meta$year_2024,
                    summary_stats$meta$year_2025,
                    summary_stats$meta$year_2026)),
    flock = unlist(c(summary_stats$flock$all,
                     summary_stats$flock$year_2024,
                     summary_stats$flock$year_2025,
                     summary_stats$flock$year_2026)),
    observations = unlist(c(summary_stats$observations$all,
                     summary_stats$observations$year_2024,
                     summary_stats$observations$year_2025,
                     summary_stats$observations$year_2026)),
    gps = unlist(c(summary_stats$gps$all,
                   summary_stats$gps$year_2024,
                   summary_stats$gps$year_2025,
                   summary_stats$gps$year_2026)),
    meta_gps = unlist(c(summary_stats$meta_gps$all,
                     summary_stats$meta_gps$year_2024,
                     summary_stats$meta_gps$year_2025,
                     summary_stats$meta_gps$year_2026)),
    all_flock = unlist(c(summary_stats$all_flock$all,
                     summary_stats$all_flock$year_2024,
                     summary_stats$all_flock$year_2025,
                     summary_stats$all_flock$year_2026)),
    locs = unlist(c(summary_stats$locs$all,
                      NA,
                      NA,
                      NA)),
    person = unlist(c(summary_stats$person$all,
                      NA,
                      NA,
                      NA)),
    other_species = unlist(c(summary_stats$other_species$all,
                      NA,
                      NA,
                      NA)),
    rings = unlist(c(summary_stats$rings$all,
                      NA,
                      NA,
                      NA))
) |>
    dplyr::as_tibble()
summary_df

## Close database connection
DBI::dbDisconnect(con)
