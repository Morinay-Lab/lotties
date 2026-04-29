## Filename : clean_gps.R
## Origin : 2024-2025_Flock_following_cleaning.Rmd /
##          2025-2026_Flock_following_cleaning.Rmd
## Description : Cleans the GPS data which we only want to do once.

library(dplyr)
library(readxl)
library(stringr)
library(lubridate)
library(tidyr)
library(readr)
library(hms)

CURRENT_DIR <- getwd()
DATA_DIR <- paste0(CURRENT_DIR, "/data/")
XLS_DIR <- paste0(DATA_DIR, "xls/")
CSV_DIR <- paste0(DATA_DIR, "csv/")
GPX_DIR <- paste0(CSV_DIR, "gpx/")

## List for results
gps <- list()
##################################################################
## GPS data                                                     ##
##################################################################
## NB - Track_2025_07_09_SJB.csv is a different format to others so is removed and loaded separately needs
## cleaning/handling separately (likely handle by switching to using GPX files)
file_names <- paste0(GPX_DIR, dir(GPX_DIR))
track_2025_07_09 <- paste0(GPX_DIR, "Track_2025_07_09_SJB.csv")
file_names <- file_names[!stringr::str_detect(file_names, "Track_2025_07_09_SJB.csv")]
gps$gps_clean <- read_csv(file_names,
                      id = "name",
                      col_names = TRUE,
                      skip = 42,
                      col_select = c("name", "lat", "lon", "ele", "time"),
                      show_col_types = FALSE) |>
    dplyr::mutate(person = case_when(str_detect(name, pattern = "LN") == TRUE ~ "LN",
                              str_detect(name, pattern = "MJ") == TRUE ~ "MJ",
                              str_detect(name, pattern = "ND") == TRUE ~ "ND",
                              str_detect(name, pattern = "SB|SJB") == TRUE ~ "SB",
                              TRUE ~ NA),

           date_time = as.POSIXct(time, format = "%Y-%M-%d %h:%m:%s"),
           time = as.POSIXct(time, format = "%Y-%M-%d %h:%m:%s") |>
               format(., format = "%H:%M:%S"),
           date = as.Date(date_time),
           day_of_year = lubridate::yday(date)) |>
    dplyr::select(-name)
## Average data points
gps$gps_average <- gps$gps_clean |>
    dplyr::summarise()

##################################################################
## Additional GPS points                                        ##
##################################################################
## 2024-25
pos_SB_24_25 <- readxl::read_xlsx(paste0(XLS_DIR, "SARAH_Flocks_and_birds_seen_winter_2024-2025.xlsx"),
    sheet = "flock position",
    col_types = c("date", "numeric", "date","text", "text", "text")) |>
    dplyr::mutate(person="SB",
          lat_orig = lat,
          lat_orig = stringr::str_replace(lat_orig, "\\(", ""),
          lat_orig = stringr::str_replace(lat_orig, "\\)", ""),
          lat_and_lon = stringr::str_detect(lat_orig, ","),
          lat_lon_split = stringr::str_locate(lat_orig, ",")[,1],
          lat = dplyr::case_when(
                    lat_and_lon == TRUE ~ as.numeric(stringr::str_sub(lat_orig,
                                                                      1,
                                                                      stringr::str_locate(lat_orig,
                                                                                          ",")[,1] - 1)),
                    .default = as.numeric(lat)),
          lon = dplyr::case_when(
                     lat_and_lon == TRUE ~ as.numeric(stringr::str_sub(lat_orig,
                                                      stringr::str_locate(lat_orig,
                                                                          ",")[,1] + 1)),
                    .default = as.numeric(long))) |>
        dplyr::select(person, flockID, lat, lon)
pos_LN_24_25 <- readxl::read_xlsx(paste0(XLS_DIR, "LUKE_Flocks_and_birds_seen_winter_2024-2025.xlsx"),
    sheet = "flock position",
    col_types = c("date", "numeric", "date", "numeric", "numeric", "text")) |>
    dplyr::mutate(
        person = "LN",
        lat = as.numeric(lat),
        lon = as.numeric(long)) |>
    dplyr::select(flockID, person, lat, lon)
## 2025-26
## NB - Only LN has recorded additional "flock position" worksheet
worksheet = "flock position"
pos_LN_25_26 <- readxl::read_xlsx(paste0(XLS_DIR, "LUKE_Flocks_and_birds_seen_winter_2025-2026.xlsx"),
    sheet = worksheet
) |>
    dplyr::mutate(person = "LN") |>
    dplyr::filter(!is.na(lat)) |>
    dplyr::rename(lon = long) |>
    dplyr::select(flockID, person, lat, lon)

## Combine and clean
gps$additional_clean <- bind_rows(pos_LN_24_25, pos_LN_25_26, pos_SB_24_25) |>
    filter(!is.na(lat)) |>
    dplyr::mutate(IDD = paste(person, flockID, sep = "_")) |>
    dplyr::select(IDD, lat, lon) |>
    dplyr::group_by(IDD) |>
    dplyr::summarise(
        mean_lat = mean(lat),
        mean_lon = mean(lon),
        min_lat = min(lat),
        min_lon = min(lon),
        max_lat = max(lat),
        max_lon = max(lon))
rm(pos_LN_24_25, pos_LN_25_26, pos_SB_24_25)

## ns-rse 2026-04-29 - Can't combine raw GPS data and additional data points at this point because gps$gps_clean doesn't
## have flockID variable nor in turn IDD (paste(person, flockID, sep = "_"))

## Write to CSV
write.csv(gps$gps_clean,
    paste0(CSV_DIR, "gps_clean.csv"),
    row.names = FALSE)
write.csv(gps$gps_additional_clean,
          paste0(CSV_DIR, "additional_gps_clean.csv"),
          row.names = FALSE)
