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
GPX_DIR <- paste0(DATA_DIR, "gpx/")

## List for results
gps <- list()
##################################################################
## GPS data                                                     ##
##################################################################
## NB - Track_2025_07_09_SJB.csv is a different format to others so is removed and loaded separately needs
## cleaning/handling separately (likely handle by switching to using GPX files)
col_select <- c("name", "lat", "lon", "ele", "time")
GPX_AS_CSV_DIR <- paste0(CSV_DIR, "gps/")
file_names <- paste0(GPX_AS_CSV_DIR, dir(GPX_AS_CSV_DIR))
track_2025_07_09 <- paste0(GPX_AS_CSV_DIR, "Track_2025_07_09_SJB.csv")
file_names <- file_names[!stringr::str_detect(file_names, "Track_2025_07_09_SJB.csv")]
gps$gps_clean <- read_csv(file_names,
    id = "name",
    col_names = TRUE,
    skip = 42,
    col_select = col_select,
    show_col_types = FALSE
) |>
    dplyr::mutate(
        person = case_when(
            str_detect(name, pattern = "LN") == TRUE ~ "LN",
            str_detect(name, pattern = "MJ") == TRUE ~ "MJ",
            str_detect(name, pattern = "ND") == TRUE ~ "ND",
            str_detect(name, pattern = "SB|SJB") == TRUE ~ "SB",
            TRUE ~ NA
        ),
        date_time = as.POSIXct(time, format = "%Y-%M-%d %h:%m:%s"),
        time = as.POSIXct(time, format = "%Y-%M-%d %h:%m:%s") |>
            format(format = "%H:%M:%S"),
        date = as.Date(date_time),
        day_of_year = lubridate::yday(date),
        track = gsub(".*/", "", name)
    ) |>
    dplyr::select(-name) |>
    ## We now need to remove erroneous date/times across all files as the Garmin devices retain two data points from the
    ## last recording when starting a new track
    dplyr::group_by(track) |>
    dplyr::slice(3:n())

## Track_2025_07_09_SJB.csv
##
## This is a bit messy as it has two headers as though two files have been concatenated, to work round this we read the
## file twice, skipping the first header then the first header, the data that follows and the second header.
track_2025_07_09_sjb_block1 <- read_csv(track_2025_07_09,
    id = "name",
    col_names = TRUE,
    skip = 25,
    col_select = col_select,
    show_col_types = FALSE
) |>
    dplyr::slice(1:138) |>
    dplyr::mutate(
        lat = as.numeric(lat),
        person = case_when(
            str_detect(name, pattern = "LN") == TRUE ~ "LN",
            str_detect(name, pattern = "MJ") == TRUE ~ "MJ",
            str_detect(name, pattern = "ND") == TRUE ~ "ND",
            str_detect(name, pattern = "SB|SJB") == TRUE ~ "SB",
            TRUE ~ NA
        ),
        date_time =  lubridate::ymd_hms(time))
## ns-rse 2026-05-14 Having looked at the dates and lat/lon they are not contiguous so I think we can safely ignore
## these data points, they are not actual GPS traces, there are just a few points on each date.
track_2025_07_09_sjb_block1 |> dplyr::select(lat, lon, ele, date_time) |> head()
track_2025_07_09_sjb_block1 |>
    dplyr::select(lat, lon, ele, time, date_time) |>
    tail()

track_2025_07_09_sjb_block2 <- read_csv(track_2025_07_09,
    id = "name",
    col_names = TRUE,
    skip = 221,
    col_select = col_select,
    show_col_types = FALSE
) |>
    dplyr::mutate(
        person = case_when(
            str_detect(name, pattern = "LN") == TRUE ~ "LN",
            str_detect(name, pattern = "MJ") == TRUE ~ "MJ",
            str_detect(name, pattern = "ND") == TRUE ~ "ND",
            str_detect(name, pattern = "SB|SJB") == TRUE ~ "SB",
            TRUE ~ NA
        ),
        date_time =  as.POSIXct(time, format = "%Y-%M-%d %h:%m:%s"),
        time = as.POSIXct(time, format = "%Y-%M-%d %h:%m:%s") |>
            format(format = "%H:%M:%S"),
        date = as.Date(date_time),
        day_of_year = lubridate::yday(date),
        track = gsub(".*/", "", name)) |>
    dplyr::select(-name)
## This second block looks more like GPS data with seuential readings every minute. It contains tracks from 33 days. For
## now append to gps_clean. We de-duplicate to remove any possible duplicated data points (of which there were 330)
gps$gps_clean <- rbind(gps$gps_clean, track_2025_07_09_sjb_block2) |> unique()

## Average data points. This is done by track and date since tracks(/files) can have multiple days included
gps$gps_average <- gps$gps_clean |>
    dplyr::group_by(track, date) |>
    dplyr::summarise(
        mean_lat = mean(lat),
        mean_lon = mean(lon),
        mean_ele = mean(ele),
        mean_date_time = mean(date_time),
        mean_date = mean(date),
        min_lat = min(lat),
        min_lon = min(lon),
        min_ele = min(ele),
        min_date_time = min(date_time),
        min_date = min(date),
        max_lat = max(lat),
        max_lon = max(lon),
        max_ele = max(ele),
        max_date_time = max(date_time),
        max_date = max(date))

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
write.csv(gps$gps_average,
    paste0(CSV_DIR, "gps_average.csv"),
    row.names = FALSE)
write.csv(gps$gps_additional_clean,
          paste0(CSV_DIR, "additional_gps_clean.csv"),
          row.names = FALSE)
