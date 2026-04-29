## Filename : clean_2024_25.R
## Origin : 2024-2025_Flock_following_cleaning.Rmd /
##          2025-2026_Flock_following_cleaning.Rmd
## Description : Cleans the ring data


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

##################################################################
## Rings                                                        ##
##################################################################
possible_colour_rings <- read_xlsx(paste0(XLS_DIR, "Most recent colour rings and date.xlsx")) |>
    dplyr::mutate(ring_date = as.Date(sys_LastColourRing.RingDate, format="%Y-%m-%d %H:%M:%s"),
           ring_year= lubridate::year(ring_date)) |>
    dplyr::rename(colour_ring=LastColourRing) |>
    dplyr::select(colour_ring, ring_date, ring_year) |>
    dplyr::group_by(colour_ring) |>
    dplyr::slice(1)

write.csv(possible_colour_rings, paste0(CSV_DIR, "possible_colour_rings.csv"), row.names = FALSE)
