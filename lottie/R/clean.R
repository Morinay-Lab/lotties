## Filename : clean.R
## Origin : N/A
## Description : Master file for cleaning all data and populating SQlite database

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

message("Cleaning GPS data...")
source("R/clean_gps.R")
message("Cleaning Possible rings data...")
source("R/clean_rings.R")
message("Cleaning 2024/25 observations...")
source("R/clean_2024_25.R")
message("Cleaning 2025/26 observations...")
source("R/clean_2025_26.R")
## message("Populating SQLite database...")
## source("R/populate_db.R")
