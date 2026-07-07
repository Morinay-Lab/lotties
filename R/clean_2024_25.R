## Filename : clean_2024_25.R
## Origin : 2024-2025_Flock_following_cleaning.Rmd
## Description : Cleans the flock following and GPS data

library(dplyr)
library(readxl)
library(stringr)
library(lubridate)
library(tidyr)
library(readr)
library(hms)

CURRENT_DIR <- getwd()
DATA_DIR <- paste0(CURRENT_DIR, "/inst/")
XLS_DIR <- paste0(DATA_DIR, "xls/")
CSV_DIR <- paste0(DATA_DIR, "csv/")
GPX_DIR <- paste0(DATA_DIR, "gpx/")

## Setup a list to hold 2024/25 data
data_2024_25 <- list()
##################################################################
## Metadata                                                     ##
##################################################################
worksheet <- "flock description"
meta_sb <- read_xlsx(paste0(XLS_DIR,
                            "SARAH_Flocks_and_birds_seen_winter_2024-2025.xlsx"),
                     sheet = worksheet) |>
  dplyr::mutate(person="SB")
meta_ln <- read_xlsx(paste0(XLS_DIR,
                            "LUKE_Flocks_and_birds_seen_winter_2024-2025.xlsx"),
                     sheet = worksheet) |>
  dplyr::mutate(person="LN")
data_2024_25$meta_clean <- bind_rows(meta_ln, meta_sb) |>
  dplyr::mutate(
    ## minimum nb of birds seen in the flock
    min_nb = stringr::str_extract_all(`Nb of birds in the flock`, "[0-9]+") |>
      purrr::map(~ifelse(length(.x) == 1,
                         as.numeric(.x),
                         min(unlist(as.numeric(.x))))) |> unlist(),
    ## deleting question marks
    nb_unringed = gsub("\\?.*", NA, `Nb of unringed`),
    # min nb of unidentified birds in the flock
    min_nb_unindent = stringr::str_extract_all(nb_unringed, "[0-9]+") |>
        purrr::map(~ifelse(length(.x)>0,
                    ifelse(length(.x) == 1,
                           as.numeric(.x),
                           min(unlist(as.numeric(.x)))),
                    NA)) |> unlist() ,
    min_nb_unindent = dplyr::case_when(min_nb_unindent == "NA" ~ NA,
                                TRUE ~ min_nb_unindent),
    ## Date  & Times in good format
    date = as.Date(Date, format = "%Y-%m-%d"),
    time_seen = as.POSIXct(`Time first seen`,
                           format = "%Y-%M-%d %H:%M:%S") |>
                format(format = "%H:%M:%S"),
    time_lost = as.POSIXct(`Time lost/left`,
                           format = "%Y-%M-%d %H:%M:%S")|>
                format(format = "%H:%M:%S"),
    ## Correcting a missing times
    time_seen = dplyr::case_when(is.na(time_seen) ~ "12:54:00",
                          TRUE ~ time_seen),
    time_lost = dplyr::case_when(is.na(time_lost) ~ "13:14:00",
                          TRUE ~ time_lost),
    ## ToDo - Add date/time for seen/last
    ## date_time_seen =
    ## date_time_last =
    ## Presence of other species:
    BT = dplyr::case_when(str_detect(`Other species`,
                              pattern="BLUTI | Bluti|bluti|Blue tit|blue tit|Blue Tit") == TRUE ~ 1,
                   TRUE ~ NA),
    GT = dplyr::case_when(str_detect(`Other species`,
                              pattern="Great Tit|great tit|greti|Greti") == TRUE ~ 1,
                   TRUE ~ NA),
    CT = dplyr::case_when(str_detect(`Other species`,
                              pattern="coati|coat tit|Coal Tit") == TRUE ~ 1,
                   TRUE ~ NA),
    GC = dplyr::case_when(str_detect(`Other species`,
                              pattern="Goldcrest|goldcrest|goldc|GOLDC|Goldc") == TRUE ~ 1,
                   TRUE ~ NA),
    RB = dplyr::case_when(str_detect(`Other species`,
                              pattern="robin") == TRUE ~ 1,
                   TRUE ~ NA),
    NH = dplyr::case_when(str_detect(`Other species`,
                              pattern="Nuthatch|Nutha") == TRUE ~ 1,
                   TRUE ~ NA),
    TC = dplyr::case_when(str_detect(`Other species`,
                              pattern="Treecreeper|treecreeper|tree creeper|treec|Treec|TREEC") == TRUE ~ 1,
                   TRUE ~ NA),
    WP = dplyr::case_when(str_detect(`Other species`,
                              pattern="Great Spotted Woodpecker") == TRUE ~ 1,
                   TRUE ~ NA),
    no_other_species = dplyr::case_when(str_detect(`Other species`,
                                            pattern = "NA|none") == TRUE ~ 0,
                                 TRUE ~ NA),
    ## Creating unique ID
    IDD = paste(person, flockID, sep="_"),
    ## Day of Year
    day_of_year = lubridate::yday(date)
  ) |>
    dplyr::rename(in_flock = `in flock`,
           flock_id = flockID,
           section = Section,
           whole_flock_id = Whole_flock_ID) |>
    ## Selecting only some columns
    dplyr::select(date, day_of_year, IDD, flock_id, person, section, time_seen, time_lost,
           in_flock, whole_flock_id, min_nb, min_nb_unindent,
           BT, GT, CT, GC, RB, NH, TC, WP, no_other_species, notes)
rm(meta_ln, meta_sb)

##################################################################
## Observation data                                             ##
##################################################################
worksheet <- "times out looking for them"
observations_SB <- read_xlsx(paste0(XLS_DIR,
                                    "SARAH_Flocks_and_birds_seen_winter_2024-2025.xlsx"),
    sheet = worksheet) |>
    dplyr::mutate(person="SB")
observations_LN <- read_xlsx(paste0(XLS_DIR,
                                    "LUKE_Flocks_and_birds_seen_winter_2024-2025.xlsx"),
    sheet = worksheet) |>
  dplyr::mutate(person="LN")
data_2024_25$observation_clean <- bind_rows(observations_LN, observations_SB) |>
  dplyr::filter(!is.na(Date))|>
  dplyr::mutate(
      ## Format dates and times
      ## ToDo - Combine date and time
      date = as.Date(Date, format="%Y-%m-%d"),
      time_start = as.POSIXct(`Time your started out`,
                              format = "%Y-%M-%d %H:%M:%S")|>
          format(format = "%H:%M:%S"),
      time_end = as.POSIXct(`Time you finished`,
                            format = "%Y-%M-%d %H:%M:%S")|>
          format(format = "%H:%M:%S"),
      ## st = ymd_hms(`Time your started out`, format = "%Y-%M-%d %H:%M:%S"),
      ## end = ymd_hms(`Time you finished`, format = "%Y-%M-%d %H:%M:%S"),
      ## Duration = as.integer(Time.end-Time.start)*60,
      ## Dividing the weather column
      weather = strsplit(weather, ","),
      ## Unique obs_ID
      obs_id = paste(Date, person, sep = "_"),
      ## Day of Year
      day_of_year = lubridate::yday(Date)
  ) |>
  dplyr::filter(!is.na(Date))|>
  unnest(weather) |>
  dplyr::mutate(value = 1,
         ## cleaning weather column
         weather = str_replace_all(weather, " ", ""),
         weather = str_replace_all(weather, "/", "_"))|>
  pivot_wider(names_from = weather, values_fill = 0)|>
  dplyr::select(person, date, time_start, time_end, obs_id, day_of_year,
                cloudy_grey, sunny, partlycloudy, windy, lightrain, foggy)
rm(observations_LN, observations_SB)


##################################################################
## Flock composition                                            ##
##################################################################
worksheet <- "flock composition"
composition_SB <- read_xlsx(paste0(XLS_DIR, "SARAH_Flocks_and_birds_seen_winter_2024-2025.xlsx"),
                      sheet = worksheet) |>
  dplyr::mutate(person="SB",
         Flock_Unique_ID=str_extract(Flock_Unique_ID, "[^.]+"))|>
  dplyr::rename(Left_leg=LEFT_leg,
         Right_leg=RIGHT_leg)
composition_LN <- read_xlsx(paste0(XLS_DIR, "LUKE_Flocks_and_birds_seen_winter_2024-2025.xlsx"),
                      sheet = worksheet) |>
    dplyr::mutate(person="LN",
           Flock_Unique_ID = as.character(Flock_Unique_ID))
data_2024_25$flock_clean <- bind_rows(composition_LN, composition_SB) |>
    dplyr::filter(Ringed == "yes",
           ## CR_certainty == "sure",
           !grepl("BTO",Colour_ring),
           str_length(Colour_ring) <= 5,
           Colour_ring %in% possible_colour_rings$colour_ring)|>
    dplyr::rename(comfo = `flock/pair/alone`,
           flock_id = Flock_Unique_ID,
           colour_ring = Colour_ring) |>
    dplyr::mutate(
        ## Date  & Times in good format
        date=as.Date(Date, format = "%Y-%m-%d"),
        time_seen = as.POSIXct(`Time seen`, format = "%Y-%M-%d %H:%M:%S") |>
            format(format = "%H:%M:%S"),
        ## Unique IDD
        IDD = paste(person, flock_id, sep = "_"),
        ## Day of Year
        day_of_year = lubridate::yday(Date)
    ) |>
  dplyr::select(date, IDD, flock_id, person, time_seen, colour_ring, comfo, notes)
rm(composition_LN, composition_SB)
write.csv(data_2024_25$flock_clean,
    paste0(CSV_DIR, "flock_clean_2024_2025.csv"),
    row.names = FALSE)


##################################################################
## GPS points                                                   ##
##################################################################
## Join metadata and GPS so we have IDD and mean/min/max lat/lon/ele
data_2024_25$locs <- dplyr::left_join(gps$gps_clean,
                                      data_2024_25$meta_clean,
                                      by = c("day_of_year", "person")) |>
    dplyr::filter(time >= time_seen & time <= time_lost) |>
    dplyr::select(IDD, lat, lon, ele) |>
    group_by(IDD) |>
    summarise(
        mean_lat = mean(lat),
        mean_lon = mean(lon),
        mean_ele = mean(ele),
        min_lat = min(lat),
        min_lon = min(lon),
        min_ele = min(ele),
        max_lat = max(lat),
        max_lon = max(lon),
        max_ele = max(ele))
## Joining the GPS data and additional location data
data_2024_25$locs <- dplyr::bind_rows(data_2024_25$locs, gps$additional_clean)
## Expect 176
length(unique(data_2024_25$locs$IDD))

# Combined meta and location data
data_2024_25$meta_clean_gps <- dplyr::left_join(data_2024_25$meta_clean,
                                                data_2024_25$locs,
                                                by = "IDD")

## ns-rse 2026-04-29 - Possibly unnecessary changes
## Clean flock_composition_clean using the possible_colour_rings
data_2024_25$flock_clean <- data_2024_25$flock_clean |>
    dplyr::mutate(
        ## Day of Year - possibly unnecessary?
        day_of_year = lubridate::yday(date),
        ## correcting rings
        colour_ring_certainty = dplyr::case_when(
            grepl("BTO", colour_ring) ~ NA,
            str_length(colour_ring) > 5 ~ NA,
            grepl("\\?", colour_ring) ~ NA,
            colour_ring == "U" ~ NA,
            TRUE ~ colour_ring),
        indiv_certainty = ifelse(is.na(colour_ring_certainty),
                                 paste0("UNK", cumsum(is.na(colour_ring_certainty))),
                                 colour_ring_certainty)) |>
    dplyr::rename(
        time = time_seen) |>
    dplyr::select(
        date,
        time,
        colour_ring_certainty,
        person,
        IDD,
        indiv_certainty,
        day_of_year)

## Merge with met data (with GPS)
data_2024_25$all_flock_data <- dplyr::left_join(data_2024_25$flock_clean,
                                                data_2024_25$meta_clean_gps,
                                                by = "IDD") |>
    dplyr::rename(date = date.x,
                  day_of_year = day_of_year.x,
                  person = person.x) |>
  dplyr::select(-date.y, -day_of_year.y, -person.y)
## Notes on merge
##
## - 13 instances where `date.x != date.y`
## - 29 instances where `day_of_year.x != day_of_year.y`
## - 0 instances where  `person.x != person.y`
