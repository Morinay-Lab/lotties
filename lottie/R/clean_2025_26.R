## Filename : clean_2025_26.R
## Origin : 2025-2026_Flock_following_cleaning.Rmd
## Description : Cleans the flock following and GPS data

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

## Setup a list to hold 2025/26 data
data_2025_26 <- list()
##################################################################
## Metadata                                                     ##
##################################################################
worksheet <- "flock description"
meta_SB <- read_xlsx(paste0(XLS_DIR, "SARAH_Flocks_and_birds_seen_winter_2025-2026.xlsx"),
    sheet = worksheet,
  col_types = c("date", "numeric", "date", "text", "text", "text", "text", "date", "text", "text", "text")
) |>
    dplyr::mutate(person="SB") |>
  dplyr::rename(`Other species` = `Other species in the flock`)
meta_LN <- read_xlsx(paste0(XLS_DIR, "LUKE_Flocks_and_birds_seen_winter_2025-2026.xlsx"),
                     sheet = worksheet) |>
    dplyr::mutate(person="LN") |>
  dplyr::rename(`Other species` = `Other species in the flock`)
meta_MJ <- read_xlsx(paste0(XLS_DIR, "MICKO_Flocks_and_birds_seen_winter_2025-2026.xlsx"),
                     sheet = worksheet) |>
    dplyr::mutate(person="MJ") |>
    dplyr::rename(`Other species` = `Other species in the flock`)
meta_ND <- read_xlsx(paste0(XLS_DIR, "NINA_Flocks_and_birds_seen_winter_2025-2026.xlsx"),
                     sheet = worksheet) |>
    dplyr::mutate(person="ND") |>
    dplyr::rename(`Other species` = `Other species in the flock`)
data_2025_26$meta_clean <- bind_rows(meta_LN, meta_SB, meta_MJ, meta_ND) |>
    dplyr::mutate(
    ## minimum nb of birds seen in the flock
    min_nb = str_extract_all(`Nb of birds in the flock`, "[0-9]+") |>
      purrr::map(~ifelse(length(.x) == 1,
                         as.numeric(.x),
                         min(unlist(as.numeric(.x))) ) ) |> unlist(),
    ## deleting question marks
    nb_unringed = gsub("\\?.*", NA, `Nb of unringed`),
    # min nb of unidentified birds in the flock
    min_nb_unindent = str_extract_all(nb_unringed, "[0-9]+") |>
        purrr::map(~ifelse(length(.x) > 0,
                    ifelse(length(.x) == 1,
                           as.numeric(.x),
                           min(unlist(as.numeric(.x)))),
                    NA)) |> unlist() ,
    min_nb_unindent = case_when(min_nb_unindent == "NA" ~ NA,
                                TRUE ~ min_nb_unindent),
    # Date  & Times in good format
    date=as.Date(Date, format="%Y-%m-%d"),
    time_seen = as.POSIXct(`Time first seen`,
                           format = "%Y-%M-%d %H:%M:%S") |>
      format(format = "%H:%M:%S"),
    time_lost = as.POSIXct(`Time lost/left`,
                           format = "%Y-%M-%d %H:%M:%S") |>
      format(format = "%H:%M:%S"),
    # Correcting a missing times
    time_seen = case_when(is.na(time_seen)~"12:54:00",
                          TRUE~time_seen),
    time_lost = case_when(is.na(time_lost)~"13:14:00",
                          TRUE~time_lost),
    # Presence of other species:
    BT=case_when(str_detect(`Other species`,
                            pattern = "BLUTI | Bluti|bluti|Blue tit|blue tit|Blue Tit") == TRUE ~ 1,
                 TRUE ~ NA),
    GT=case_when(str_detect(`Other species`,
                            pattern = "Great Tit|great tit|greti|Greti") == TRUE ~ 1,
                 TRUE ~ NA),
    CT=case_when(str_detect(`Other species`,
                            pattern = "coati|coat tit|Coal Tit") == TRUE ~ 1,
                 TRUE ~ NA),
    GC=case_when(str_detect(`Other species`,
                            pattern = "Goldcrest|goldcrest|goldc|GOLDC|Goldc") == TRUE ~ 1,
                 TRUE ~ NA),
    RB=case_when(str_detect(`Other species`,
                            pattern = "robin") == TRUE ~ 1,
                 TRUE ~ NA),
    NH=case_when(str_detect(`Other species`,
                            pattern = "Nuthatch|Nutha") == TRUE ~ 1,
                 TRUE ~ NA),
    TC=case_when(str_detect(`Other species`,
                            pattern = "Treecreeper|treecreeper|tree creeper|treec|Treec|TREEC") == TRUE ~ 1,
                 TRUE ~ NA),
    WP = case_when(str_detect(`Other species`,
                            pattern = "Great Spotted Woodpecker") == TRUE ~ 1,
                 TRUE ~ NA),
    no_other_species=case_when(str_detect(`Other species`, pattern = "NA|none") == TRUE ~ 0,
                               TRUE ~ NA),
    # Creating unique ID
    IDD = paste(person, flockID, sep = "_"),
    # Day of Year
    day_of_year = lubridate::yday(date)
  ) |>
  # renaming
    dplyr::rename(in_flock = `in flock`,
           flock_id = flockID,
           section = Section,
           whole_flock_id = Whole_flock_ID) |>
  # Selecting only some columns
  dplyr::select(date, day_of_year, IDD, flock_id, person, section, time_seen, time_lost,
         in_flock, whole_flock_id, min_nb, min_nb_unindent,
         BT, GT, CT, GC, RB, NH, TC, WP, no_other_species, notes)

rm(meta_LN, meta_SB, meta_MJ, meta_ND)

##################################################################
## Observation data                                             ##
##################################################################
worksheet <- "times out looking for them"
observations_SB <- read_xlsx(paste0(XLS_DIR,"SARAH_Flocks_and_birds_seen_winter_2025-2026.xlsx"),
                             sheet = worksheet) |>
    dplyr::mutate(person = "SB")
observations_LN <- read_xlsx(paste0(XLS_DIR, "LUKE_Flocks_and_birds_seen_winter_2025-2026.xlsx"),
                  sheet = worksheet) |>
    dplyr::mutate(person = "LN")
observations_MJ <- read_xlsx(paste0(XLS_DIR, "MICKO_Flocks_and_birds_seen_winter_2025-2026.xlsx"),
                  sheet = worksheet) |>
    dplyr::mutate(person = "MJ")

observations_ND <- read_xlsx(paste0(XLS_DIR, "NINA_Flocks_and_birds_seen_winter_2025-2026.xlsx"),
                  sheet = worksheet) |>
  dplyr::mutate(person = "ND")
data_2025_26$observations_clean <- bind_rows(observations_LN,
                                            observations_SB,
                                            observations_MJ,
                                            observations_ND) |>
      dplyr::filter(!is.na(Date)) |>
  dplyr::mutate(
    # Date  & Times in good format
    Date=as.Date(Date, format="%Y-%m-%d"),
    Time.start = as.POSIXct(`Time your started out`, format = "%Y-%M-%d %H:%M:%S") |>
      format(format = "%H:%M:%S"),
    Time.end = as.POSIXct(`Time you finished`, format = "%Y-%M-%d %H:%M:%S") |>
      format(format = "%H:%M:%S"),
    # st = ymd_hms(`Time your started out`, format = "%Y-%M-%d %H:%M:%S"),
    # end = ymd_hms(`Time you finished`, format = "%Y-%M-%d %H:%M:%S"),

    # Duration = as.integer(Time.end-Time.start)*60,
    # Dividing the weather column
    weather = strsplit(weather,",")) |>
  dplyr::filter(!is.na(Date)) |>
  unnest(weather) |>
  dplyr::mutate(value = 1,
        # cleaning weather column
        weather=str_replace_all(weather, " ", ""),
        weather=str_replace_all(weather, "/", "_")) |>
  pivot_wider(names_from = weather, values_fill = 0) |>
  dplyr::mutate(
    ## Unique obs_ID
    obs_ID=paste(Date, person, sep = "_"),
    ## Day of Year
    day_of_year=lubridate::yday(Date)) |>
    ## dplyr::selecting columns
    dplyr::select(-`Time your started out`, -`Time you finished`)
rm(observations_LN, observations_SB, observations_MJ, observations_ND)


##################################################################
## Flock composition                                            ##
##################################################################
worksheet <- "flock composition"
composition_SB <- read_xlsx(paste0(XLS_DIR, "SARAH_Flocks_and_birds_seen_winter_2025-2026.xlsx"),
                            sheet = worksheet) |>
    dplyr::mutate(person = "SB",
         Flock_Unique_ID=str_extract(Flock_Unique_ID, "[^.]+")) |>
  dplyr::rename(Left_leg=LEFT_leg,
         Right_leg=RIGHT_leg) |>
  dplyr::filter(!is.na(Date))
composition_LN <- read_xlsx(paste0(XLS_DIR, "LUKE_Flocks_and_birds_seen_winter_2025-2026.xlsx"),
                            sheet = worksheet) |>
  dplyr::mutate(person = "LN",
         Flock_Unique_ID = as.character(Flock_Unique_ID))
composition_MJ <- read_xlsx(paste0(XLS_DIR, "MICKO_Flocks_and_birds_seen_winter_2025-2026.xlsx"),
                            sheet = worksheet) |>
  dplyr::mutate(person = "MJ",
         Flock_Unique_ID = as.character(Flock_Unique_ID))
composition_ND <- read_xlsx(paste0(XLS_DIR, "NINA_Flocks_and_birds_seen_winter_2025-2026.xlsx"),
                            sheet = worksheet) |>
  dplyr::mutate(person = "ND",
         Flock_Unique_ID = as.character(Flock_Unique_ID))
data_2025_26$flock_clean <- bind_rows(composition_LN, composition_SB, composition_MJ, composition_ND) |>
    dplyr::filter(!is.na(Date)) |>
    dplyr::filter(Ringed=="yes",
           ## CR_certainty=="sure",
           !grepl("BTO",Colour_ring),
           str_length(Colour_ring) <= 5,
           Colour_ring %in% possible_colour_rings$colour_ring) |>
    dplyr::rename(comfo=`flock/pair/alone`,
           flock_id = Flock_Unique_ID,
           colour_ring=Colour_ring) |>
    dplyr::mutate(
        ## Date  & Times in good format
        date=as.Date(Date, format="%Y-%m-%d"),
        time_seen = as.POSIXct(`Time seen`, format = "%Y-%M-%d %H:%M:%S") |>
            format(format = "%H:%M:%S"),
        ## Unique IDD
        IDD=paste(person, flock_id, sep = "_"),
        ## Day of Year
        day_of_year=lubridate::yday(Date)
    ) |>
    dplyr::select(date, IDD, flock_id, person, time_seen, colour_ring, CR_certainty, comfo, notes)
rm(composition_LN, composition_SB, composition_MJ, composition_ND)
write.csv(data_2025_26$flock_clean,
    paste0(CSV_DIR, "flock_clean_2025_2026.csv"),
    row.names = FALSE)


##################################################################
## GPS points                                                   ##
##################################################################
## Join metadata and GPS so we have IDD and mean/min/max lat/lon/ele
##
data_2025_26$locs <- dplyr::left_join(gps$gps_clean,
    data_2025_26$meta_clean,
    by = c("date", "person")
) |>
    dplyr::filter(time >= time_seen & time <= time_lost) |>
    dplyr::ungroup() |>
    dplyr::select(IDD, lat, lon, ele) |>
    dplyr::group_by(IDD) |>
    dplyr::summarise(
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
data_2025_26$locs <- bind_rows(data_2025_26$locs, gps$additional_clean)

# Combine meta and location data
data_2025_26$meta_clean_gps <- dplyr::left_join(data_2025_26$meta_clean,
                                                data_2025_26$locs,
                                                by = "IDD")

## Merge flock with meta data (with GPS)
data_2025_26$all_flock_data <- dplyr::left_join(
                                          data_2025_26$flock_clean,
                                          data_2025_26$meta_clean_gps,
                                          by = "IDD") |>
    dplyr::rename(date = date.x,
                  person = person.x) |>
  dplyr::select(-date.y, -person.y)
## Notes on merge
##
## - 1182 instances where `time_seen.x != time_seen.y`
## - 41 instances where `date.x != date.y`
## - 29 instances where `day_of_year.x != day_of_year.y`
## - 0 instances where  `person.x != person.y`
## - 0 instances where  `flock_id.x != flock_id.y`

#################################################################
## Merge info                                                  ##
#################################################################
## Aggregate some summary data on number of rows and unique
## observations in data that is combined/collpased
merge_data <- list()
merge_data$gps_clean <- c("gps$gps_clean (track)",
    gps$gps_clean |> nrow(),
    gps$gps_clean |> dplyr::ungroup() |> dplyr::select(track, date, person) |> unique() |> nrow(),
    NA
)
merge_data$meta_clean <- c(
    "data_2025_26$meta_clean (no track)",
    data_2025_26$meta_clean |> nrow(),
    data_2025_26$meta_clean |> dplyr::ungroup() |> dplyr::select(date, person) |> unique() |> nrow(),
    data_2025_26$meta_clean |> dplyr::ungroup() |> dplyr::select(IDD) |> unique() |> nrow()
)
tmp_left_join <- dplyr::left_join(gps$gps_clean,
    data_2025_26$meta_clean,
    by = c("date", "person")
)
merge_data$left_join <- c(
    "left_join, no summarise (track)",
    tmp_left_join |> nrow(),
    tmp_left_join |> dplyr::ungroup() |> dplyr::select(track, date, person) |> unique() |> nrow(),
    tmp_left_join |> dplyr::ungroup() |> dplyr::select(IDD) |> unique() |> nrow()
)
merge_data$locs <- c(
    "data_2025_26$locs (no track)",
    data_2025_26$locs |> nrow(),
    NA,
    data_2025_26$locs |> dplyr::ungroup() |> dplyr::select(IDD) |> unique() |> nrow()
)
merge_data$additional_clean <- c("gps$additional_clean (no track)",
    gps$additional_clean |> nrow(),
    NA,
    gps$additional_clean |> dplyr::ungroup() |> dplyr::select(IDD) |> unique() |> nrow()
)
merge_data$meta_clean_gps <- c(
    "data_2025_26$meta_clean_gps (no track)",
    data_2025_26$meta_clean_gps |> nrow(),
    data_2025_26$meta_clean_gps |> dplyr::ungroup() |> dplyr::select(date, person) |> unique() |> nrow(),
    data_2025_26$meta_clean_gps |> dplyr::ungroup() |> dplyr::select(IDD) |> unique() |> nrow()
)
merge_data$flock_clean <- c(
    "data_2025_26$flock_clean (no track)",
    data_2025_26$flock_clean |> nrow(),
    data_2025_26$flock_clean |> dplyr::ungroup() |> dplyr::select(date, person) |> unique() |> nrow(),
    data_2025_26$flock_clean |> dplyr::ungroup() |> dplyr::select(IDD) |> unique() |> nrow()
)
merge_data$all_flock_data <- c(
    "data_2025_26$all_flock_data (no track)",
    data_2025_26$all_flock_data |> nrow(),
    data_2025_26$all_flock_data |> dplyr::ungroup() |> dplyr::select(date, person) |> unique() |> nrow(),
    data_2025_26$all_flock_data |> dplyr::ungroup() |> dplyr::select(IDD) |> unique() |> nrow()
)
merge_data$table <- rbind(
    merge_data$gps_clean,
    merge_data$meta_clean,
    merge_data$left_join,
    merge_data$locs,
    merge_data$additional_clean,
    merge_data$meta_clean_gps,
    merge_data$flock_clean
) |> dplyr::as_tibble()
colnames(merge_data$table) <- c("Data", "Rows", "Unique ([track/]date/person)", "Unique (IDD)")
message("Summary of tables being combined (2025/26)...")
merge_data$table |>
    knitr::kable() |>
    capture.output() |>
    paste0(collapse = "\n") |>
    message()
message("Notes on flock and meta data (with GPS) merge 2024/25...")
message("")
message(" - 1182 instances where `time_seen.x != time_seen.y`")
message(" - 41 instances where `date.x != date.y`")
message(" - 29 instances where `day_of_year.x != day_of_year.y`")
message(" - 0 instances where  `person.x != person.y`")
message(" - 0 instances where  `flock_id.x != flock_id.y`")
