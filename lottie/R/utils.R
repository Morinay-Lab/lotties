library(readr)

#' Clean a GPS file.
#'
#' Extracts GPS data from the supplied file. The 'person' variable is derived from patterns in the file name, currently
#' 'LN', 'MJ', 'ND', 'SB' and 'SJB'. Typically when the GPS data is exported to CSV the files have 42 rows in their
#' header which are not required, hence the 'skip' value being 42. Many GPS tracks appear to have a slight lag in
#' obtaining position and the first two recorded points retain the lat/lon/elevation from the last recorded trip. These
#' can optionally be removed using the 'drop_two' boolean option which defaults to TRUE.
#'
#' @param file str Path to file to be loaded.
#' @param format str Format of GPS file, currently on "csv" is supported.
#' @param skip int Number of rows in header of ".csv" to skip. Default is 42.
#' @param col_select list List of columns to select. Default is c("name", "lat", "lon", "ele", "time")
#' @param drop_two bool Whether to drop the first two rows of GPS data points. Default is TRUE.
#' @export
clean_gps <- function(
                      file,
                      format = "csv",
                      skip = 42,
                      col_select = c("name", "lat", "lon", "ele", "time"),
                      drop_two = TRUE) {
    gps_clean <- dplyr::read_csv(file, id = "name", col_names = TRUE, skip = skip) |>
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
        dplyr::select(-name)
    if (drop_two) {
        gps_clean <- gps_clean |>
            dplyr::group_by(track) |>
            dplyr::slice(3:n())
    }
    gps_clean
}

#' Clean Flock Description data.
#'
#' Mostly this is just a case of renaming variables. THe data points as captured are cleaner.
#'
#' @param df data.frame Data frame to be cleaned.
#' @param person str Person data petains to, should be one of 'LN', 'MJ', 'ND', 'SB'.
#' @export
#'
#'
## Need the following columns
##
## "date"
## "day_of_year"
## "IDD"
## "flock_id"
## "person"
## "section"
## "time_seen"
## "time_lost"
## "in_flock"
## "whole_flock_id"
## "min_nb"
## "min_nb_unindent"
## "BT"
## "GT"
## "CT"
## "GC"
## "RB"
## "NH"
## "TC"
## "WP"
## "no_other_species"
## "notes"
clean_flock_description <- function(df, person) {
    df <- df |>
        dplyr::mutate(
            ## Date  & Times in good format
            date = lubridate::ymd(description_date),
            # day_of_year = ,
            IDD = paste0(person, "_", flock_id),
            flock_id = description_flock_id,
            person = person,
            section = description_section,
            time_seen = lubridate::ymd_hm(description_start_time),
            time_lost = lubridate::ymd_hm(description_end_time),
            in_flock = description_flock_type,
            # whole_flock_id =
            min_nb = description_n_flock,
            min_nb_unident = description_n_flock - description_n_ringed,
            # BT =  ,
            # GT =  ,
            # CT =  ,
            # GC =  ,
            # RB =  ,
            # NH =  ,
            # TC =  ,
            # WP =  ,
            # no_other_species =  ,
            notes = description_notes
        ) |>
        dplyr::select(
            date,
            # day_of_year,
            IDD,
            flock_id,
            person,
            section,
            time_seen,
            time_lost,
            in_flock,
            # whole_flock_id,
            min_nb,
            min_nb_unident,
            # BT,
            # GT,
            # CT,
            # GC,
            # RB,
            # NH,
            # TC,
            # WP,
            # no_other_species,
            notes
        )
}

#' Clean Flock Composition data.
#'
#' Mostly this is just a case of renaming variables. The data points as captured are cleaner.
#'
#' @param df data.frame Data frame to be cleaned.
#' @param person str Person data petains to, should be one of 'LN', 'MJ', 'ND', 'SB'.
#' @export
#'
#'
## Need the following columns
## date
## time
## colour_ring_certainty
## person
## IDD
## indiv_certainty
## day_of_year
## clean_flock_composition <- function() {}


#' Clean Flock Interactions data.
#'
#' Mostly this is just a case of renaming variables. THe data points as captured are cleaner.
#'
#' @param df data.frame Data frame to be cleaned.
#' @param person str Person data petains to, should be one of 'LN', 'MJ', 'ND', 'SB'.
#' @export
#'
#'
## Need the following columns
## clean_flock_interactions <- function(df, person) {
##     df <- df |> dplyr::mutate(date = composition_date,)
## }


#' Extract rings
#'
#' @param code str The code from which rings and leg are to be extracted.
#' @param valid_codes list List of valid codes, if `code` is not in `valid_codes` it is not extracted.
#' @param known_rings list List of rings used.
extract_rings <- function(code, valid_codes, known_rings) {
    ## Validate that the supplied code is valid
    if (!(code %in% known_rings)) {
        print(print0("WARNING!!! The provided combination (", code, ") is not in known_rings."))
    }
    code_length <- stringr::str_length(code)
    rings <- list()
    rings$leg <- stringr::str_sub(code, start = -1)

    ## @ns-rse 2026-06-16 : Handle BTO rings first, felt it was simpler to follow the logic if done this way
    if (stringr::str_detect(code, "\\*")) {
        if (code_length == 3) {
            rings$top <- stringr::str_sub(code, 1, 2)
            rings$bottom <- ""
        } else if (code_length == 4) {
            if (stringr::str_locate(code, "\\*")[1] == 2) {
                rings$top <- stringr::str_sub(code, 1, 2)
                rings$bottom <- stringr::str_sub(code, 3, 3)
            } else {
                rings$top <- stringr::str_sub(code, 1, 1)
                rings$bottom <- stringr::str_sub(code, 2, 3)
            }
        } else if (code_length == 5) {
            rings$top <- stringr::str_sub(code, 1, 2)
            rings$bottom <- stringr::str_sub(code, 3, 4)
        } else {
            print(paste0("WARNING : code is > 5 characters : ", code))
        }
    } else if (code_length == 3) {
        ## If length is 3 then all codes are upper case and single letter codes are used, we can therefore easily split the
        ## top and bottom rings out
        rings$top <- stringr::str_sub(code, 1, 1)
        rings$bottom <- stringr::str_sub(code, 2, 2)
    } else if (code_length == 5) {
        ## If length is 5 then all codes are two letter and we can easily split top and bottom
        rings$top <- stringr::str_sub(code, 1, 2)
        rings$bottom <- stringr::str_sub(code, 3, 4)
        ## If length is 4 then it is trickier, we do not know if it is the first or second ring that is 2 characters
    } else if (code_length == 4) {
        ## We check to see if the first two characters are in the subset of rings that are two characters in length, if so we
        ## use the first two characters as the top and the third is the bottom
        known_rings2 <- known_rings[stringr::str_length(known_rings) == 2]
        if (code == "None") {
            rings$leg <- ""
            rings$top <- ""
            rings$bottom <- ""
        } else if (stringr::str_sub(code, 1, 2) %in% known_rings2) {
            rings$top <- stringr::str_sub(code, 1, 2)
            rings$bottom <- stringr::str_sub(code, 3, 3)
        ## If not then the first character is the top and the second and third are the bottom.
        } else {
            rings$top <- stringr::str_sub(code, 1, 1)
            rings$bottom <- stringr::str_sub(code, 2, 3)
        }
    }
    ## Check
    if (!(rings$leg %in% c("L", "R"))) {
        print(paste0("WARNING!!! Ring is neither L nor R : ", rings$leg))
    }
    if (!(rings$top %in% valid_codes)) {
        print(paste0("WARNING!!! Top ring is unknown : ", rings$top))
    }
    if (!(rings$bottom %in% valid_codes)) {
        print(paste0("WARNING!!! Bottom ring is unknown : ", rings$bottom))
    }
    rings
}

## End of file
