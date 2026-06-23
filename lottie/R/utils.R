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

#' Tidy other bird species columns.
#'
#' Ensures there is a boolean column for each possible "Other species" that might be observed. This is required because
#' if a species is _not_ observed then after reshaping to wide there is no boolean column for that species. As a
#' consequence combining the dataframe with other flock description observations during that session will fail and
#' attempting to update the database will also fail.
#'
#' Further when recording "Other Species" the default check box that is selected is "None" so that we do not observe an
#' error when reshaping the data if no "Other Species" are observed. As a consequence it is possible for the reshaped
#' data frame to contain a column called `none` which is irrelevant (whether this is because it was not unchecked when
#' adding "Other Species" or there were genuinely no "Other Species" observed). A call is therefore made to the
#' `remove_none_column()` function so that if the "None" check box was checked the resulting column is removed.
#'
#' @param df data.frame Data frame to be augmented.
#' @param expected_cols list[str] List of columns that the data frame should hold.
#'
tidy_other_species_columns <- function(df, expected_cols) {
    for (missing_col in expected_cols) {
        if (!(missing_col %in% colnames(df))) {
            df[[missing_col]] <- FALSE
        }
    }
    df <- remove_none_column(df)
    df
}

#' Remove 'none' column from a dataframe if it exists.
#'
#' @param df data.frame Data frame to be augmented.
remove_none_column <- function(df) {
    if ("none" %in% colnames(df)) {
        df <- df |> dplyr::select(-none)
    }
    df
}


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
    rings$bto <- FALSE

    ## @ns-rse 2026-06-16 : Handle BTO rings first, felt it was simpler to follow the logic if done this way
    if (stringr::str_detect(code, "\\*")) {
        rings$bto <- TRUE
        ## If length is 3 then it is a BTO ring (first two characters) and a leg
        if (code_length == 3) {
            rings$top <- stringr::str_sub(code, 1, 2)
            rings$bottom <- ""
        } else if (code_length == 4) {
            ## If '*' is at the second position then BTO is top
            if (stringr::str_locate(code, "\\*")[1] == 2) {
                rings$top <- stringr::str_sub(code, 1, 2)
                rings$bottom <- stringr::str_sub(code, 3, 3)
                rings$bto_pos <- "Top"
            ## Otherwise BTO is bottom
            } else {
                rings$top <- stringr::str_sub(code, 1, 1)
                rings$bottom <- stringr::str_sub(code, 2, 3)
                rings$bto_pos <- "Bottom"
            }
        } else if (code_length == 5) {
            rings$top <- stringr::str_sub(code, 1, 2)
            rings$bottom <- stringr::str_sub(code, 3, 4)
            ## If '*' is at the second position then BTO is top
            if (stringr::str_locate(code, "\\*")[1] == 2) {
                rings$bto_pos <- "Top"
            ## Otherwise BTO is bottom
            } else {
                rings$bto_pos <- "Bottom"
            }
        } else {
            print(paste0("WARNING : code is > 5 characters : ", code))
        }
    } else if (code_length == 3) {
        ## If length is 3 then all codes are single letters, we can therefore easily split the top and bottom rings out
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

#' Update individual ring certainty based on overall certainty
#'
#' If the "Certainty" box is ticked for the overall ring code this function updates the relevant individual "Certainty"
#' checkbox for the given "tag"
#'
#' @param ring_certainty bool Whether the overall certainty is 'TRUE' or 'FALSE'.
#' @param tag str The individual certainty box to be updated ('left_top' | 'left_bottom' | 'right_top' | 'right_bottom')
#' @param session The Shiny session to update.
update_certainty <- function(ring_certainty, tag, session) {
    shiny::updateCheckboxInput(
               session,
               paste("composition", tag, "certain", sep = "_"),
               value = ring_certainty)
}

## End of file
