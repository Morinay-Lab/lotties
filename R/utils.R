library(dplyr)
library(readr)
library(stringr)

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
#'
#' @returns Dataframe of GPS positions loaded from '.csv' file.
#'
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
                stringr::str_detect(name, pattern = "LN") == TRUE ~ "LN",
                stringr::str_detect(name, pattern = "MJ") == TRUE ~ "MJ",
                stringr::str_detect(name, pattern = "ND") == TRUE ~ "ND",
                stringr::str_detect(name, pattern = "SB|SJB") == TRUE ~ "SB",
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
#'
#' @returns Dataframe of flock description with columns renamed.
#' @export
clean_flock_description <- function(df, person) {
    df <- df |>
        dplyr::mutate(
            ## Date  & Times in good format
            date = lubridate::ymd(description_date),
            IDD = paste0(person, "_", flock_id),
            flock_id = description_flock_id,
            person = person,
            section = description_section,
            time_seen = lubridate::ymd_hm(description_start_time),
            time_lost = lubridate::ymd_hm(description_end_time),
            in_flock = description_flock_type,
            min_nb = description_n_flock,
            min_nb_unident = description_n_flock - description_n_ringed,
            notes = description_notes
        ) |>
        dplyr::select(
            date,
            IDD,
            flock_id,
            person,
            section,
            time_seen,
            time_lost,
            in_flock,
            min_nb,
            min_nb_unident,
            notes
        )
}

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
#' @returns Dataframe with missing columns added and `none` column removed.
#'
#' @export
tidy_columns <- function(df, expected_cols) {
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
#'
#' @returns Dataframe with column called `none` removed.
#' @export
remove_none_column <- function(df) {
    if ("none" %in% colnames(df)) {
        df <- df |> dplyr::select(-none)
    }
    df
}


#' Extract rings
#'
#' See the document
#' \href{https://docs.google.com/document/d/10JnL50Fm5DneEl9PgqYLbxUDLo6wcnaUbGvY6e3j8Oc/edit?tab=t.0}{Reading Colour
#' Rings} for further details on the rules used in the coding system.
#'
#' @param code str The code from which rings and leg are to be extracted.
#' @param valid_codes list List of valid codes, if `code` is not in `valid_codes` it is not extracted.
#' @param known_rings list List of rings used.
#'
#' @returns List of ring properties.
#'
#'  | Name     | Type    | Description                                              |
#'  |:---------|:--------|:---------------------------------------------------------|
#'  |`code`    | str     | The original full ring code.                             |
#'  |`leg`     | str     | The leg denoted by the full rung code (the last letter). |
#'  |`pit`     | boolean | Whether a "pit" ring is indicated.                       |
#'  |`pit_pos` | str     | The position of the "pit" ring.                          |
#'  |`bto`     | str     | The leg the BTO ring is on (inferred).                   |
#'  |`first`   | str     | The first ring from the full ring code.                  |
#'  |`second`  | str     | The second ring from the full ring code.                 |
#'
#' @export
extract_rings <- function(code, valid_codes, known_rings) {
    ## Validate that the supplied code is valid
    if (!(code %in% known_rings)) {
        print(print0("WARNING!!! The provided combination (", code, ") is not in known_rings."))
    }
    rings <- list()
    rings$code <- code
    ## Code is unlisted, return empty values, users should add their own
    if ((code == "Unlisted")) {
        rings$leg <- ""
        rings$pit <- NA
        rings$bto <- ""
        rings$first <- ""
        rings$second <- ""
        return(rings)
    }
    code_length <- stringr::str_length(code)
    rings$leg <- stringr::str_sub(code, start = -1)
    rings$pit <- FALSE
    ## BTO ring is always on the other leg to coloured rings...
    if (rings$leg == "L") {
        rings$bto <- "R"
    } else if (rings$leg == "R") {
        rings$bto <- "L"
    } else {
        rings$bto <- "None"
        rings$pit <- NA
    }
    ## ...unless the recorded ring is "BTO L" or "BTO R" in which case there are no other rings other than the BTO on
    ## the indicated leg.
    if (stringr::str_sub(code, start = 1, end = 3) == "BTO") {
        rings$bto <- stringr::str_sub(code, start = -1)
        rings$leg <- stringr::str_sub(code, start = -1)
        return(rings)
    }
    ## @ns-rse 2026-06-16 : Handle PIT rings first, felt it was simpler to follow the logic if done this way
    if (stringr::str_detect(code, "\\*")) {
        rings$pit <- TRUE
        ## If length is 3 then it is a PIT ring (first two characters) and a leg
        if (code_length == 3) {
            rings$first <- stringr::str_sub(code, 1, 2)
            rings$second <- ""
        } else if (code_length == 4) {
            ## If '*' is at the second position then PIT is first
            if (stringr::str_locate(code, "\\*")[1] == 2) {
                rings$first <- stringr::str_sub(code, 1, 2)
                rings$second <- stringr::str_sub(code, 3, 3)
                rings$pit_pos <- "first"
            ## Otherwise PIT is bottom
            } else {
                rings$first <- stringr::str_sub(code, 1, 1)
                rings$second <- stringr::str_sub(code, 2, 3)
                rings$pit_pos <- "second"
            }
        } else if (code_length == 5) {
            rings$first <- stringr::str_sub(code, 1, 2)
            rings$second <- stringr::str_sub(code, 3, 4)
            ## If '*' is at the second position then PIT is top
            if (stringr::str_locate(code, "\\*")[1] == 2) {
                rings$pit_pos <- "first"
            ## Otherwise PIT is bottom
            } else {
                rings$pit_pos <- "second"
            }
        } else {
            print(paste0("WARNING : code is > 5 characters : ", code))
        }
    } else if (code_length == 3) {
        ## If length is 3 then all codes are single letters, we can therefore easily split the top and bottom rings out
        rings$first <- stringr::str_sub(code, 1, 1)
        rings$second <- stringr::str_sub(code, 2, 2)
    } else if (code_length == 5) {
        ## If length is 5 then all codes are two letter and we can easily split top and bottom
        rings$first <- stringr::str_sub(code, 1, 2)
        rings$second <- stringr::str_sub(code, 3, 4)
        ## If length is 4 then it is trickier, we do not know if it is the first or second ring that is 2 characters
    } else if (code_length == 4) {
        ## We check to see if the first two characters are in the subset of rings that are two characters in length, if so we
        ## use the first two characters as the top and the third is the bottom
        known_rings2 <- known_rings[stringr::str_length(known_rings) == 2]
        if (code == "None") {
            rings$leg <- ""
            rings$first <- ""
            rings$second <- ""
        } else if (stringr::str_sub(code, 1, 2) %in% known_rings2) {
            rings$first <- stringr::str_sub(code, 1, 2)
            rings$second <- stringr::str_sub(code, 3, 3)
        ## If not then the first character is the top and the second and third are the bottom.
        } else {
            rings$first <- stringr::str_sub(code, 1, 1)
            rings$second <- stringr::str_sub(code, 2, 3)
        }
    }
    ## Check
    if (!(rings$leg %in% c("L", "R"))) {
        print(paste0("WARNING!!! Ring is neither L nor R : ", rings$leg))
    }
    if (!(rings$first %in% valid_codes)) {
        print(paste0("WARNING!!! Top ring is unknown : ", rings$first))
    }
    if (!(rings$second %in% valid_codes)) {
        print(paste0("WARNING!!! Bottom ring is unknown : ", rings$second))
    }
    rings
}

#' Update the selected value of a specific leg position.
#'
#' @param session Shiny session which is to be updated.
#' @param tag str A string indicating the leg/position to be updated (one of `left_top`, `left_bottom`, `right_top`,
#' `right_bottom`).
#' @param selected str The value to update the given tag with.
#'
#' @returns Nothing, updates a ring field input for given session.
#'
#' @export
update_ring <- function(session, tag, selected) {
    ## ns-rse 2026-06-23 - debugging
    ## print(paste0("Setting : ", tag))
    ## print(paste0("To : ", selected))
    shiny::updateSelectInput(
        session,
        paste("composition",
            tag,
            sep = "_"
        ),
        selected = selected
    )
}
#' Update individual ring options based on global selection.
#'
#' Takes the selected rings that have been split using `extract_ring()` and updates the displayed ring for individual
#' left/right top/left options.
#'
#' The logic handles PIT rings first, then BTO rings before handling any that are just coloured rings alone. BTO rings
#' are always present but are rarely explicitly stated.
#'
#' @param rings list A named list of ring attributes, returned by `extract_ring()`.
#' @param session A shiny session which is to be updated.
#'
#' @returns List of ring attributes. `pit` (boolean) `pit_pos` (str), `leg` (str), `code` (str), `bto` (str).
#'
#' @export
update_all_rings <- function(rings, session) {        ## We again deal with PIT ring logic separately
    if (rings$pit == TRUE) {
        ## ns-rse 2026-06-23 - Debugging
        ## print("PIT RING!")
        ## Coloured ring is on Left leg...
        if (rings$leg == "L") {
            ## ns-rse 2026-06-23 - Debugging
            ## print("LEFT")
            ## But we need to pull the ring from the correct top/bottom which is conditional on whether the PIT ring
            ## was listed first, this has been stored in the rings$pit_pos
            if (rings$pit_pos == "first") {
                ## ns-rse 2026-06-23 - Debugging
                ## print("PIT first")
                update_ring(session, tag = "left_top", selected = rings$first)
                update_ring(session, tag = "left_bottom", selected = "None")
                update_ring(session, tag = "right_top", selected = "BTO")
                update_ring(session, tag = "right_bottom", selected = rings$second)
            } else {
                ## ns-rse 2026-06-23 - Debugging
                ## print("PIT second")
                update_ring(session, tag = "left_top", selected = rings$second)
                update_ring(session, tag = "left_bottom", selected = "None")
                update_ring(session, tag = "right_top", selected = rings$first)
                update_ring(session, tag = "right_bottom", selected = "BTO")
            }
            ## Coloured ring is on the Right leg...
        } else {
            ## ns-rse 2026-06-23 - Debugging
            ## print("RIGHT")
            if (rings$pit_pos == "first") {
                ## ns-rse 2026-06-23 - Debugging
                ## print("PIT first")
                update_ring(session, tag = "left_top", selected = "BTO")
                update_ring(session, tag = "left_bottom", selected = rings$second)
                update_ring(session, tag = "right_top", selected = rings$first)
                update_ring(session, tag = "right_bottom", selected = "None")
            } else {
                ## ns-rse 2026-06-23 - Debugging
                ## print("PIT second")
                update_ring(session, tag = "left_top", selected = rings$first)
                update_ring(session, tag = "left_bottom", selected = "BTO")
                update_ring(session, tag = "right_top", selected = rings$second)
                update_ring(session, tag = "right_bottom", selected = "None")
            }
        }
    }
    ## Now set rings for birds with just BTO
    else if( stringr::str_sub(rings$code, 1, 3) == "BTO") {
        ## ns-rse 2026-06-23 - Debugging
        ## print("JUST BTO")
        if (rings$bto == "L") {
            ## ns-rse 2026-06-23 - Debugging
            ## print("LEFT")
            update_ring(session, tag = "left_top", selected = "BTO")
            update_ring(session, tag = "right_top", selected = "None")
        } else {
            ## ns-rse 2026-06-23 - Debugging
            ## print("RIGHT")
            update_ring(session, tag = "left_top", selected = "None")
            update_ring(session, tag = "right_top", selected = "BTO")
        }
        update_ring(session, tag = "left_bottom", selected = "None")
        update_ring(session, tag = "right_bottom", selected = "None")
    }
    ## Finally set rings for birds with just colour (no PIT but BTO on opposite leg)
    else if (rings$leg == "L") {
        ## ns-rse 2026-06-23 - Debugging
        ## print("NO PIT, JUST COLOUR")
        ## print("LEFT")
        update_ring(session, tag = "left_top", selected = rings$first)
        update_ring(session, tag = "left_bottom", selected = rings$second)
        update_ring(session, tag = "right_top", selected = "BTO")
        update_ring(session, tag = "right_bottom", selected = "None")
    } else if (rings$leg == "R") {
        ## ns-rse 2026-06-23 - Debugging
        ## print("NO PIT, JUST COLOUR")
        ## print("RIGHT")
        update_ring(session, tag = "left_top", selected = "BTO")
        update_ring(session, tag = "left_bottom", selected = "None")
        update_ring(session, tag = "right_top", selected = rings$first)
        update_ring(session, tag = "right_bottom", selected = rings$second)
    } else if(rings$code == "None"){
        ## ns-rse 2026-06-23 - Debugging
        ## print("NO RINGS")
        update_ring(session, tag = "left_top", selected = "None")
        update_ring(session, tag = "left_bottom", selected = "None")
        update_ring(session, tag = "right_top", selected = "None")
        update_ring(session, tag = "right_bottom", selected = "None")
    }
    update_ring(session, tag = "bto_ring_position", selected = rings$bto)
}

#' Update individual ring certainty based on overall certainty
#'
#' If the "Certainty" box is ticked for the overall ring code this function updates the relevant individual "Certainty"
#' checkbox for the given "tag"
#'
#' @param ring_certainty bool Whether the overall certainty is 'TRUE' or 'FALSE'.
#' @param tag str The individual certainty box to be updated ('left_top' | 'left_bottom' | 'right_top' | 'right_bottom')
#' @param session The Shiny session to update.
#'
#' @returns Nothing, updates ring_certainty fields input for given tag and session.
#'
#' @export
update_certainty <- function(ring_certainty, tag, session) {
    shiny::updateCheckboxInput(
               session,
               paste("composition", tag, "certain", sep = "_"),
               value = ring_certainty)
}

#' Update a `shiny::dateInput()` field.
#'
#' @param date Date/time to update with.
#' @param tag str The `inputID` of the element to be updated.
#' @param session Shiny session to update from.
#'
#' @returns Nothing, updates date input for given session.
#'
#' @export
update_date <- function(date, tag, session) {
    shiny::updateDateInput(session=session, tag, value=date)
}

#' Update a `shinyTime::timeInput()` field.
#'
#' @param date Date/time to update with.
#' @param tag str The `inputID` of the element to be updated.
#' @param session Shiny session to update from.
#'
#' @returns Nothing, updates time input for given session.
#'
#' @export
update_time <- function(date, tag, session) {
    shinyTime::updateTimeInput(session=session, tag, value=date)
}


#' Save data to SQLite database
#'
#' This generic function can be used to populate lookup tables when testing the database in memory or more generally
#' when running the database to add data that has been entered.
#'
#' @param data dataframe Dataframe of data to be added to a table in the database.
#' @param db_path str Path to database.
#' @param table str Table to add data to.
#' @param append bool Whether to append the data.
#'
#' @returns Nothing returned.
#'
#' @export
save_data <- function(data, db_path = db_path, table, append = TRUE, overwrite = FALSE) {
    conn <- DBI::dbConnect(RSQLite::SQLite, db_path)
    ## query <- sprintf("INSERT INTO %s (%s) VALUES ('%s')")
    ## paste(names(data), collapse = ",")
    ## paste(data, collapse = "','")
    RSQLite::dbWriteTable(conn = conn, name = table, value = data, overwrite = overwrite)
    DBI::dbDisconnect()
}

#' Extract and compress data from database to zip file.
#'
#' @param zip_file str Zip filename.
#' @param conn Database connection
#' @param input List of tables to extract
#'
#' @returns Returns nothing, exports database tables to CSV and compresses them.
#'
#' @export
extract_and_compress_data <- function(zip_file, conn, input) {
    ## Loop over selected tables
    csv_files <- list()
    for (table in input$download_raw_data_selection) {
        ## Select all from given table
        query <- paste0("SELECT * FROM ", table)
        df <- RSQLite::dbGetQuery(conn = conn, query = query)
        ## Lowercase table name and add .csv
        file_name <- paste0(tolower(table), ".csv")
        ## Write CSV file
        write.csv(df, file_name, row.names = FALSE)
        ## Add filename to list for zipping
        csv_files[[tolower(table)]] <- file_name
    }
    zip::zip(
        zipfile = zip_file,
        files = unlist(csv_files)
    )
    ## Remove CSV files from system
    unlink(csv_files)
}

#' Query a database table to check contents for debugging.
#'
#' @param conn Database connection.
#' @param table str Name of table to query.
#'
#' @returns Prints the table.
#'
#' @export
db_table_debug <- function(conn, table) {
    print(paste0("Checking table : "))
    query <- paste0("SELECT * FROM ", table)
    print(RSQLite::dbGetQuery(conn, query))
}

#' Setup empty dataframes for the different components.
#'
#' @returns List of empty dataframes used in the Shiny application.
#'
#' @export
create_empty_dataframes <- function() {
    empty_df <- list()
    empty_df$conditions <- data.frame(
        user = character(),
        date = character(),
        start_time = character(),
        end_time = character(),
        sunny = character(),
        partly_cloudy = character(),
        cloudy_grey = character(),
        foggy = character(),
        windy = character(),
        light_rain = character(),
        really_rain = character(),
        visibility = character(),
        stringsAsFactors = FALSE
    )
    empty_df$composition_data <- data.frame(
        date = character(),
        time = character(),
        flock_number = integer(),
        ringed = character(),
        colour_ring = character(),
        certain = character(),
        left_top = character(),
        left_top_certain = character(),
        left_bottom = character(),
        left_bottom_certain = character(),
        right_top = character(),
        right_top_certain = character(),
        right_bottom = character(),
        right_bottom_certain = character(),
        bto_ring_position = character(),
        notes = character(),
        stringsAsFactors = FALSE
    )
    empty_df$description_data <- data.frame(
        date = character(),
        start_time = character(),
        end_time = character(),
        flock_type = character(),
        flock_number = integer(),
        whole_flock = character(),
        n_flock = integer(),
        n_ringed = integer(),
        section = character(),
        mist_net = character(),
        notes = character(),
        blue_tit = logical(),
        chiff_chaff = logical(),
        chaffinch = logical(),
        coal_tit = logical(),
        dunnock = logical(),
        goldcrest = logical(),
        great_tit = logical(),
        nuthatch = logical(),
        robin = logical(),
        siskin = logical(),
        tree_creeper = logical(),
        unknown_tit = logical(),
        woodpecker = logical(),
        wren = logical(),
        willow_warbler = logical(),
        stringsAsFactors = FALSE,
        check.names = FALSE
    )
    empty_df$interactions_data <- data.frame(
        date = character(),
        time = character(),
        flock_a = integer(),
        flock_b = integer(),
        foraging_together = logical(),
        a_chasing_b = logical(),
        b_chasing_a = logical(),
        close_but_not_interacting = logical(),
        other = logical(),
        notes = character(),
        stringsAsFactors = FALSE)
    empty_df$gps_data <- data.frame(
        Filename = character(),
        Points = integer(),
        Start=character(),
        Finish = character())
}

#' Render a dataframe table as a Shiny.
#'
#' @param df dataframe Dataframe for rendering as table.
#' @param striped bool Whether the table should be striped or not (default `TRUE`).
#'
#' @returns Shiny rendered table.
#'
#' @export
render_table <- function(df, striped = TRUE) {
        shiny::renderTable(
            {df},
            striped = striped
        )}

#' Filter a list of inputs for a subset and remove those that are to be excluded.
#'
#' We wish to reset input fields, to do so we need a list of labels used for `input` objects. This is provided by the
#' `all_inputs()` function which returns _all_ inputs for a given session. We have three areas we wish to subset
#' `composition`, `description`, and `interactions` but there are a small number of each that we do _not_ want to
#' reset because they are either automatically incremented or take their values from other areas that have been input
#' (e.g. when entering in `composition_` fields we wish to retain the `flock_number` that has been entered from the
#' `flock` description; for interactions we wish the list of possible flocks under `flock_a` and `flock_b` to be
#' based on the flocks that have been entered already.
#'
#' @param inputs list A list of strings which are to be filtered.
#' @param filter str The string on which to filter, should be one of `^composition_`, `^description_`, and
#' `^interactions_`.
#' @param exclude list A list of inputs to exclude.
#'
#' @returns List of input fields filtered for the subset specified but with some fields excluded.
#'
#' @export
filter_inputs <- function(inputs, filter, exclude) {
    tmp_inputs <- inputs[stringr::str_detect(inputs, filter)]
    tmp_inputs[!(tmp_inputs %in% exclude)]
}

#' De-duplicate ringed entries from flock description.
#'
#' When entering data there should be no duplicated individuals in the flock with rings. Un-ringed birds are
#' permissible, by virtue of sharing the characteristic of not having rings.
#'
#' @param df dataframe Dataframe for de-duplicating.
#'
#' @returns Dataframe with ringed birds deduplicated.
#'
#' @export
deduplicate_flock <- function(df) {
    rbind(
        df |> dplyr::filter(ringed == FALSE),
        df |> dplyr::filter(ringed == TRUE) |> unique()
    )
}

#' Update ring component fields.
#'
#' This function is called when the `composition_ringed` field is changed to "No" (`FALSE`) and clears all ring fields
#' so that no erroneous data is submitted.
#'
#' @param session The session to be updated.
#'
#' @returns Updates input fields for rings in the supplied session.
#' @export
update_rings_when_not_ringed <- function(session) {
    shiny::updateSelectInput(session, "composition_colour_ring", selected="None")
    shiny::updateCheckboxInput(session, "composition_certain", value = FALSE)
    for (tag in c("left_top", "left_bottom", "right_top", "right_bottom")) {
        shiny::updateSelectInput(session, paste0("composition_", tag), selected = "None")
        shiny::updateCheckboxInput(session, paste0("composition_", tag, "certain"), value = FALSE)
    }
    shiny::updateSelectInput(session, "composition_bto_ring_position", selected="None")
}


## End of file
