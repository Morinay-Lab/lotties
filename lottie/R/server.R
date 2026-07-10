## Filename : server.R
## Description : Shiny server
library(DBI)
library(RSQLite)
library(shiny)
library(xml2)

## ns-rse 2026-06-15 - hack to load the extract_ring() function, need to work out how to get package installed in renv
## so we can use `lottie::extract_ring()`
source("utils.R")

## When developing set to TRUE, otherwise set to FALSE
testing <- TRUE
## testing <- FALSE
## If testing we load the database in memory with this data.
if (testing) {
    db_path <- ":memory:"
    ## source("clean.R")
} else {
    ## ...otherwise we have a database on disc and load it.
    db_path <- "../data/sqlite/lottie.sql"
}
## Setup connection to database
con <- DBI::dbConnect(RSQLite::SQLite(), db_path)



## Load lookup dataframes and populate database if testing
if (testing) {
    ## Load lookup dataframes and populate database if testing
    source("lookups.R")
    overwrite <- TRUE
    ## People
    RSQLite::dbWriteTable(
        conn = con,
        name = "Person",
        person_df,
        overwrite = overwrite
    )

    ## Other Species
    RSQLite::dbWriteTable(
        conn = con,
        name = "OtherSpecies",
        other_species_df,
        overwrite = overwrite
    )

    ## Rings
    RSQLite::dbWriteTable(
        conn = con,
        name = "Rings",
        rings_df,
        overwrite = overwrite
    )

    ## Section
    RSQLite::dbWriteTable(
        conn = con,
        name = "Site",
        section_df,
        overwrite = overwrite
    )
    ## Conditions
    ## This needs creating up-front because we reshape the input data from long to wide and can not guarantee that the
    ## first entry will include all possible values for "weather". This would mean that the table in the database was
    ## missing some columns and subsequent attempts to append data would fail.
    RSQLite::dbWriteTable(
        conn = con,
        name = "Conditions",
        data.frame(
            "user" = character(),
            "date" = character(),
            "start_time" = character(),
            "end_time" = character(),
            "weather" = character(),
            "visibility" = character(),
            "cloudy_grey" = logical(),
            "foggy" = logical(),
            "light_rain" = logical(),
            "partly_cloudy" = logical(),
            "really_rainy" = logical(),
            "sunny" = logical(),
            "windy" = logical(),
            stringsAsFactors = FALSE,
            check.names = FALSE
        ),
        overwrite = overwrite
    )
    ## GPS
    RSQLite::dbWriteTable(
        conn = con,
        name = "GPS",
        data.frame(
            "time" = character(),
            "lat" = numeric(),
            "lon" = numeric(),
            "ele" = numeric(),
            "filename" = character(),
            stringsAsFactors = FALSE,
            check.names = FALSE
        ),
        overwrite = overwrite
    )
} else {
    ## Otherwise extract lookups from database
    ## Person
    query <- "SELECT * FROM Person"
    person_df <- RSQLite::dbGetQuery(con, query)
    ## Other Species
    query <- "SELECT * FROM OtherSpecies"
    other_species_df <- RSQLite::dbGetQuery(con, query)
    ## Rings
    query <- "SELECT * FROM Rings"
    rings_df <- RSQLite::dbGetQuery(con, query)
    ## Sites
    query <- "SELECT * FROM Sites"
    sites_df <- RSQLite::dbGetQuery(con, query)
}

## Setup empty dataframes
empty_dataframes <- create_empty_dataframes()

server <- function(input, output, session) {
    #################################################################################
    ## Conditions                                                                  ##
    #################################################################################
    conditions_data <- shiny::reactiveVal(empty_dataframes$conditions_data)
    shiny::observeEvent(input$submit_conditions, {
        ## ns-rse - Reshape the data to wide as weather column can have multiple values and these expand to long format
        ## data
        to_add <- data.frame(
            user = input$user,
            date = as.character(input$conditions_date),
            start_time = as.character(format(input$conditions_start_time, "%H:%M")),
            end_time = as.character(format(input$conditions_end_time, "%H:%M")),
            weather = input$conditions_weather,
            visibility = input$conditions_visibility,
            stringsAsFactors = FALSE) |>
        dplyr::mutate(present = TRUE) |>
            tidyr::pivot_wider(
                names_from = weather,
                values_from = present,
                values_fill = FALSE)
        to_add <- tidy_columns(df = to_add, expected_cols = as.list(conditions_df$code))
        conditions_to_add <- rbind(conditions_data(),  to_add)
        conditions_data(conditions_to_add)
    })
    output$conditions <- shiny::renderTable(
        {
            conditions_data()
        },
        striped = TRUE
    )

    #################################################################################
    ## GPS                                                                         ##
    #################################################################################
    gps_data <- shiny::observeEvent(input$gpx, {
        ## Load GPX data from input$gpx ($datapath is the path to the temporary file that has been uploaded)
        gpx_data <- xml2::read_xml(input$gpx$datapath)
        ## Extract the namespace
        gpx_namespace <- xml2::xml_ns(gpx_data)
        ## Extract the trkpt (the lat/lon)
        trkpts <- xml2::xml_find_all(gpx_data, ".//d1:trkpt", gpx_namespace)
        ## Extract the lat/lon from trkpts and convert to numeric
        lat <- xml2::xml_attr(trkpts, "lat") |> as.numeric()
        lon <- xml2::xml_attr(trkpts, "lon") |> as.numeric()
        ## ns-rse 2026-06-18 - Extract to a function in utils.R
        ## Extract the ele, handling missing
        ele <- vapply(trkpts, function(n) {
            x <- xml2::xml_find_first(n, "d1:ele", gpx_namespace)
            if (is.na(x)) NA_character_ else xml2::xml_text(x)
        }, character(1))
        ele <- as.numeric(ele)
        ## Extract the time, handling missing
        time <- vapply(trkpts, function(n) {
            x <- xml2::xml_find_first(n, "d1:time", gpx_namespace)
            if (is.na(x)) NA_character_ else xml2::xml_text(x)
        }, character(1))
        time <- as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        ## Extract filename
        filename <- paste0(
            xml2::xml_find_first(gpx_data, ".//d1:trk/d1:name", gpx_namespace) |> xml2::xml_text(),
            ".gpx"
        )
        ## Build dataframe
        gps_df <- data.frame(
            time = as.character(time),
            lat = lat,
            lon = lon,
            ele = ele,
            filename = rep(filename, length(lat))
        )
        ## Update the date/time fields with those from GPX file, these may not be exact but are closer than using the
        ## current date/time
        start_date_time = min(gps_df$time)
        end_date_time = max(gps_df$time)
        update_date(date = start_date_time, tag = "conditions_date", session)
        update_time(date = start_date_time, tag = "conditions_start_time", session)
        update_time(date = end_date_time, tag = "conditions_end_time", session)
        update_date(date = start_date_time, tag = "composition_date", session)
        update_time(date = start_date_time, tag = "composition_time", session)
        update_date(date = start_date_time, tag = "description_date", session)
        update_time(date = start_date_time, tag = "description_start_time", session)
        update_time(date = end_date_time, tag = "description_end_time", session)
        update_date(date = start_date_time, tag = "interactions_date", session)
        update_time(date = start_date_time, tag = "interactions_time", session)

        ## Update the user field based on the uploaded filename (which may be different from the internal filename
        ## extracted above)
        uploaded_filename <- dplyr::select(input$gpx, name)
        user <- NULL
        for (i in seq_len(nrow(person_df))) {
            row <- person_df[i, ]
            to_match <- unlist(c(row$code, strsplit(row$synonyms, split = ",")))
            if (grepl(pattern = paste(to_match, collapse = "|"), uploaded_filename)) {
                user <- row$code
                ## print(paste("Matched user:", user))
                break
            }
        }
        shiny::updateSelectInput(
            session = session,
            inputId = "user",
            selected = user)

        ## Add to database
        RSQLite::dbWriteTable(
            conn = con,
            name = "GPS",
            gps_df,
            overwrite = FALSE,
            append = TRUE
        )
        ## @ns-rse 2026-06-02 Debugging...
        ## print("WHAT HAVE WE GOT IN THE DATABASE GPS TABLE?")
        ## query <- "SELECT * FROM GPS"
        ## print(RSQLite::dbGetQuery(conn = con, query))
        ## gps_df
    })

    ## Make a table out of the single GPX filename
    output$gps_file_table <- renderTable(
        {
            req(input$gpx)
            ## ns-rse 2026-06-19- ideally I would like to use the `gps_df` from the above shiny::observeEvent() but
            ## haven't been able to work out how to access it
            gpx_data <- xml2::read_xml(input$gpx$datapath)
            ## Extract the namespace
            gpx_namespace <- xml2::xml_ns(gpx_data)
            ## Extract the trkpt (the lat/lon)
            trkpts <- xml2::xml_find_all(gpx_data, ".//d1:trkpt", gpx_namespace)
            ## Extract the time, handling missing
            time <- vapply(trkpts, function(n) {
                x <- xml2::xml_find_first(n, "d1:time", gpx_namespace)
                if (is.na(x)) NA_character_ else xml2::xml_text(x)
            }, character(1))
            time <- as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
            gps_summary <- data.frame(
                "Filename" = dplyr::select(input$gpx, name),
                "Points" = length(trkpts),
                "Start" = as.character(min(time)),
                "Finish" = as.character(max(time))
            )
            gps_summary
        },
        striped = TRUE)

    #################################################################################
    ## Flock Composition                                                           ##
    #################################################################################
    ## Extract ring colours from selection so they can be used to populate the `selectInput(..., choices=)` of the
    ## `colour_ring_inputs()` function
    selected_rings <- shiny::reactive({
        ## We split the returned code using lottie::extract_rings(), this returns rings$leg, rings$top and rings$bottom
        rings <- extract_rings(
            code = input$composition_colour_ring,
            valid_codes = rings_df$code,
            known_rings = colour_ring_df$code)
        rings
    })
    ## Having split the rings we can now use the leg and and assign the top/bottom to be selected automagically in the
    ## UI
    shiny::observe({
        rings <- selected_rings()
        update_all_rings(rings, session)
    })
    ## Update certainty boxes
    ring_composition_certain <- shiny::reactive({
        input$composition_certain
    })
    shiny::observe({
        ring_certainty <- ring_composition_certain()
        update_certainty(ring_certainty, tag = "left_top", session)
        update_certainty(ring_certainty, tag = "left_bottom", session)
        update_certainty(ring_certainty, tag = "right_top", session)
        update_certainty(ring_certainty, tag = "right_bottom", session)
    })
    ## Build the dataframe/table of birds within a flock when the "Submit bird description" button is clicked
    composition_data <- shiny::reactiveVal(empty_dataframes$composition_data)
    shiny::observeEvent(input$add_composition, {
        composition_to_add <- rbind(composition_data(), data.frame(
            date = as.character(input$composition_date),
            time = as.character(format(input$composition_time, "%H:%M")),
            flock_number = input$composition_flock_number,
            ringed = input$composition_ringed,
            colour_ring = input$composition_colour_ring,
            certain = input$composition_certain,
            left_top = input$composition_left_top,
            left_top_certain = input$composition_left_top_certain,
            left_bottom = input$composition_left_bottom,
            left_bottom_certain = input$composition_left_bottom_certain,
            right_top = input$composition_right_top,
            right_top_certain = input$composition_right_top_certain,
            right_bottom = input$composition_right_bottom,
            right_bottom_certain = input$composition_right_bottom_certain,
            bto_ring_position = input$composition_bto_ring_position,
            notes = input$composition_notes,
            stringsAsFactors = FALSE
        ))
        ## Reset the input fields using shinyjs, we get the list of all ids that are to be reset from the reactive
        ## function all_inputs(), this requires filtering all_inputs for those that start with composition_
        ## then removing those we do not want to update (in this case composition_flock_number since we want to
        ## increment that automatically)
        tmp_inputs <- filter_inputs(
            input=all_inputs(),
            filter = "^composition_",
            exclude = c("composition_flock_number"))
        lapply(tmp_inputs, shinyjs::reset)
        composition_data(composition_to_add)
    })
    ## The composition table is returned and rendered on the page
    output$composition <- shiny::renderTable(
        {
            composition_data()
        },
        striped = TRUE)

    #################################################################################
    ## Flock Description                                                           ##
    #################################################################################
    ## Build a data frame of flock description when the "Submit flock description" button is clicked
    description_data <- shiny::reactiveVal(empty_dataframes$description_data)
    ## Update flock size based on flock_type being `Pair` (2) or `Individual` (1)
    flock_type <- shiny::reactive({
        input$description_flock_type
    })
    shiny::observe({
        if (flock_type() == "pair") {
            shiny::updateNumericInput(session, "description_n_flock", value = 2)
            shiny::updateNumericInput(session, "description_n_ringed", value = 2)
        } else if (flock_type() == "individual") {
          shiny::updateNumericInput(session, "description_n_flock", value = 1)
          shiny::updateNumericInput(session, "description_n_ringed", value = 1)
        } else {
          shiny::updateNumericInput(session, "description_n_flock", value = 12)
          shiny::updateNumericInput(session, "description_n_ringed", value = 12)
        }
    })
    ## Build the dataframe/table of flock descriptions when the "Submit flock description" button is clicked
    shiny::observeEvent(input$add_description, {
        shiny::validate(
                   shiny::need(input$description_n_ringed <= input$description_n_flock,
                               "You can not have more ringed birds than the total flock size."))
        ## We need to check if there are other species, if there are none then the value is NULL and this causes errors
        ## when creating dataframes.
        if (is.null(input$description_other_species)) {
            description_other_species <- ""
        } else {
            description_other_species <- input$description_other_species
        }
        ## ns-rse - Reshape the data to wide as other_species column can have multiple
        ## values and are captured in long format
        to_add <-data.frame(
            date = as.character(input$description_date),
            start_time = as.character(format(input$description_start_time, "%H:%M")),
            end_time = as.character(format(input$description_end_time, "%H:%M")),
            flock_type = input$description_flock_type,
            flock_number = input$description_flock_number,
            whole_flock = input$description_whole_flock,
            n_flock = input$description_n_flock,
            n_ringed = input$description_n_ringed,
            other_species = description_other_species,
            section = input$description_section,
            mist_net = input$description_mist_net,
            notes = input$description_notes,
            stringsAsFactors = FALSE) |>
            dplyr::mutate(present = TRUE) |>
            tidyr::pivot_wider(
                names_from = other_species,
                values_from = present,
                values_fill = FALSE)
        ## Add potentially missing columns
        to_add <- tidy_columns(df = to_add, expected_cols = as.list(other_species_df$code))
        description_to_add <- rbind(description_data(),
                                    to_add)
        ## Update available flocks based on added descriptions (used in composition and interactions)
        flocks <- as.vector(description_to_add$flock_number)
        shiny::updateSelectInput(session, "composition_flock_number", choices = flocks)
        shiny::updateSelectInput(session, "interactions_flock_a", choices = flocks)
        shiny::updateSelectInput(session, "interactions_flock_b", choices = flocks)
        ## Reset the input fields using shinyjs, we get the list of all ids that are to be reset from the reactive
        ## function all_inputs(), this requires filtering all_inputs for those that start with description_
        ## then removing those we do not want to update (in this case description_flock_number since we want to
        ## increment that automatically)
        tmp_inputs <- filter_inputs(
            input=all_inputs(),
            filter = "^description_",
            exclude = c("description_flock_number"))
        lapply(tmp_inputs, shinyjs::reset)
        description_data(description_to_add)
    })
    ## Increment Flock numbers
    flock_number <- shiny::reactive({
        input$description_flock_number
    })
    shiny::observeEvent(input$add_description, {
        shiny::updateNumericInput(session, "description_flock_number", value = flock_number() + 1)
    })
    ## The description table is returned and rendered on the page
    output$description <- shiny::renderTable(
        {
            description_data()
        },
        striped = TRUE)

    #################################################################################
    ## Interactions                                                                ##
    #################################################################################
    ## Build a data frame of interactions when the "Submit interaction" button is clocked
    interactions_data <- shiny::reactiveVal(empty_dataframes$interactions_data)
    shiny::observeEvent(input$add_interactions, {
        ## ns-rse - Reshape the data to wide as type column can have multiple
        ## values and are captured in long format
        to_add <- data.frame(
            date = as.character(input$interactions_date),
            time = as.character(format(input$interactions_time, "%H:%M")),
            flock_a = input$interactions_flock_a,
            flock_b = input$interactions_flock_b,
            type = input$interactions_type,
            notes = input$interactions_notes,
            stringsAsFactors = FALSE) |>
            dplyr::mutate(present = TRUE) |>
            tidyr::pivot_wider(names_from = type, values_from = present, values_fill = FALSE)
        to_add <- tidy_columns(df = to_add, expected_cols = as.list(interactions_df$code))
        interactions_to_add <- rbind(interactions_data(), to_add)
        ## Reset the input fields using shinyjs, we get the list of all ids that are to be reset from the reactive
        ## function all_inputs(), this requires filtering all_inputs for those that start with description_
        ## then removing those we do not want to update (in this case interactions_flock_number since we want to
        ## increment that automatically)
        tmp_inputs <- filter_inputs(
            input=all_inputs(),
            filter = "^interactions_",
            exclude = c("interactions_flock_a", "interactions_flock_b"))
        interactions_data(interactions_to_add)
    })
    ## The interaction table is returned and rendered on the page
    output$interactions <- shiny::renderTable(
        {
            interactions_data()
        },
        striped = TRUE)

    #################################################################################
    ## Extract a list of all input id's                                            ##
    #################################################################################
    all_inputs <- shiny::reactive({
        x <- shiny::reactiveValuesToList(input)
        names(x)
    })

    #################################################################################
    ## Database submission                                                         ##
    #################################################################################
    ## Add data to SQLite database when the "Submit all data" button is pressed
    shiny::observeEvent(input$submit_all, {
        ## GPS data is submitted on file upload
        ## Conditions/metadata
        RSQLite::dbWriteTable(
            conn = con,
            name = "Conditions",
            unique(conditions_data()),
            overwrite = FALSE,
            append = TRUE
        )
        ## @ns-rse 2026-06-08 Debugging...
        db_table_debug(conn = con, table = "Conditions")
        ## Composition
        RSQLite::dbWriteTable(
            conn = con,
            name = "Composition",
            composition_data(),
            overwrite = FALSE,
            append = TRUE
        )
        ## @ns-rse 2026-06-02 Debugging...
        db_table_debug(conn = con, table = "Composition")
        ## Description
        RSQLite::dbWriteTable(
            conn = con,
            name = "Description",
            unique(description_data()),
            overwrite = FALSE,
            append = TRUE
        )
        ## @ns-rse 2026-06-02 Debugging...
        db_table_debug(conn = con, table = "Description")
        ## Interactions
        RSQLite::dbWriteTable(
            conn = con,
            name = "Interactions",
            unique(interactions_data()),
            overwrite = FALSE,
            append = TRUE
        )
        ## @ns-rse 2026-06-02 Debugging...
        db_table_debug(conn = con, table = "Interactions")
        ## Reset the input fields using shinyjs, we get the list of all ids that are to be reset from the reactive
        ## function all_inputs()
        lapply(all_inputs(), shinyjs::reset)
    })



    ## Download Raw Data
    output$download_raw_data <- shiny::downloadHandler(
        filename = function() {
            paste0(
                "lottie",
                stringr::str_replace(
                    Sys.time(),
                    " ",
                    "_"
                ),
                ".zip")
        },
    ## ns-rse 2026-06-08 : Would like to avoid duplication of code and have a single function but this errors with
    ##
    ## Warning: Error in download$func: unused argument (tmpdata)
    ##     content = function() {
    ##         extract_and_compress_data(
    ##             zip_file = file,
    ##             conn = con,
    ##             input = input$download_raw_data_selection
    ##     )}
    ## )
        content = function(file) {
            ## Loop over selected tables
            csv_files <- list()
            for (table in input$download_raw_data_selection) {
                ## Select all from given table
                query <- paste0("SELECT * FROM ", table)
                df <- RSQLite::dbGetQuery(con, query)
                ## Lowercase table name and add .csv
                file_name <- paste0(tolower(table), ".csv")
                ## Write CSV file
                write.csv(df, file_name, row.names = FALSE)
                ## Add filename to list for zipping
                csv_files[[tolower(table)]] <- file_name
            }
            zip::zip(
                zipfile = file,
                files = unlist(csv_files))
            ## Remove CSV files from the system
            unlink(csv_files)
        })
    output$download_clean_data <- shiny::downloadHandler(
        filename = function() {
            paste0("lottie",
                   stringr::str_replace(Sys.time(),
                                        " ",
                                        "_"),
                   ".zip")
        },
        content = function(file) {
            ## Loop over selected tables
            csv_files <- list()
            for (table in input$download_clean_data_selection) {
                ## Select all from given table
                query <- paste0("SELECT * FROM ", table)
                df <- RSQLite::dbGetQuery(con, query)
                ## Lowercase table name and add .csv
                file_name <- paste0(tolower(table), ".csv")
                ## Write CSV file
                write.csv(df, file_name, row.names = FALSE)
                ## Add filename to list for zipping
                csv_files[[tolower(table)]] <- file_name
            }
            zip::zip(
                zipfile = file,
                files = unlist(csv_files))
            ## Remove CSV files from the system
            unlink(csv_files)
        })
}
