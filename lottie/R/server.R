## Filename : server.R
## Description : Shiny server
library(DBI)
library(RSQLite)
library(bslib)
library(shiny)

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


#' Save data to SQLite database
#'
#' This generic function can be used to populate lookup tables when testing the database in memory or more generally
#' when running the database to add data that has been entered.
#'
#' @param data dataframe Dataframe of data to be added to a table in the database.
#' @param db_path str Path to database.
#' @param table str Table to add data to.
#' @param append bool Whether to append the data.
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
extract_and_compress_data <- function(zip_file, conn, input) {
    ## Loop over selected tables
    csv_files <- list()
    for (table in input$download_raw_data_selection) {
        ## Select all from given table
        query <- paste0("SELECT * FROM ", table)
        df <- RSQLite::dbGetQuery(conn=conn, query=query)
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
    ## TODO : Remove CSV files from system
}

if (testing) {
    ## Load lookup dataframes and populate database if testing
    source("lookups.R")
    overwrite <- TRUE
    ## People
    RSQLite::dbWriteTable(
                 conn = con,
                 name = "Person",
                 person_df,
                 overwrite = overwrite)

    ## Other Species
    RSQLite::dbWriteTable(
                 conn = con,
                 name = "OtherSpecies",
                 other_species_df,
                 overwrite = overwrite)

    ## Rings
    RSQLite::dbWriteTable(
                 conn = con,
                 name = "Rings",
                 rings_df,
                 overwrite = overwrite)

    ## Section
    RSQLite::dbWriteTable(
                 conn = con,
                 name = "Site",
                 section_df,
                 overwrite = overwrite)
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

server <- function(input, output, session) {
    ## Flock Composition
    ## Build a data frame of birds within a flock when the "Submit bird description" button is clicked
    composition_data <- shiny::reactiveVal(data.frame(
            date = character(),
            time = character(),
            flock_number = integer(),
            ringed = character(),
            colour_ring = character(),
            certain = character(),
            left_top= character(),
            left_top_certain = character(),
            left_bottom= character(),
            left_bottom_certain = character(),
            right_top = character(),
            right_top_certain = character(),
            right_bottom = character(),
            right_bottom_certain = character(),
            bto_ring_position = character(),
            notes = character(),
            stringsAsFactors = FALSE
            ))
   shiny::observeEvent(input$add_composition, {
        composition_to_add <- rbind(composition_data(), data.frame(
            date = input$composition_date,
            time = input$composition_time,
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
        composition_data(composition_to_add)
    })
    ## The composition table is returned and rendered on the page
    output$composition <- shiny::renderTable(
        {
            composition_data()
        },
        striped = TRUE
        )
    ## Add data to SQLite database when the "Submit all composition data" button is pressed
    shiny::observeEvent(input$submit_composition, {
        RSQLite::dbWriteTable(
                     conn = con,
                     name = "Composition",
                     composition_data(),
                     overwrite = FALSE,
                     append = TRUE)
        ## @ns-rse 2026-06-02 Debugging...
        ## print("WHAT HAVE WE GOT IN THE DATABASE Composition TABLE?")
        ## query <- "SELECT * FROM Composition"
        ## print(RSQLite::dbGetQuery(conn = con, query))
    })
    ## Flock Description
    ## Build a data frame of flock description when the "Submit flock description" button is clicked
   description_data <- shiny::reactiveVal(data.frame(
            date = character(),
            start_time = character(),
            end_time = character(),
            flock_type = character(),
            flock_number = integer(),
            whole_flock = character(),
            n_flock = integer(),
            n_ringed = integer(),
            other_species = character(),
            section = character(),
            mist_net = character(),
            notes = character(),
            stringsAsFactors = FALSE
    ))
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
        description_to_add <- rbind(description_data(),
                                    data.frame(
                                        date = input$description_date,
                                        start_time = input$description_start_time,
                                        end_time = input$description_end_time,
                                        flock_type = input$description_flock_type,
                                        flock_number = input$description_flock_number,
                                        whole_flock = input$description_whole_flock,
                                        n_flock = input$description_n_flock,
                                        n_ringed = input$description_n_ringed,
                                        other_species = description_other_species,
                                        section = input$description_section,
                                        mist_net = input$description_mist_net,
                                        notes = input$description_notes,
                                        stringsAsFactors = FALSE
                                    ))
        description_data(description_to_add)
    })
    ## The description table is returned and rendered on the page
    output$description <- shiny::renderTable(
        {
            description_data()
        },
        striped = TRUE
    )
    ## Add data to SQLite database when the "Submit all flock data" button is pressed
    shiny::observeEvent(input$submit_description, {
        RSQLite::dbWriteTable(
                     conn = con,
                     name = "Description",
                     description_data(),
                     overwrite = FALSE,
                     append = TRUE)
        ## @ns-rse 2026-06-02 Debugging...
        ## print("WHAT HAVE WE GOT IN THE DATABASE Description TABLE?")
        ## query <- "SELECT * FROM Description"
        ## print(RSQLite::dbGetQuery(conn = con, query))
    })
    ## Interactions
    ## Build a data frame of interactions when the "Submit interaction" button is clocked
    interactions_data <- shiny::reactiveVal(data.frame(
            date = character(),
            time = character(),
            flock_a = integer(),
            flock_b = integer(),
            type = character(),
            notes = character(),
            stringsAsFactors = FALSE
    ))
    shiny::observeEvent(input$add_interactions, {
        interactions_to_add <- rbind(interactions_data(), data.frame(
            date = input$interactions_date,
            time = input$interactions_time,
            flock_a = input$interactions_flock_a,
            flock_b = input$interactions_flock_b,
            type = input$interactions_type,
            notes = input$interactions_notes,
            stringsAsFactors = FALSE
        ))
        interactions_data(interactions_to_add)
    })
    ## The interaction table is returned and rendered on the page
    output$interactions <- shiny::renderTable(
        {
            interactions_data()
        },
        striped = TRUE
    )
    ## Add data to SQLite database when the "Submit all interaction data" button is pressed
    shiny::observeEvent(input$submit_interactions, {
        RSQLite::dbWriteTable(
                     conn = con,
                     name = "Interactions",
                     interactions_data(),
                     overwrite = FALSE,
                     append = TRUE)
        ## @ns-rse 2026-06-02 Debugging...
        ## print("WHAT HAVE WE GOT IN THE DATABASE Interactions TABLE?")
        ## query <- "SELECT * FROM Interactions"
        ## print(RSQLite::dbGetQuery(conn = con, query))
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
                ".zip"
            )
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
                files = unlist(csv_files)
            )
            ## ns-rse 2026-06-08 : Remove CSV files from the system
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
                files = unlist(csv_files)
            )
            ## ns-rse 2026-06-08 : Remove CSV files from the system
        })
}
