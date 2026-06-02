## Filename : server.R
## Description : Shiny server
library(DBI)
library(RSQLite)
library(bslib)
library(shiny)

testing <- TRUE
## If testing we need to clean existing data and populate the database in memory with this data.
if (testing) {
    db_path = ":memory:"
    ## source("clean.R")
} else {
    ## ...otherwise we have a database in place and load it.
    db_path = "../data/sqlite/lottie.sql"
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

    ## Sites
    RSQLite::dbWriteTable(
                 conn = con,
                 name = "Site",
                 site_df,
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
    composition_data <- shiny::reactiveVal(data.frame(
            date = character(),
            time = character(),
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
    ## output$composition_test <- shiny::renderTable({
    ##     composition_data()
    ## })
    output$composition <- shiny::renderTable(
        {
            composition_data()
        },
        striped = TRUE
    )
    ## Flock Description
    ## ns-rse 2026-05-12
    ## Warning: Error in data.frame: arguments imply differing number of rows: 1, 0
    ## 89: stop
    ## 88: data.frame
    ## 85: observe [/home/neil/work/git/hub/Morinay-Lab/lotties/ns-rse/shiny/lottie/R/server.R#140]
    ## 84: <observer:observeEvent(input$add_description)>
    ##  1: shiny::runApp
    description_data <- shiny::reactiveVal(data.frame(
            date = character(),
            start_time = character(),
            end_time = character(),
            flock_type= character(),
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
        description_to_add <- rbind(description_data(), data.frame(
            date = input$description_date,
            start_time = input$description_start_time,
            end_time = input$description_end_time,
            flock_type = input$description_flock_type,
            whole_flock = input$description_whole_flock,
            n_flock = input$description_n_flock,
            n_ringed = input$description_n_ringed,
            other_species = input$description_other_species,
            section = input$description_section,
            mist_net = input$description_mist_net,
            notes = input$description_notes,
            stringsAsFactors = FALSE
        ))
        description_data(description_to_add)
    })
    output$description <- shiny::renderTable(
        {
            description_data()
        },
        striped = TRUE
    )
    ## Interactions
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
    output$interactions <- shiny::renderTable(
        {
            interactions_data()
        },
        striped = TRUE
    )
    ## output$welcome <- shiny::renderPrint({
    ##     paste0("Welcome ", input$user, ", how are you today?")
    ## })
}
