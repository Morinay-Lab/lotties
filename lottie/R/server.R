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

save_data <- function(data, db_path = db_path, table) {
    conn <- DBI::dbConnect(RSQLite::SQLite, db_path)
    query <- sprintf("INSERT INTO %s (%s) VALUES ('%s')")
    paste(names(data), collapse = ",")
    paste(data, collapse = "','")
    RSQLite::dbGetQuery(conn, query)
    DBI::dbDisconnect()
}
#'
#' @param table str The table to pull data from, one of 'Person', 'Rings' or 'OtherSpecies'
#' @param db_path str Path to database.
get_lookups <- function(table, db_path = db_path) {
    conn <- DBI::dbConnect(RSQLite::SQLite, db_path)
    query <- sprintf("SELECT * FROM %s", table)
    lookup <- RSQLite::dbGetQuery(conn, query)
    DBI::dbDisconnect()
    lookup
}

get_rings <- function(db_path = db_path) {
    conn <- DBI::dbConnect(RSQLite::SQLite, db_path)
    query <- "SELECT * FROM Rings"
    rings <- RSQLite::dbGetQuery(conn, query)
    DBI::dbDisconnect()
    rings
}

server <- function(input, output, session) {
    ## Flock Composition
    composition_data <- shiny::reactiveVal(data.frame(
            date = character(),
            time = character(),
            colour_ring = character(),
            ringed = character(),
            left_leg = character(),
            left_certainty = character(),
            right_leg = character(),
            right_certainty = character(),
            bto_ring_position = character(),
            notes = character(),
            stringsAsFactors = FALSE
    ))
    shiny::observeEvent(input$add_composition, {
        composition_to_add <- rbind(composition_data(), data.frame(
            date = input$composition_date,
            time = input$composition_time,
            colour_ring = input$composition_colour_ring,
            ringed = input$composition_ringed,
            left_leg = input$composition_left_leg,
            left_certainty = input$composition_left_certainty,
            right_leg = input$composition_right_leg,
            right_certainty = input$composition_right_certainty,
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
            notes = character()
    ))
    shiny::observeEvent(input$add_interactions, {
        interactions_to_add <- rbind(interactions_data(), data.frame(
            date = input$interactions_date,
            time = input$interactions_start_time,
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
