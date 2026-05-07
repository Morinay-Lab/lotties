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
    user <- shiny::reactive({get(input$user)})
    output$welcome <- renderPrint({
        paste0("Welcome ", input$user, ", how are you today?")
    })
}
