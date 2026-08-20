## Filename    : setup.R
## Author      : n.shephard@sheffield.ac.uk
## Description : Setup a clean SQLite3 database with no observation data. It includes the lookup tables that areq
##               required though. Typically this script _won't_ need to be used very often, it is used to setup the
##               database at 'inst/sqlite/lottie.sql' so that when testing <- FALSE is set in server.R the lookup
##               tables/dataframes are loaded from the database.
library(DBI)
library(RSQLite)
library(shiny)
library(xml2)

db_path <- "inst/sqlite/lottie.sql"
## Setup connection to database
con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
overwrite <- FALSE

## Load the lookup tables/dataframes
source("R/lookups.R")

## People
RSQLite::dbWriteTable(conn = con,
                      name = "Person",
                      person_df,
                      overwrite = overwrite)

## Other Species
RSQLite::dbWriteTable(conn = con,
                      name = "OtherSpecies",
                      other_species_df,
                      overwrite = overwrite)

## Rings
RSQLite::dbWriteTable(conn = con,
                      name = "Rings",
                      valid_rings_df,
                      overwrite = overwrite)

## Section
RSQLite::dbWriteTable(conn = con,
                      name = "Sites",
                      section_df,
                      overwrite = overwrite)
## Conditions
## This needs creating up-front because we reshape the input data from long to wide and can not guarantee that the
## first entry will include all possible values for "weather". This would mean that the table in the database was
## missing some columns and subsequent attempts to append data would fail.
RSQLite::dbWriteTable(conn = con,
                      name = "Conditions",
                      data.frame("user" = character(),
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
                                 check.names = FALSE),
                      overwrite = overwrite)
## GPS
RSQLite::dbWriteTable(conn = con,
                      name = "GPS",
                      data.frame("time" = character(),
                                 "lat" = numeric(),
                                 "lon" = numeric(),
                                 "ele" = numeric(),
                                 "filename" = character(),
                                 stringsAsFactors = FALSE,
                                 check.names = FALSE),
                      overwrite = overwrite)

## Composition
RSQLite::dbWriteTable(conn = con,
                      name = "Composition",
                      data.frame("date" = character(),
                                 "time" = character(),
                                 "flock_number" = character(),
                                 "ringed" = numeric(),
                                 "colour_ring" = character(),
                                 "certain" = numeric(),
                                 "left_top" = character(),
                                 "left_top_certain" = numeric(),
                                 "left_bottom" = character(),
                                 "left_bottom_certain" = numeric(),
                                 "right_top" = character(),
                                 "right_top_certain" = numeric(),
                                 "right_bottom" = character(),
                                 "right_bottom_certain" = numeric(),
                                 "notes" = character(),
                                 "bto_ring_position" = character(),
                                 stringsAsFactors = FALSE,
                                 check.names = FALSE),
                      overwrite = overwrite)

## Description
RSQLite::dbWriteTable(conn = con,
                      name = "Description",
                      data.frame("date" = character(),
                                 "start_time" = character(),
                                 "end_time" = character(),
                                 "flock_type" = character(),
                                 "flock_number" = character(),
                                 "whole_flock" = numeric(),
                                 "n_flock" = numeric(),
                                 "n_ringed" = numeric(),
                                 "section" = character(),
                                 "mist_net" = numeric(),
                                 "notes" = character(),
                                 "chaffinch" = numeric(),
                                 "dunnock" = numeric(),
                                 "great_tit" = numeric(),
                                 "willow_warbler" = numeric(),
                                 "blue_tit" = numeric(),
                                 "chiff_chaff" = numeric(),
                                 "coal_tit" = numeric(),
                                 "goldcrest" = numeric(),
                                 "nuthatch" = numeric(),
                                 "robin" = numeric(),
                                 "siskin" = numeric(),
                                 "tree_creeper" = numeric(),
                                 "unknown_tit" = numeric(),
                                 "woodpecker" = numeric(),
                                 "wren" = numeric(),
                                 stringsAsFactors = FALSE,
                                 check.names = FALSE),
                      overwrite = overwrite)

## Interactions
RSQLite::dbWriteTable(conn = con,
                      name = "Interactions",
                      data.frame("date" = character(),
                                 "time" = character(),
                                 "flock_a" = character(),
                                 "flock_b" = character(),
                                 "notes" = numeric(),
                                 "a_chasing_b" = numeric(),
                                 "foraging_together" = numeric(),
                                 "b_chasing_a" = character(),
                                 "close_but_not_interacting" = numeric(),
                                 "other" = character(),
                                 stringsAsFactors = FALSE,
                                 check.names = FALSE),
                      overwrite = overwrite)
