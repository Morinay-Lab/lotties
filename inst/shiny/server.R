## Filename : server.R
## Description : Shiny server
library(DBI)
library(RSQLite)
library(shiny)
library(xml2)

## ns-rse 2026-06-15 - hack to load the extract_ring() function, need to work out how to get package installed in renv
## so we can use `lottie::extract_ring()`
## source("utils.R")
