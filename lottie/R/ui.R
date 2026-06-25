library(bsicons)
library(bslib)
library(shiny)
library(shinyTime)

options(shiny.autoreload = TRUE, shiny.trace = TRUE)
## Notes
##
## Using the bslib package to aid with layout https://rstudio.github.io/bslib/index.html
##
## Nomenclature
##
## For the 'id' parameter to each input I have opted to use a prefix for each card that indicates the table to which a
## given variable belongs. This avoids confusion when there are similar fields collected across tables
## (e.g. date/time/notes).
##
## | Table              | card_header         | Prefix              |
## |--------------------|---------------------|---------------------|
## | survey conditions  | Survey Conditions   | conditions_         |
## | flock composition  | Flock Composition   | composition_        |
## | flock description  | Flock Description   | description_        |
## | flock interactions | Flock Interactions  | interactions_       |

## ns-rse ToDo
##
## 1. Ensure all inputs are unique, particularly dates/times across files
## 2. Can we reuse sections or partially submit some input?

## Load lookups
source("lookups.R")

#' Simple card
#'
#' @param header Card header
#' @param ... Card contents
#'
#' @returns bslib::card()
simple_card <- function(header, ...) {
  bslib::card(
    bslib::card_header(header, class = "bg-dark"),
    bslib::card_body(..., min_height = 150)
  )
}

#' Colour ring inputs
#'
#' @param position str Position, one of "lt", "lb", "rt", or "rb"
#' @param selected str Ring that is selected, initially blank until the ``Colour Ring`` has been selected.
#'
#' @returns List containing colour ring select and checkbox
colour_ring_inputs <- function(position, selected, ...) {
## colour_ring_inputs <- function(position, ...) {
  pos_vars <- switch(
    position,
    lt = list(title = "Top Ring", tag = "left_top"),
    lb = list(title = "Bottom Ring", tag = "left_bottom"),
    rt = list(title = "Top Ring", tag = "right_top"),
    rb = list(title = "Bottom Ring", tag = "right_bottom")
  )
  list(
    shiny::selectInput(
      paste("composition", pos_vars$tag, sep = "_"),
      label = paste(pos_vars$title, ":"),
      selected = selected,
      choices = split(rings_df$code, rings_df$description)
    ),
    shiny::checkboxInput(
      paste("composition", pos_vars$tag, "certain", sep = "_"),
      label = paste(pos_vars$title, "Certain")
    )
  )
}

#' Display section
#'
#' Create a UI display section from the passed in objects.
#'
#' @param header Card header
#' @param ... Card contents
#'
#' @returns bslib::card()
display_section <- function(header, ...) {
  bslib::card(
    full_screen = TRUE,
    fill = FALSE,
    bslib::card_header(header),
    bslib::card_body(...)
  )
}

## GPS card
gps_inputs <- list(
  ## Upload GPX file
  shiny::helpText(
    "Upload a GPX file corresponding to the observations you will be entering under the Flock Composition,
    Description and Interactions tab."
  ),
  shiny::br(),
  shiny::fileInput(
    "gpx",
    "Choose GPX File",
    multiple = FALSE,
    accept = c(".gpx")
  ),
  shiny::checkboxInput(
    "gpx_complete", "GPS data accurate?", value = TRUE
  )
)

gps_card <- display_section(
  header = "GPS Data",
  shiny::p("Uploaded GPS file :"),
  shiny::tableOutput("gps_file_table")
)

## Observation metadata card
conditions_card <- display_section(
    header = "Observation Metadata",
    shiny::tableOutput("conditions")
)
## Individual flock member card
individual_inputs <- list(
  bslib::layout_column_wrap(
    shiny::dateInput(
      "composition_date",
      label = "Date : ",
      format = "yyyy-mm-dd"
    ),
    shiny::div(
      class = "shiny-input-container bslib-gap-spacing",
      shinyTime::timeInput(
        "composition_time",
        "Time : ",
        seconds = FALSE,
        value = Sys.time()
      )
    )
  ),
  shiny::numericInput(
    "composition_flock_number",
    label = "Flock Number : ",
    min = 1,
    max = 60,
    value = 1,
    step = 1
  ),
  bslib::card_title("Rings"),
  shiny::helpText(
      "For details on reading rings see the document",
      tags$a(
          href="https://docs.google.com/document/d/10JnL50Fm5DneEl9PgqYLbxUDLo6wcnaUbGvY6e3j8Oc/edit?tab=t.0",
          "Reading Colour Rings"),
      "."),
  shiny::br(),
  bslib::layout_column_wrap(
    list(
      shiny::selectInput(
        "composition_ringed",
        label = "Ringed : ",
        selected = "Yes",
        choices = c("Yes" = "yes", "No" = "no")
      ),
      shiny::selectInput(
        "composition_bto_ring_position",
        label = "BTO Ring Position : ",
        selected = "None",
        choices = c("None" = NA, "Left" = "L", "Right" = "R")
      )
    ),
    list(
      shiny::selectInput(
        "composition_colour_ring",
        label = "Colour Ring : ",
        selected = "None",
        choices = colour_ring_df$code
      ),
      shiny::checkboxInput(
        "composition_certain",
        label = "Certain? ",
        value = FALSE
      )
    )
  ),
  bslib::layout_column_wrap(
    simple_card("Left Leg...",
                colour_ring_inputs(position = "lt", selected = ""),
                colour_ring_inputs(position = "lb", selected = "")
                ),
    simple_card("Right Leg...",
                colour_ring_inputs(position = "rt", selected = ""),
                colour_ring_inputs(position = "rb", selected = "")
                ),
    fill = FALSE
  ),
  shiny::textInput("composition_notes",
                   label = "Notes : ",
                   value = "",
                   updateOn = "blur"),
  shiny::actionButton("add_composition", label = "Submit bird description")
)

individual_card <- display_section(
  header = "Flock Composition",
  shiny::tableOutput("composition"),
  shiny::helpText("When you have added all individuals submit your data. NB - Duplicate observations in the above table will be removed on submission."),
  shiny::actionButton("submit_composition", label = "Submit all composition data")
)

## Flock description card
flock_inputs <- list(
  shiny::dateInput("description_date",
                   label = "Date : ",
                   format = "yyyy-mm-dd"
  ),
  bslib::layout_column_wrap(
    shiny::div(
      class = "shiny-input-container bslib-gap-spacing",
      shinyTime::timeInput("description_start_time",
                           label = "Time First Seen : ",
                           seconds = FALSE,
                           value = Sys.time()
      )
    ),
    shiny::div(
      class = "shiny-input-container bslib-gap-spacing",
      shinyTime::timeInput("description_end_time",
                           label = "Time Lost / Left : ",
                           seconds = FALSE,
                           value = Sys.time()
      )
    )
  ),
  bslib::layout_column_wrap(
    shiny::numericInput(
      "description_flock_number",
      label = "Flock Number : ",
      min = 1,
      max = 60,
      value = 1,
      step = 1
    ),
    shiny::selectInput(
      "description_flock_type",
      label =  "Flock Type : ",
      selected = NULL,
      choices = c("Flock" = "flock",
                  "Pair" = "pair",
                  "Individual" = "individual")
    )
  ),
  shiny::checkboxInput(
    "description_whole_flock",
    label = "Whole flock identified?"
  ),
  shiny::checkboxInput(
    "description_mist_net",
    label = "Flock captured in mist net?",
    value = FALSE
  ),
  bslib::layout_column_wrap(
    shiny::numericInput(
      "description_n_flock",
      label = "Flock Size : ",
      min = 0,
      max = 60,
      value = 12,
      step = 1
    ),
    shiny::numericInput(
      "description_n_ringed",
      label = "Number of Ringed Birds : ",
      min = 0,
      max = 60,
      value = 12,
      step = 1
    )
  ),
  shiny::checkboxGroupInput("description_other_species",
                            label = "Other Species : ",
                            selected = "none",
                            choices = split(other_species_df$code,
                                            other_species_df$description)),
  shiny::selectInput("description_section",
                     label = "Section : ",
                     selected = NULL,
                     choices = split(section_df$code, section_df$description)),
  shiny::textInput("description_notes",
                   label = "Notes : ",
                   value = "",
                   updateOn = "blur"),
  shiny::actionButton("add_description", label = "Submit flock description")
)

flock_card <- display_section(
  header = "Flocks",
  shiny::tableOutput("description"),
  shiny::helpText("When you have described all flocks submit your data. NB - Duplicate observations in the above table will be removed on submission."),
  shiny::actionButton("submit_description", label = "Submit all flock data")
)

## Flock interaction card
interaction_inputs <- list(
  bslib::layout_column_wrap(
    shiny::dateInput(
      "interactions_date",
      label = "Date : ",
      format = "yyyy-mm-dd"
    ),
    shiny::div(
      class = "shiny-input-container bslib-gap-spacing",
      shinyTime::timeInput(
        "interactions_time",
        label = "Time : ",
        seconds = FALSE,
        value = Sys.time()
      )
    )
  ),
  bslib::layout_column_wrap(
    shiny::numericInput(
      "interactions_flock_a",
      label = "Flock A (numeric ID) : ",
      min = 0,
      max = 300,
      value = 0,
      step = 1
    ),
    shiny::numericInput(
      "interactions_flock_b",
      label = "Flock B (numeric ID) : ",
      min = 0,
      max = 300,
      value = 0,
      step = 1
    )
  ),
  shiny::checkboxGroupInput("interactions_type",
                     label = "Type of Interaction : ",
                     choices = split(interactions_df$code,
                                     interactions_df$description)),
  shiny::textInput("interactions_notes",
                   label = "Notes : ",
                   value = "",
                   updateOn = "blur"),
  shiny::actionButton("add_interactions", label = "Submit interaction")
)

interaction_card <- display_section(
  header = "Flock Interactions",
  shiny::tableOutput("interactions"),
  shiny::helpText("When you have added all interactions submit your data. NB - Duplicate observations in the above table will be removed on submission."),
  shiny::actionButton("submit_interactions", label = "Submit all interaction data")
)

## Download card
download_card <- bslib::layout_column_wrap(
  width = 1/2,
  bslib::card(
    bslib::card_header("Raw data"),
    bslib::card_body(
      shiny::helpText(
        "Please select the", shiny::em("raw"), "data tables you wish to extract.",
        "Files will be extracted to CSV and compressed into a single .zip file."
      ),
      shiny::checkboxGroupInput(
        "download_raw_data_selection",
        label = "",
        choices = c(
          "Conditions" = "Conditions",
          "Flock Composition" = "Composition",
          "Flock Description" = "Description",
          "Flock Interactions" = "Interactions",
          "GPS" = "GPS"),
        selected = c("Conditions", "Composition", "Description", "Interactions", "GPS")
      ),
      shiny::downloadButton("download_raw_data", label = "Download raw data"),
    )
  ),
  bslib::card(
    bslib::card_header("Cleaned data"),
    bslib::card_body(
      shiny::helpText(
        "Please select the", shiny::em("cleaned"), "data tables you wish to extract.",
        "Files will be extracted to CSV and compressed into a single .zip file."
      ),
      shiny::checkboxGroupInput(
        "download_clean_data_selection",
        label = "",
        choices = c("GPS" = "GPS"),
        selected = c("GPS")
      ),
      shiny::downloadButton("download_clean_data", label = "Download cleaned data")
    )
  )
)

conditions_inputs <- list(
  shiny::selectInput("user",
                     label = "User",
                     choices = split(person_df$code,
                                     person_df$forename)),
  shiny::dateInput("conditions_date",
                   "Date : ",
                   format = "yyyy-mm-dd"),
  bslib::layout_column_wrap(
    shiny::div(
      class = "shiny-input-container bslib-gap-spacing",
      shinyTime::timeInput(
        "conditions_start_time",
        label = "Start Time : ",
        seconds = FALSE,
        value = Sys.time()
      )
    ),
    shiny::div(
      class = "shiny-input-container bslib-gap-spacing",
      shinyTime::timeInput(
        "conditions_end_time",
        label = "End Time : ",
        seconds = FALSE,
        value = Sys.time()
      )
    )
  ),
  bslib::layout_column_wrap(
    shiny::checkboxGroupInput(
      "conditions_weather",
      label="Weather : ",
      selected = "sunny",
      choices = split(conditions_df$code,
                      conditions_df$description)
    ),
    shiny::selectInput(
      "conditions_visibility",
      label = "Visibility : ",
      choices = split(visibility_df$code,
                      visibility_df$description)
    )
  ),
  shiny::actionButton("submit_conditions", label = "Submit metadata")
  ## Record user
  ## shiny::selectInput("user",
  ##                    label = "User",
  ##                    choices = split(person_df$code,
  ##                                    person_df$forename)),
)

sidebar_accordion <- bslib::sidebar(
  width = "33%",
  shiny::helpText("Enter your flock observation data here. Data will be displayed in the Observations tab as you add it.",
                  "When you have finished adding all of your data you can submit your observations to the database."),
  bslib::accordion(
    multiple = FALSE,
    bslib::accordion_panel(
      "GPS data", icon = bsicons::bs_icon("geo-alt"),
      !!!gps_inputs
    ),
    bslib::accordion_panel(
      "Observation metadata", icon = bsicons::bs_icon("sun"),
      !!!conditions_inputs
    ),
    bslib::accordion_panel(
      "Flock data", icon = bsicons::bs_icon("sliders"),
      !!!flock_inputs
    ),
    bslib::accordion_panel(
      "Individual data", icon = bsicons::bs_icon("sliders"),
      !!!individual_inputs
    ),
    bslib::accordion_panel(
      "Interaction data", icon = bsicons::bs_icon("sliders"),
      !!!interaction_inputs
    )
  )
)

sidebar_orig <- bslib::sidebar(
  title = "Conditions",
  !!!conditions_inputs
)

ui <- bslib::page_sidebar(
    title = shiny::h1("Lottie - Long-tailed Tit Data Capture"),
    sidebar = sidebar_accordion,
    ## ns-rse : This is using non-standard evaluation but I think its an old form, newer methods are lazyeval
    ## (https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html)
    ## ns-rse 2026-05-12 : Consider switching to column-layout
    ## https://rstudio.github.io/bslib/articles/column-layout/index.html
    ## bslib::layout_column_wrap(
    ##    width = "300px",
    ##    height = 300,
    ##    fixed_width = FALSE,
    ##    heights_equal = "all",
    # bslib::navset_card_underline(
    #     title = "Flock Observations...",
    #     bslib::nav_panel("GPS", gps_card),
    #     bslib::nav_panel("Description", flock_card),
    #     bslib::nav_panel("Composition", individual_card),
    #     bslib::nav_panel("Interactions", interaction_card),
    #     bslib::nav_panel("Download", download_card)
    #     )
    bslib::navset_card_underline(
      bslib::nav_panel(
        "Observations",
        gps_card,
        conditions_card,
        flock_card,
        individual_card,
        interaction_card
      ),
      bslib::nav_panel(
        "Download",
        download_card
      )
    )
)
