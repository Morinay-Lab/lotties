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

#' Simple card
#'
#' @param header Card header
#' @param ... Card contents
#'
#' @returns bslib::card()
#'
#' @export
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
#'
#' @export
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
#'
#' @export
display_section <- function(header, ...) {
  bslib::card(
    full_screen = TRUE,
    fill = FALSE,
    bslib::card_header(header),
    bslib::card_body(...)
  )
}

#' GPS card for uploading GPX file.
#'
#' @returns List of Shiny input elements for uploading GPX files.
#'
#' @export
gps_inputs <- function(...) {
  list(
    shiny::helpText(
      "Upload a GPX file corresponding to the observations you will be entering under the Flock Composition,
      Description and Interactions tab."
    ),
    shiny::br(),
    shiny::fileInput(
      "gpx",
      label = "Choose GPX File",
      multiple = FALSE,
      accept = c(".gpx")
    ),
    shiny::checkboxInput(
      "gpx_complete", "GPS data accurate?", value = TRUE
    )
  )
}

#' GPS card for displaying information from uploaded GPX file.
#'
#' @export
gps_card <- function(...) {
  display_section(
    header = "GPS Data",
    shiny::p("Uploaded GPS file :"),
    shiny::tableOutput("gps_file_table")
  )
}

#' Observation metadata
#'
#' @export
observation_metadata <- function(...) {
  list(
  bslib::value_box(
      title = "User",
      value = shiny::textOutput("observation_user"),
      showcase = bsicons::bs_icon("person-circle"),
      theme = "purple"
    ),
    bslib::value_box(
      title = "Date",
      value = shiny::textOutput("observation_date"),
      showcase = bsicons::bs_icon("calendar"),
      theme = "teal"
    ),
    bslib::value_box(
      title = "Start Time",
      value = shiny::textOutput("observation_start_time"),
      showcase = bsicons::bs_icon("clock"),
      theme = "pink"
    ),
    bslib::value_box(
      title = "End Time",
      value = shiny::textOutput("observation_end_time"),
      showcase = bsicons::bs_icon("clock-fill"),
      theme = "blue"
    ),
    bslib::value_box(
      title = "Weather",
      value = shiny::textOutput("observation_weather"),
      showcase = bsicons::bs_icon("cloud-sun"),
      theme = "orange"
    ),
    bslib::value_box(
      title = "Visibility",
      value = shiny::textOutput("observation_visibility"),
      showcase = bsicons::bs_icon("eye"),
      theme = "yellow"
    )
  )
}

#' Observation metadata card
#'
#' @returns A display section for observation metadata
#'
#' @export
observation_metadata_card <- function() {
  display_section(
      header = "Observation Metadata",
      bslib::layout_column_wrap(
        width = 1/2,
        !!!observation_metadata
      )
  )
}

#' Individual flock member card
#'
#' @returns Individual flock member card.
#'
#' @export
individual_inputs <- function(...) {
  list(
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
          label = "Time : ",
          seconds = FALSE,
          value = Sys.time()
        )
      )
    ),
    shiny::selectInput(
       "composition_flock_number",
       label = "Flock Number : ",
       selected = NULL,
       choices = seq(1, 20)
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
          choices = c("Yes" = TRUE, "No" = FALSE)
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
                  colour_ring_inputs(position = "lt", selected = "None"),
                  colour_ring_inputs(position = "lb", selected = "None")
                  ),
      simple_card("Right Leg...",
                  colour_ring_inputs(position = "rt", selected = "None"),
                  colour_ring_inputs(position = "rb", selected = "None")
                  ),
      fill = FALSE
    ),
    shiny::textInput("composition_notes",
                     label = "Notes : ",
                     value = "",
                     updateOn = "blur"),
    shiny::actionButton("add_composition", label = "Submit bird description")
  )
}

#' Individual Card
#'
#' @returns
#'
#' @export
individual_card <- function(...) {
  display_section(
    header = "Flock Composition",
    shiny::tableOutput("composition"),
    shiny::helpText("NB - Duplicate observations of ringed birds in the above table will be removed on submission.")
  )
}

#' Flock description card
#'
#' @returns List of Shiny input elements for entering flock description.
#'
#' @export
flock_inputs <- function(...) {
  list(
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
        min = 1,
        max = 60,
        value = 12,
        step = 1
      ),
      shiny::numericInput(
        "description_n_ringed",
        label = "Number of Ringed Birds : ",
        min = 1,
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
}

#' Flock display card.
#'
#' @returns Display section for flock data entered.
#'
#' @export
flock_card <- function(...) {
  display_section(
    header = "Flocks",
    shiny::tableOutput("description"),
    shiny::helpText("NB - Duplicate observations in the above table will be removed on submission.")
  )
}

#' Submit data to database display card.
#'
#' @returns Display section for submitting data to database.
#'
#' @export
submit_card <- display_section(
    header = "Submit data",
    shiny::helpText("When you have entered all data and are ready to submit it to the database click on the button below."),
    shiny::actionButton("submit_all", label = "Submit all data")
)

#' Flock interaction card
#'
#' @returns
#'
#' @export
interaction_inputs <- function(...) {
  list(
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
      shiny::selectInput(
        "interactions_flock_a",
        label = "Flock A (numeric ID) : ",
         selected = NULL,
         choices = seq(1, 20)
      ),
      shiny::selectInput(
        "interactions_flock_b",
        label = "Flock B (numeric ID) : ",
         selected = NULL,
         choices = seq(1, 20)
      ),
    ),
    shiny::checkboxGroupInput("interactions_type",
                       label = "Type of Interaction : ",
                       selected = "a_chasing_b",
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
    shiny::helpText("NB - Duplicate observations in the above table will be removed on submission.")
  )
}

#' Download card.
#'
#' @returns
#'
#' @export
download_card <- function(...) {
  bslib::layout_column_wrap(
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
}

#' Metadata observations inputs.
#'
#' @returns Metadata observations inputs.
#'
#' @export
metadata_observations_inputs <- function(...) {
  list(
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
  )
}

#' Sidebar accordian
#'
#' @returns A sidebar accordian for the various elements of data entry.
#'
#' @export
sidebar_accordion <- function() {
  bslib::sidebar(
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
        !!!metadata_observations_inputs
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
}
