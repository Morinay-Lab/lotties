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

## Setup ring cards
## https://pkgs.rstudio.com/bslib/articles/filling/index.html
##
## @ns-rse 2026-06-02 : Not sure it is possible to have nested page_fillable() items so this may be a blind alley
## plot_card <- function(header, ...) {
##   card(
##     full_screen = TRUE,
##     card_header(header, class = "bg-dark"),
##     card_body(..., min_height = 150)
##   )
## }
## ring_cards <- list(
##     "left_top" = plot_card("Left Leg (Top)...",
##                            shiny::selectInput("composition_left_leg_top",
##                                               "Left Leg Top Ring : ",
##                                               selected = NULL,
##                                               choices = split(rings_df$code, rings_df$description)),
##                            shiny::checkboxInput("composition_left_leg_top_certain",
##                                                   "Left Leg Top Ring Certain")),
##     "left_bottom" = plot_card("Left Leg (Bottom)",
##                               shiny::selectInput("composition_left_leg_bottom",
##                                                  "Left Leg Bottom Ring : ",
##                                                  selected = NULL,
##                                                  choices = split(rings_df$description, rings_df$code)),
##                               shiny::checkboxInput("composition_left_leg_bottom_certain",
##                                                    "Left Leg Bottom Ring Certain")),
##     "right_top" = plot_card("Right Leg (Top)...",
##                            shiny::selectInput("composition_right_leg_top",
##                                               "Right Leg Top Ring : ",
##                                               selected = NULL,
##                                               choices = split(rings_df$code, rings_df$description)),
##                            shiny::checkboxInput("composition_right_leg_top_certain",
##                                                   "Right Leg Top Ring Certain")),
##     "right_bottom" = plot_card("Right Leg (Bottom)",
##                               shiny::selectInput("composition_right_leg_bottom",
##                                                  "Right Leg Bottom Ring : ",
##                                                  selected = NULL,
##                                                  choices = split(rings_df$description, rings_df$code)),
##                               shiny::checkboxInput("composition_right_leg_bottom_certain",
##                                                    "Right Leg Bottom Ring Certain"))
## )

## Setup cards
cards <- list(
    bslib::card(
        full_screen = TRUE,
        card_header = "Survey Conditions & GPS",
        bslib::card_body(
            ## Record user
            ## Upload GPX file
            shiny::h4("GPS Data"),
            shiny::p("You can only upload a single GPX file at a time. This should correspond to the observations you will be entering under the Flock Composition, Description and Interactions tab."),
            shiny::fileInput(
                "gpx",
                "Choose GPX File",
                multiple = FALSE,
                accept = c(".gpx")),
            shiny::p("Uploaded GPS file(s) :"),
            shiny::tableOutput("gps_file_table"),
            ## @ns-rse 2026-06-02 : Show the filename of an uploaded file here
            shiny::checkboxInput("gpx_complete", "GPS data accurate?", value = TRUE),
            )),
    bslib::card(
        full_screen = FALSE,
        card_header = "Flock Composition",
        bslib::card_body(
                   shiny::dateInput("composition_date",
                                    label = "Date : ",
                                    format = "yyyy-mm-dd"),
                   shinyTime::timeInput("composition_time",
                                        "Time : ",
                                        seconds = FALSE,
                                        value = Sys.time()),
                  shiny::numericInput("composition_flock_number",
                                      label = "Flock number",
                                      min = 1,
                                      max = 60,
                                      value = 1,
                                      step = 1),
                   shiny::h4("Rings..."),
                   shiny::selectInput("composition_ringed",
                                      label = "Ringed : ",
                                      selected = "Yes",
                                      choices = c("Yes" = "yes", "No" = "no")),
                   shiny::selectInput("composition_colour_ring",
                                      label = "Colour Ring : ",
                                      selected = "None",
                                      choices = colour_ring_df$code),
                   shiny::checkboxInput("composition_certain",
                                        label = "Certain? ",
                                        value = FALSE),
                   shiny::h5("Left Leg..."),
                   shiny::selectInput("composition_left_top",
                                      label = "Left Leg Top Ring : ",
                                      choices = split(rings_df$code, rings_df$description)),
                   shiny::checkboxInput("composition_left_top_certain",
                                        label = "Left Leg Top Ring Certain",
                                        value = FALSE),
                   shiny::selectInput("composition_left_bottom",
                                      label = "Left Leg Bottom Ring : ",
                                      choices = split(rings_df$code, rings_df$description)),
                   shiny::checkboxInput("composition_left_bottom_certain",
                                        label = "Left Leg Bottom Ring Certain",
                                        value = FALSE),
                   shiny::h5("Right Leg..."),
                   shiny::selectInput("composition_right_top",
                                      label = "Right Leg Top Ring : ",
                                      choices = split(rings_df$code, rings_df$description)),
                   shiny::checkboxInput("composition_right_top_certain",
                                        label = "Right Leg Top Ring Certain",
                                        value = FALSE),
                   shiny::selectInput("composition_right_bottom",
                                      label = "Right Leg Bottom Ring : ",
                                      choices = split(rings_df$code, rings_df$description)),
                   shiny::checkboxInput("composition_right_bottom_certain",
                                        label = "Right Leg Bottom Ring Certain",
                                        value = FALSE),
                   shiny::h4("Other..."),
                   shiny::selectInput("composition_bto_ring_position",
                                      label = "BTO Ring Position : ",
                                      selected = "None",
                                      choices = c("None" = NA, "Left" = "left", "Right" = "right")),
                   shiny::textInput("composition_notes",
                                    label = "Notes : ",
                                    value = "",
                                    updateOn = "blur"),
                   shiny::actionButton("add_composition", "Submit bird description"),
                   shiny::tableOutput("composition"),
                   shiny::p("When you have added all individuals submit your data."),
                   shiny::actionButton("submit_composition", "Submit all composition data")
           )),
    bslib::card(
        full_screen = FALSE,
        card_header = "Flock Description",
        bslib::card_body(
                  shiny::h4("First seen..."),
                  shiny::dateInput("description_date",
                      label = "Date : ",
                      format = "yyyy-mm-dd"
                  ),
                  shinyTime::timeInput("description_start_time",
                      label = "Time : ",
                      seconds = FALSE,
                      value = Sys.time()
                      ),
                  shinyTime::timeInput("description_end_time",
                      label = "Time : ",
                      seconds = FALSE,
                      value = Sys.time()
                  ),
                  shiny::h4("Flock features..."),
                  shiny::selectInput("description_flock_type",
                                     label =  "Flock type : ",
                                     selected = NULL,
                                     choices = c("In flock" = "flock",
                                                 "Pair" = "pair",
                                                 "Individual" = "individual")),
                  shiny::numericInput("description_flock_number",
                                      label = "Flock number",
                                      min = 1,
                                      max = 60,
                                      value = 1,
                                      step = 1),
                  shiny::checkboxInput("description_whole_flock",
                                       label = "Whole flock identified?"),
                  shiny::checkboxInput("description_mist_net",
                                       label = "Flock captured in mist net : ",
                                       value = FALSE),
                  shiny::numericInput("description_n_flock",
                                      label = "Flock Size : ",
                                      min = 0,
                                      max = 60,
                                      value = 12,
                                      step = 1),
                  shiny::numericInput("description_n_ringed",
                                      label = "Ringed Birds : ",
                                      min = 0,
                                      max = 60,
                                      value = 12,
                                      step = 1),
                  shiny::checkboxGroupInput("description_other_species",
                                            label = "Other Species : ",
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
                  shiny::actionButton("add_description", label = "Submit flock description"),
                  shiny::tableOutput("description"),
                  shiny::p("When you have described all flocks submit your data."),
                  shiny::actionButton("submit_description", label = "Submit all flock data")
    )),
    bslib::card(
        full_screen = FALSE,
        card_header = "Flock Interactions",
        bslib::card_body(
                   shiny::dateInput("interactions_date",
                                    label = "Date : ",
                                    format = "yyyy-mm-dd"),
                   shinyTime::timeInput("interactions_time",
                                        label = "Time : ",
                                        seconds = FALSE,
                                        value = Sys.time()),
                   shiny::numericInput("interactions_flock_a",
                                       label = "Flock A (numeric ID) : ",
                                       min = 0,
                                       max = 300,
                                       value = 0,
                                       step = 1),
                   shiny::numericInput("interactions_flock_b",
                                       label = "Flock B (numeric ID) : ",
                                       min = 0,
                                       max = 300,
                                       value = 0,
                                       step = 1),
                   shiny::selectInput("interactions_type",
                                      label = "Type of Interaction : ",
                                      choices = split(interactions_df$code,
                                                      interactions_df$description)),
                   shiny::textInput("interactions_notes",
                                    label = "Notes : ",
                                    value = "",
                                    updateOn = "blur"),
                   shiny::actionButton("add_interactions", "Submit interaction"),
                   shiny::tableOutput("interactions"),
                   shiny::p("When you have added all interactions submit your data."),
                   shiny::actionButton("submit_interactions", "Submit all interaction data")
               )),
    bslib::card(
               full_screen = FALSE,
               card_header = "Download Data",
               bslib::card_body(
                   shiny::h4("Raw data"),
                   shiny::p("Please select the raw data tables you wish to extract. Files will be extracted to CSV and compressed into a single .zip file."),
                   shiny::checkboxGroupInput("download_raw_data_selection",
                                             label = "Select raw data to download : ",
                                             choices = c("Flock Composition" = "Composition",
                                                         "Flock Description" = "Description",
                                                         "Flock Interactions" = "Interactions",
                                                         "GPS" = "GPS"),
                                             selected = c("Composition", "Description", "Interactions", "GPS")),
                   shiny::downloadButton("download_raw_data", "Download"),
                   shiny::h4("Cleaned data"),
                   shiny::p("Please select the cleaned data tables you wish to extract. Files will be extracted to CSV and compressed into a single .zip file."),
                   shiny::checkboxGroupInput("download_clean_data_selection",
                                             label = "Select clean data to download : ",
                                             choices = c("GPS" = "GPS"),
                                             selected = c("GPS")),
                   shiny::downloadButton("download_clean_data", "Download")
               ))
)

ui <- bslib::page_sidebar(
    title = shiny::h1("Lottie - Long-tailed Tit Data Capture"),
    sidebar = bslib::sidebar(
        title = "Conditions",
        shiny::selectInput("user",
                           label = "User",
                           choices = split(person_df$code,
                                           person_df$forename)),
        shiny::dateInput("conditions_date",
                         "Date : ",
                         format = "yyyy-mm-dd"),
        shinyTime::timeInput("conditions_start_time",
                             label = "Start Time : ",
                             seconds = FALSE,
                             value = Sys.time()),
        shinyTime::timeInput("conditions_end_time",
                             label = "End Time : ",
                             seconds = FALSE,
                             value = Sys.time()),
        shiny::checkboxGroupInput("conditions_weather",
                                  label="Weather : ",
                                  choices = split(conditions_df$code,
                                                  conditions_df$description)),
        shiny::selectInput("conditions_visibility",
                           label = "Visibility : ",
                           choices = split(visibility_df$code,
                                           visibility_df$description)),
        shiny::actionButton("submit_conditions", "Submit")        ## Record user
        ## shiny::selectInput("user",
        ##                    label = "User",
        ##                    choices = split(person_df$code,
        ##                                    person_df$forename)),
    ##     ## Upload GPX file
    ##     shiny::h4("GPS Data"),
    ##     shiny::p("You can upload multiple files but they can not be a mixture of accurate/complete and inaccurate/incomplete files, do not mix and match files."),
    ##     shiny::fileInput(
    ##         "gpx",
    ##         "Choose GPX File(s)",
    ##         multiple = TRUE,
    ##         accept = c(".gpx")
    ##         ),
    ##     shiny::p("Uploaded GPS file(s) :"),
    ##     shiny::textOutput("gps_filename"),
        ## @ns-rse 2026-06-02 : Show the filename of an uploaded file here
        ## shiny::checkboxInput("gpx_complete", "GPS data accurate?", value = TRUE),
        ## cards[[1]]
    ),
    ## ns-rse : This is using non-standard evaluation but I think its an old form, newer methods are lazyeval
    ## (https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html)
    ## ns-rse 2026-05-12 : Consider switching to column-layout
    ## https://rstudio.github.io/bslib/articles/column-layout/index.html
    ## bslib::layout_column_wrap(
    ##    width = "300px",
    ##    height = 300,
    ##    fixed_width = FALSE,
    ##    heights_equal = "all",
    bslib::navset_card_underline(
        title = "Flock Observations...",
        bslib::nav_panel("GPS", cards[[1]]),
        bslib::nav_panel("Composition", cards[[2]]),
        bslib::nav_panel("Description", cards[[3]]),
        bslib::nav_panel("Interactions", cards[[4]]),
        bslib::nav_panel("Download", cards[[5]])
        )
)
