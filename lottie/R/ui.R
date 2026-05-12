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
## 1. Replace times with shinyTime (https://burgerga.github.io/shinyTime/)
## 2. Ensure all inputs are unique, particularly dates/times across files
## 3. Can we reuse sections or partially submit some input?
## 4. Populate 'choices' from lookup tables in database

## Setup cards
cards <- list(
    bslib::card(
        full_screen = TRUE,
        card_header = "Survey Conditions",
        bslib::card_body(
                   shiny::dateInput("conditions_date",
                                    "Date : ",
                                    format = "yyyy-mm-dd"),
                   shinyTime::timeInput("conditions_start_time",
                                        "Start Time : ",
                                        seconds = FALSE,
                                        value = Sys.time()),
                   shinyTime::timeInput("conditions_end_time",
                                        "End Time : ",
                                        seconds = FALSE,
                                        value = Sys.time()),
                   ## Alternatively we can have sliders for hours/minutes
                   ## shiny::sliderInput("conditions_hour",
                   ##                    "Hour:",
                   ##                    min = 0,
                   ##                    max = 23,
                   ##                    value = 12,
                   ##                    step = 1),
                   ## shiny::sliderInput("conditions_mins",
                   ##                    "Minutes:",
                   ##                    min = 0,
                   ##                    max = 59,
                   ##                    value = 0,
                   ##                    step = 1),
                   shiny::checkboxGroupInput("conditions_weather",
                                             label="Weather : ",
                                             choices=c("Sunny" = "sunny",
                                                       "Partly cloudy" = "partly cloudy",
                                                       "Cloudy/ grey" = "cloudy/ grey",
                                                       "Foggy" = "foggy",
                                                       "Windy" = "windy",
                                                       "Light rain" = "light rain",
                                                       "Really rainy" = "really rainy")),
                   shiny::selectInput("conditions_visibility",
                                      label="Visibility : ",
                                      choices=c("Perfect" = "perfect",
                                                "Good" = "good",
                                                "Average" = "average",
                                                "Mediocre" = "mediocre"))
               )),
    bslib::card(
        full_screen = FALSE,
        card_header = "Flock Composition",
        bslib::card_body(
                   shiny::dateInput("composition_date",
                                    "Date : ",
                                    format = "yyyy-mm-dd"),
                   shinyTime::timeInput("composition_time",
                                        "Time : ",
                                        seconds = FALSE,
                                        value = Sys.time()),
                   shiny::h4("Rings..."),
                   shiny::selectInput("composition_colour_ring",
                                      "Colour Ring : ",
                                      choices = c("GBL", "YmbL")),
                   shiny::selectInput("composition_ringed",
                                      "Ringed : ",
                                      choices = c("Yes" = "yes", "No" = "no")),
                   shiny::h4("Left Leg..."),
                   shiny::selectInput("composition_left_leg",
                                      "Left Leg : ",
                                      choices = c("GBL", "YmbL")),
                   shiny::selectInput("composition_left_certainty",
                                      "Certainty (Left) ",
                                      choices = c("Sure" = "sure", "Unsure" = "not")),
                   shiny::h4("Right Leg..."),
                   shiny::selectInput("composition_right_leg",
                                      "Right Leg : ",
                                      choices = c("GBL", "YmbL")),
                   shiny::selectInput("composition_right_certainty",
                                      "Certainty (Right)  : ",
                                      choices = c("Sure" = "sure", "Unsure" =
                                                                                                             "not")),
                   shiny::h4("Other..."),
                   shiny::selectInput("composition_bto_ring_position",
                                      "BTO Ring Position : ",
                                      choices = c("Left" = "left", "Right" = "right")),
                   shiny::textInput("composition_notes",
                                    "Notes : ",
                                    value = "",
                                    updateOn = "blur"),
                   shiny::actionButton("add_composition", "Submit bird description"),
                   shiny::tableOutput("composition")
                   ## shiny::tableOutput("composition_test")
           )),
    bslib::card(
        full_screen = FALSE,
        card_header = "Flock Description",
        bslib::card_body(
                  shiny::h4("First seen..."),
                  shiny::dateInput("description_date",
                      "Date : ",
                      format = "yyyy-mm-dd"
                  ),
                  shinyTime::timeInput("description_start_time",
                      "Time : ",
                      seconds = FALSE,
                      value = Sys.time()
                  ),
                  shinyTime::timeInput("description_end_time",
                      "Time : ",
                      seconds = FALSE,
                      value = Sys.time()
                  ),
                  shiny::h4("Flock features..."),
                  shiny::selectInput("description_flock_type",
                                      "Flock type : ",
                                      choices = c("In flock" = "flock",
                                                  "Pair" = "pair",
                                                  "Individual" = "individual")),
                   shiny::checkboxInput("description_whole_flock",
                                        "Whole flock identified?"),
                   shiny::numericInput("description_n_flock",
                                      "Flock Size : ",
                                      min = 0,
                                      max = 60,
                                      value = 12,
                                      step = 1),
                   shiny::numericInput("description_n_ringed",
                                      "Ringed Birds : ",
                                      min = 0,
                                      max = 60,
                                      value = 12,
                                      step = 1),
                   shiny::checkboxGroupInput("description_other_species",
                                             "Other Species : ",
                                  choices = c("Blue Tit" = "BT",
                                              "NH" = "NH")),
                   shiny::selectInput("description_section",
                                      "Section : ",
                                      choices = c("Rivelin" = "RV",
                                                  "FH" = "FH",
                                                  "NH" = "NH",
                                                  "BB" = "BB")),
                   shiny::textInput("description_notes",
                                    "Notes : ",
                                    value = "",
                                    updateOn = "blur"),
                   shiny::actionButton("add_description", "Submit flock description"),
                   shiny::tableOutput("description")
           )),
    bslib::card(
        full_screen = FALSE,
        card_header = "Flock Interactions",
        bslib::card_body(
                   shiny::dateInput("interactions_date",
                                    "Date : ",
                                    format = "yyyy-mm-dd"),
                   shinyTime::timeInput("interactions_time",
                                        "Time : ",
                                        seconds = FALSE,
                                        value = Sys.time()),
                   shiny::numericInput("interactions_flock_a",
                                      "Flock A (numeric ID) : ",
                                      min = 0,
                                      max = 300,
                                      value = 0,
                                      step = 1),
                   shiny::numericInput("interactions_flock_b",
                                      "Flock B (numeric ID) : ",
                                      min = 0,
                                      max = 300,
                                      value = 0,
                                      step = 1),
                   shiny::checkboxGroupInput("interactions_type",
                                             "Type of Interaction : ",
                                             choices = c(
                                                 "Foraging together" = "foraging together",
                                                 "A chasing B" = "A chasing B",
                                                 "B chasing A" = "B chasing A",
                                                 "Near but not interacting" = "being close but not interacting",
                                                 "Other (see notes)" = "other - see comments")),
                   shiny::textInput("interactions_notes",
                                    "Notes : ",
                                    value = "",
                                    updateOn = "blur"),
                   shiny::actionButton("add_interactions", "Submit interaction"),
                   shiny::tableOutput("interactions")
           ))
)

data <- list(
    bslib::card(full_screen = TRUE, card_header = "Flock Composition"),
    bslib::card(full_screen = TRUE, card_header = "Flock Description"),
    bslib::card(full_screen = TRUE, card_header = "Flock Interactions")
)
ui <- bslib::page_sidebar(
    title = shiny::h1("Lottie - Long-tailed Tit Data Capture"),
    sidebar = bslib::sidebar(
        title = "Conditions",
        ## Record user
        shiny::selectInput("user", label = "User", choices = c("SB", "LN", "MJ", "ND")),
        ## Upload GPX file
        shiny::fileInput(
            "gpx",
            "Choose GPX File(s)",
            multiple = TRUE, accept = c(".gpx")
        ),
        cards[[1]]
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
        bslib::nav_panel("Composition", cards[[2]]),
        bslib::nav_panel("Description", cards[[3]]),
        bslib::nav_panel("Interactions", cards[[4]])
        )
    ## Attempt to show results as separate cards underneath input panels
    ## bslib::navset_card_underline(
    ##     bslib::nav_panel("Composition", data[[1]]),
    ##     bslib::nav_panel("Description", data[[2]]),
    ##     bslib::nav_panel("Interactions", data[[3]])
    ## )
)
