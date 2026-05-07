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
                   shinyTime::timeInput("conditions_time",
                                        "Time : ",
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
                                    updateOn = "blur")
           )),
    bslib::card(
        full_screen = TRUE,
        card_header = "Flock Description",
        bslib::card_body(
                   shiny::h4("First seen..."),
                   shiny::dateInput("description_date",
                                    "Date : ",
                                    format = "yyyy-mm-dd"),
                   shinyTime::timeInput("description_time_first_seen",
                                        "Time : ",
                                        seconds = FALSE,
                                        value = Sys.time()),
                   ## shiny::sliderInput("description_hour_first_seen",
                   ##                    "Hour:",
                   ##                    min = 0,
                   ##                    max = 23,
                   ##                    value = 12,
                   ##                    step = 1),
                   ## shiny::sliderInput("description_mins_first_seen",
                   ##                    "Minutes:",
                   ##                    min = 0,
                   ##                    max = 59,
                   ##                    value = 0,
                   ##                    step = 1),
                   shiny::h4("Flock features..."),
                   shiny::checkboxInput("description_in_flock",
                                        "In flock?"),
                   shiny::checkboxInput("description_whole_flock",
                                        "Whole flock?"),
                   shiny::sliderInput("description_n_flock",
                                      "Flock Size : ",
                                      min = 0,
                                      max = 60,
                                      value = 12,
                                      step = 1),
                   shiny::sliderInput("description_n_ringed",
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
                   shiny::h4("Last seen..."),
                   shinyTime::timeInput("description_time_last_seen",
                                        "Time : ",
                                        seconds = FALSE,
                                        value = Sys.time())
                   ## shiny::sliderInput("description_hour_last_seen",
                   ##                    "Hour:",
                   ##                    min = 0,
                   ##                    max = 23,
                   ##                    value = 12,
                   ##                    step = 1),
                   ## shiny::sliderInput("description_mins_last_seen",
                   ##                    "Minutes:",
                   ##                    min = 0,
                   ##                    max = 59,
                   ##                    value = 0,
                   ##                    step = 1)
           )),
    bslib::card(
        full_screen = TRUE,
        card_header = "Flock Interactions",
        bslib::card_body(
                   shiny::dateInput("interactions_date",
                                    "Date : ",
                                    format = "yyyy-mm-dd"),
                   shinyTime::timeInput("interactions_time",
                                        "Time : ",
                                        seconds = FALSE,
                                        value = Sys.time()),
                   ## shiny::sliderInput("interactions_hour",
                   ##                    "Hour:",
                   ##                    min = 0,
                   ##                    max = 23,
                   ##                    value = 12,
                   ##                    step = 1),
                   ## shiny::sliderInput("interactions_mins",
                   ##                    "Minutes:",
                   ##                    min = 0,
                   ##                    max = 59,
                   ##                    value = 0,
                   ##                    step = 1),
                   shiny::sliderInput("flock_a",
                                      "Flock A (numeric ID) : ",
                                      min = 0,
                                      max = 300,
                                      value = 0,
                                      step = 1),
                   shiny::sliderInput("flock_b",
                                      "Flock B (numeric ID) : ",
                                      min = 0,
                                      max = 300,
                                      value = 0,
                                      step = 1),
                   shiny::sliderInput("flock_c",
                                      "Flock C (numeric ID) : ",
                                      min = 0,
                                      max = 300,
                                      value = 0,
                                      step = 1),
                   shiny::checkboxGroupInput("type_of_interaction",
                                             "Type of Interaction : ",
                                             choices = c(
                                                 "Foraging together" = "foraging together",
                                                 "A chasing B" = "A chasing B",
                                                 "A chasing C" = "A chasing C",
                                                 "B chasing C" = "B chasing C",
                                                 "Near but not interacting" = "being close but not interacting",
                                                 "Other (see notes)" = "other - see comments")),
                   shiny::textInput("notes",
                                    "Notes : ",
                                    value = "",
                                    updateOn = "blur"),
           ))
)
ui <- bslib::page_sidebar(
    title = shiny::h1("Lottie - Long-tailed Tit Data Capture"),
    sidebar = bslib::sidebar(
                         title = "Conditions",
                         ## Record user
                         shiny::selectInput("user", label="User", choices=c("SB", "LN", "MJ", "ND")),
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
    bslib::navset_card_underline(
               title = "Data Entry",
               bslib::nav_panel("Flock Composition", cards[[2]]),
               bslib::nav_panel("Flock Description", cards[[3]]),
               bslib::nav_panel("Flock Interactions", cards[[4]])
           )
    )
