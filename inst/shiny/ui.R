library(bsicons)
library(bslib)
library(shiny)
library(shinyTime)

ui <- bslib::page_sidebar(
    title = shiny::h1("Lottie - Long-tailed Tit Data Capture"),
    sidebar = sidebar_accordion(),
    ## This allows use of shinyjs::reset() to reset fields on submission
    shinyjs::useShinyjs(),
    bslib::navset_card_underline(
      bslib::nav_panel(
        "Observations",
        gps_card(),
        observation_metadata_card(),
        flock_card(),
        individual_card(),
        interaction_card(),
        submit_card()
      ),
      bslib::nav_panel(
        "Download",
        download_card()
      )
    )
)
