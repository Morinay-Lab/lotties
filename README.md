# Lotties - Data capture and management for Long Tailed Tits Study

This repository contains code for developing a [Shiny][shiny] web application for recording data in the Long Tailed Tit
longitudinal study in the Rivelin Valley on the outskirts of Sheffield, South Yorkshire.

## Overview

The overarching goal of the repository is to develop a unified web-based interface for recording, validating, cleaning
and storing data and the subsequent network analysis of interactions.

### Technologies

We leverage the [Shiny][shiny] package to capture data and store it in an [SQLite][sqlite] database (using the
[RSQLite][rsqlite] package).

## Cloning

To clone this repository locally you can use either of the following commands (the second will work if you have your
GitHub account setup with [SSH][gh_ssh])

``` shell
git clone https://github.com/Morinay-Lab/lotties.git
git clone git@github.com:Morinay-Lab/lotties.git
```

Or if you use a Git client such as Git Kraken you can use that to clone the repository.

## Usage

Data should be placed in `R/data/` with directories for each file type (i.e. all `.xls` files should be placed in the
`R/data/xls/` directory). GPX files in `.csv` format should be placed under `data/csv/gpx/`. Any `.gpx` files should
reside under `data/gpx/`.

### Shiny

You can test the Shiny package locally by navigating to the `lottie/R/` directory within the cloned repository from within [R][r] and running
`shiny::runApp()`.

``` r
setwd("~/path/to/cloned/repository/lotties/lottie/R")
shiny::runApp()
```

A new tab should open in your default browser showing the Shiny User Interface.

By default `testing <- TRUE` and so no data is required. SQLite databases are created on the fly in memory for testing
functionality. If you wish to deploy the application you should edit `lottie/R/server.R` and modify line 9 to set this
to `FALSE` and if necessary update the `db_path` location, by default it is `../data/sqlite/lottie.sql` (see below).

``` r
## testing <- TRUE
testing <- FALSE
## If testing we load the database in memory with this data.
if (testing) {
    db_path <- ":memory:"
    ## source("clean.R")
} else {
    ## ...otherwise we have a database on disc and load it.
    db_path <- "../data/sqlite/lottie.sql"
}
```

#### Option

Useful options which can be set in your global or the project  `.Rprofile` are shown below. They set a consistent port
for viewing the web-page locally (`shiny.port`), increase verbosity of the running server to help debugging
(`shiny.trace`) and watch for changes in the underlying files and reload the site when changes to files are saved
(`shiny.autoreload`). For more options see the [documentation][shiny_options].

``` r
## Shiny options
options(shiny.autoreload = TRUE,
        shiny.port = 8878,
        shiny.trace = TRUE)
```
## Resources

Useful resources for R packages and Shiny development.

- [R Packages][rpackages]
- [devtools][devtools]
- [usethis][usethis]
- [Shiny tutorial][shinytutorial]
- [Shiny function reference][shinyreference]
- [Mastering Shiny][masteringshiny]
- [Shiny with Databases][shinydatabases]

[devtools]: https://devtools.r-lib.org/index.html
[gh_ssh]: https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent
[masteringshiny]: https://mastering-shiny.org/
[r]: https://www.r-project.org/
[rpackages]: https://r-pkgs.org/
[rsqlite]: https://rsqlite.r-dbi.org/
[shiny]: https://shiny.posit.co/
[shiny_options]: https://shiny.posit.co/r/reference/shiny/1.0.3/shiny-options.html
[shinydatabases]: https://posit.co/blog/shiny-with-databases
[shinyreference]: https://shiny.posit.co/r/reference/shiny/latest/
[shinytutorial]: https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/
[sqlite]: https://sqlite.org/index.html
[usethis]: https://usethis.r-lib.org/index.html
