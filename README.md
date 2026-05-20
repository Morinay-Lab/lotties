# Lotties - Data capture and management for Long Tailed Tits Study

This repository contains code for developing a [Shiny][shiny] web application for recording data in the Long Tailed Tit
longitudinal study in the Rivelin Valley on the outskirts of Sheffield, South Yorkshire.

## Overview

The overarching goal of the repository is to develop a unified web-based interface for recording, validating, cleaning
and storing data and the subsequent network analysis of interactions.

### Technologies

We leverage the [Shiny][shiny] package to capture data and store it in an [SQLite][sqlite] database (using the
[RSQLite][rsqlite] package).


## Usage

To deploy this server requires...

1. Running `lotties/R/clean.R` which will take existing data from Excel spreadsheets and CSV exported GPS files and
   populate a SQLite database with the resulting cleaned data.
2. Deploying the Shiny server from `lotties/R/` with `shiny::runApp()` which will start a WebUI on the local machine
   from where data can be input.

### Cleaning Existing data

The `lotties/R/clean*.R` files are adapted from the RMarkdown files developed and shared by @jennifmo. They require that
data files be placed into a specific location within the `lottie/data/` directory. There is a separate folder for each
file type...

- `lottie/data/xls` - for all `.xls[x]` files.
- `lottie/data/gpx` - for all `.gpx` files.
- `lottie/data/sqlite` - for all `.sqlite` files.
- `lottie/data/csv/` - for all `.csv` files.
- `lottie/data/csv/gps` - for all GPS data that has been exported to `.csv` files.

``` mermaid
[4.0K May  8 13:49]  data
├── [4.0K May  8 11:25]  data/csv
│   ├── [ 12K Apr 28 13:13]  data/csv/gps
│   │   ├── [ 16K Apr 27 11:17]  data/csv/gps/Track_2024-12-06_LN.csv
│   │   ├── [ 31K Apr 27 11:17]  data/csv/gps/Track_2024-12-09_LN.csv
│   │   ├── ...
│   │   ├── [ 34K Apr 28 13:13]  data/csv/gps/Track_2026-02-27_SJB.csv
│   │   └── [ 21K Apr 28 13:13]  data/csv/gps/Track_ 27-02-2026_NDR.csv
├── [ 12K May  8 13:49]  data/gpx
│   ├── [ 59K Jul 15  2025]  data/gpx/Track_2025-07-02_SJB.gpx
│   ├── [ 64K Jul  4  2025]  data/gpx/Track_2025-07-03_LN.gpx
│   ├── ...
│   ├── [ 55K Feb 27 07:41]  data/gpx/Track_2026-02-27_SJB.gpx
│   └── [ 35K Feb 27 06:59]  data/gpx/Track_ 27-02-2026_NDR.gpx
├── [4.0K May  5 14:08]  data/sqlite
└── [4.0K May  7 11:49]  data/xls
    ├── [140K Apr 27 11:13]  data/xls/LUKE_Flocks_and_birds_seen_winter_2024-2025.xlsx
    ├── [124K Apr 28 11:28]  data/xls/LUKE_Flocks_and_birds_seen_winter_2025-2026.xlsx
    ├── [221K Apr 28 11:28]  data/xls/MICKO_Flocks_and_birds_seen_winter_2025-2026.xlsx
    ├── [ 97K Apr 27 11:42]  data/xls/Most recent colour rings and date.xlsx
    ├── [233K Apr 28 11:28]  data/xls/NINA_Flocks_and_birds_seen_winter_2025-2026.xlsx
    ├── [179K Apr 27 11:13]  data/xls/SARAH_Flocks_and_birds_seen_winter_2024-2025.xlsx
    └── [403K Apr 28 11:28]  data/xls/SARAH_Flocks_and_birds_seen_winter_2025-2026.xlsx

7 directories, 325 files

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
[masteringshiny]: https://mastering-shiny.org/
[rpackages]: https://r-pkgs.org/
[rsqlite]: https://rsqlite.r-dbi.org/
[shiny]: https://shiny.posit.co/
[shinydatabases]: https://posit.co/blog/shiny-with-databases
[shinyreference]: https://shiny.posit.co/r/reference/shiny/latest/
[shinytutorial]: https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/
[sqlite]: https://sqlite.org/index.html
[usethis]: https://usethis.r-lib.org/index.html
