# Lotties - Data capture and management for Long Tailed Tits Study

This repository contains code for developing a [Shiny][shiny] web application for recording data in the Long Tailed Tit
longitudinal study in the Rivelin Valley on the outskirts of Sheffield, South Yorkshire.

## Overview

The overarching goal of the repository is to develop a unified web-based interface for recording, validating, cleaning
and storing data and the subsequent network analysis of interactions.

### Technologies

We leverage the [Shiny][shiny] package to capture data and store it in an [SQLite][sqlite] database (using the
[RSQLite][rsqlite] package).


## Resources

Useful resources for R packages and Shiny development.

- [R Packages][rpackages]
- [devtools][devtools]
- [usethis][usethis]
- [Shiny tutorial][shinytutorial]
- [Shiny function reference][shinyreference]

[devtools]: https://devtools.r-lib.org/index.html
[rpackages]: https://r-pkgs.org/
[rsqlite]: https://rsqlite.r-dbi.org/
[shiny]: https://shiny.posit.co/
[shinyreference]: https://shiny.posit.co/r/reference/shiny/latest/
[shinytutorial]: https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/
[sqlite]: https://sqlite.org/index.html
[usethis]: https://usethis.r-lib.org/index.html
