library(testthat)
library(lottie)

rings <- c("B", "D", "F", "G", "M", "N", "O", "P", "R", "U", "W", "Y", "Sd", "Sg", "Sn", "Sp", "bm", "dy", "gd", "ng",
           "nr", "od", "on", "rd", "ry", "B*", "G*", "N*", "R*", "W*", "Y*")
valid_codes <- c(
    "BDL", "BDR", ## Length 3
    "SgNL", "ryOR", ## Length 4, first ring is two characters
    "WY*L", "PonR", ## Length 4, first ring is one character
    "SdnrL", "bmrdR" ## Length 5
)


testthat::test_that(
    "3-character string is split correctly",
    lottie::extract_rings(
        code = "BDL",
        valid_codes =
            valid_codes, rings = rings
    ) == list(
        "leg" = "L",
        "top" = "B",
        "bottom" = "L"
    )
)
testthat::test_that(
    "3-character string is split correctly",
    lottie::extract_rings(
        code = "BDR",
        valid_codes =
            valid_codes, rings = rings
    ) == list(
        "leg" = "L",
        "top" = "B",
        "bottom" = "R"
    )
)
testthat::test_that(
    "4-character (top is first two characters) string is split correctly",
    lottie::extract_rings(
        code = "SgNL",
        valid_codes =
            valid_codes, rings = rings
    ) == list(
        "leg" = "L",
        "top" = "Sg",
        "bottom" = "N"
    )
)
testthat::test_that(
    "4-character (top is first two characters) string is split correctly",
    lottie::extract_rings(
        code = "ryOR",
        valid_codes =
            valid_codes, rings = rings
    ) == list(
        "leg" = "R",
        "top" = "ry",
        "bottom" = "O"
    )
)
testthat::test_that(
    "4-character (top is first character) string is split correctly",
    lottie::extract_rings(
        code = "WY*L",
        valid_codes =
            valid_codes, rings = rings
    ) == list(
        "leg" = "L",
        "top" = "W",
        "bottom" = "Y*"
    )
)
testthat::test_that(
    "4-character (top is first character) string is split correctly",
    lottie::extract_rings(
        code = "PonR",
        valid_codes =
            valid_codes, rings = rings
    ) == list(
        "leg" = "R",
        "top" = "P",
        "bottom" = "on"
    )
)
testthat::test_that(
    "5-character string is split correctly",
    lottie::extract_rings(
        code = "SdnrL",
        valid_codes =
            valid_codes, rings = rings
    ) == list(
        "leg" = "L",
        "top" = "Sd",
        "bottom" = "nr"
    )
)
testthat::test_that(
    "5-character string is split correctly",
    lottie::extract_rings(
        code = "bmrdR",
        valid_codes =
            valid_codes, rings = rings
    ) == list(
        "leg" = "R",
        "top" = "bm",
        "bottom" = "rd"
    )
)
