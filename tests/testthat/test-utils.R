library(lottie)
library(patrick)
library(testthat)

known_rings <- c(
        "None", "BTO", "B", "D", "F", "G", "M", "N", "O", "P", "R", "U", "W", "Y", "Sd", "Sg", "Sn", "So", "Sp", "Sr",
        "Sw", "Sy", "bm", "dg", "do", "dr", "dy", "gd", "gn", "mb", "ng", "no", "nr", "od", "on", "rd", "rn", "ry",
        "yd", "yr", "B*", "G*", "N*", "P*", "R*", "W*", "Y*")
valid_codes <- c(
    "BDL", "BDR", ## Length 3
    "SgNL", "ryOR", ## Length 4, first ring is two characters
    "WY*L", "PonR", ## Length 4, first ring is one character
    "SdnrL", "bmrdR", ## Length 5
    "gnBL", "ngDL", "ryFR", ## Split rings
    "Unlisted"
)

patrick::with_parameters_test_that("Splitting of all codes is correct:",
  {
    testthat::expect_identical(extracted_rings, expected_rings)
  },
  extracted_rings = c(
    lottie::extract_rings(code = "bmrdR", valid_codes = valid_codes, known_rings = known_rings),
    lottie::extract_rings(code = "SdnrL", valid_codes = valid_codes, known_rings = known_rings),
    lottie::extract_rings(code = "PonR", valid_codes = valid_codes, known_rings = known_rings ),
    lottie::extract_rings(code = "WY*L", valid_codes = valid_codes, known_rings = known_rings),
    lottie::extract_rings(code = "WY*L", valid_codes = valid_codes, known_rings = known_rings),
    lottie::extract_rings(code = "ryOR", valid_codes = valid_codes, known_rings = known_rings),
    lottie::extract_rings( code = "SgNL", valid_codes = valid_codes, known_rings = known_rings),
    lottie::extract_rings( code = "BDR", valid_codes = valid_codes, known_rings = known_rings),
    lottie::extract_rings( code = "BDL", valid_codes = valid_codes, known_rings = known_rings),
    lottie::extract_rings( code = "gnBL", valid_codes = valid_codes, known_rings = known_rings),
    lottie::extract_rings( code = "ngDL", valid_codes = valid_codes, known_rings = known_rings),
    lottie::extract_rings( code = "ryFR", valid_codes = valid_codes, known_rings = known_rings),
    lottie::extract_rings( code = "Unlisted", valid_codes = valid_codes, known_rings = known_rings),
    lottie::extract_rings( code = "None", valid_codes = valid_codes, known_rings = known_rings)
  ),
  expected_rings = c(
    list("code" = "bmrdR", "leg" = "R", "pit" = FALSE, "bto" = "L", "first" = "bm", "second" = "rd"),
    list("code" = "SdnrL", "leg" = "L", "pit" = FALSE, "bto" = "R", "first" = "Sd", "second" = "nr"),
    list("code" = "PonR", "leg" = "R", "pit" = FALSE, "bto" = "L", "first" = "P", "second" = "on"),
    list("code" = "WY*L", "leg" = "L", "pit" = TRUE, "bto" = "R", "first" = "W", "second" = "Y*",
         "pit_pos" = "second"),
    list(" code" = "WY*L"," leg" = "L"," pit" = TRUE," bto" = "R"," first" = "W"," second" = "Y*"," pit_pos" = "second"),
    list("code" = "ryOR", "leg" = "R", "pit" = FALSE, "bto" = "L", "first" = "ry", "second" = "O"),
    list("code" = "SgNL", "leg" = "L", "pit" = FALSE, "bto" = "R", "first" = "Sg", "second" = "N" ),
    list("code" = "BDR", "leg" = "R", "pit" = FALSE, "bto" = "L", "first" = "B", "second" = "D"),
    list( "code" = "BDL", "leg" = "L", "pit" = FALSE, "bto" = "R", "first" = "B", "second" = "D"),
    list( "code" = "gnBL", "leg" = "L", "pit" = FALSE, "bto" = "R", "first" = "gn", "second" = "B"),
    list( "code" = "ngDL", "leg" = "L", "pit" = FALSE, "bto" = "R", "first" = "ng", "second" = "D"),
    list( "code" = "ryFR", "leg" = "R", "pit" = FALSE, "bto" = "L", "first" = "ry", "second" = "F"),
    list( "code" = "Unlisted", "leg" = "", "pit" = NA, "bto" = "", "first" = "", "second" = ""),
    list( "code" = "None", "leg" = "", "pit" = NA, "bto" = "None", "first" = "", "second" = "")
  ),
  .test_name = c(
      "5-character right leg",
      "5-character left leg",
      "4-character (top is first character)",
      "4-character PIT ring (top is first character)",
      "4-character (top is first character)",
      "4-character (top is first two characters)",
      "4-character (top is first two characters)",
      "3-character (right leg)",
      "3-character (left leg)",
      "Split ring check (gnBL), was originally incorrectly split",
      "Split ring check (ngDL), was originally incorrectly split",
      "Split ring check (ryFR), was originally incorrectly split",
      "Unlisted returns missing/NA",
      "None returns missing/NA"
  )
)

testthat::test_that("Test missing column is added correctly.", {
    tidy_columns <- lottie::tidy_columns(df = data.frame(a = 1, b = 2, c = 3),
                                    expected_cols= c("a", "b", "c", "d")) |> colnames()
    testthat::expect_equal(tidy_columns, c("a", "b", "c", "d"))
})


testthat::test_that("Test 'none' column is removed.", {
    tidy_columns <- lottie::remove_none_column(df = data.frame(a = 1, b = 2, none = 3)) |> colnames()
    testthat::expect_equal(tidy_columns, c("a", "b"))
    testthat::expect_false("none" %in% tidy_columns)
})


testthat::test_that("Deduplication of ringed birds only", {
    deduplicated <- lottie::deduplicate_flock(df = data.frame(flock_id = c(1, 1, 1, 1), ringed = c(FALSE, FALSE, TRUE, TRUE)))
    testthat::expect_equal(deduplicated, data.frame(flock_id = c(1, 1, 1), ringed = c(FALSE, FALSE, TRUE)))
})

patrick::with_parameters_test_that("Deduplication of ringed birds only:",
  {
    testthat::expect_identical(deduplicated, expected_rings)
  },
  deduplicated = c(
    lottie::deduplicate_flock(data.frame(flock_id = c(1, 1, 1, 1),
                                         ringed = c(FALSE, FALSE, TRUE, TRUE))),
    lottie::deduplicate_flock(data.frame(flock_id = c(1, 1, 1, 2),
                                         ringed = c(FALSE, FALSE, TRUE, TRUE))),
    lottie::deduplicate_flock(data.frame(flock_id = c(1, 1, 1, 1, 1, 1),
                                         ringed = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)))
  ),
  expected_rings = c(
    data.frame(flock_id = c(1, 1, 1), ringed = c(FALSE, FALSE, TRUE)),
    data.frame(flock_id = c(1, 1, 1, 2), ringed = c(FALSE, FALSE, TRUE, TRUE)),
    data.frame(flock_id = c(1, 1, 1), ringed = c(FALSE, FALSE, TRUE))
  ),
  .test_name = c(
    "Single duplication",
    "No duplicates",
    "Four duplicates"
  )
)
