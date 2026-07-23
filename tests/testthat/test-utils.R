library(lottie)
library(patrick)
library(testthat)

valid_rings <- c(
  "None", "BTO", "B", "D", "F", "G", "M", "N", "O", "P", "R", "U", "W", "Y", "Sd", "Sg", "Sn", "So", "Sp", "Sr",
  "Sw", "Sy", "bm", "dg", "do", "dr", "dy", "gd", "gn", "mb", "ng", "no", "nr", "od", "on", "rd", "rn", "ry",
  "yd", "yr", "B*", "G*", "N*", "P*", "R*", "W*", "Y*")
valid_ring_combinations <- c(
  "BDL", "BDR", ## Length 3
  "SgNL", "ryOR", ## Length 4, first ring is two characters
  "WY*L", "PonR", ## Length 4, first ring is one character
  "SdnrL", "bmrdR", ## Length 5
  "gnBL", "ngDL", "ryFR", ## Split rings
  "Unlisted"
)

patrick::with_parameters_test_that("Splitting of known codes is correct:",
  {
    testthat::expect_identical(extracted_rings, expected_rings)
  },
  extracted_rings = c(
    lottie::extract_rings(code = "bmrdR", valid_ring_combinations = valid_ring_combinations, valid_rings = valid_rings),
    lottie::extract_rings(code = "SdnrL", valid_ring_combinations = valid_ring_combinations, valid_rings = valid_rings),
    lottie::extract_rings(code = "PonR", valid_ring_combinations = valid_ring_combinations, valid_rings = valid_rings),
    lottie::extract_rings(code = "WY*L", valid_ring_combinations = valid_ring_combinations, valid_rings = valid_rings),
    lottie::extract_rings(code = "Y*WL", valid_ring_combinations = valid_ring_combinations, valid_rings = valid_rings),
    lottie::extract_rings(code = "ryOR", valid_ring_combinations = valid_ring_combinations, valid_rings = valid_rings),
    lottie::extract_rings(code = "SgNL", valid_ring_combinations = valid_ring_combinations, valid_rings = valid_rings),
    lottie::extract_rings(code = "BDR", valid_ring_combinations = valid_ring_combinations, valid_rings = valid_rings),
    lottie::extract_rings(code = "BDL", valid_ring_combinations = valid_ring_combinations, valid_rings = valid_rings),
    lottie::extract_rings(code = "gnBL", valid_ring_combinations = valid_ring_combinations, valid_rings = valid_rings),
    lottie::extract_rings(code = "ngDL", valid_ring_combinations = valid_ring_combinations, valid_rings = valid_rings),
    lottie::extract_rings(code = "ryFR", valid_ring_combinations = valid_ring_combinations, valid_rings = valid_rings),
    lottie::extract_rings(code = "Unlisted", valid_ring_combinations = valid_ring_combinations, valid_rings =  valid_rings),
    lottie::extract_rings(code = "None", valid_ring_combinations = valid_ring_combinations, valid_rings = valid_rings)
  ),
  expected_rings = c(
    list("code" = "bmrdR", "ringed" = TRUE, "leg" = "R", "pit" = FALSE, "bto" = "L", "first" = "bm", "second" = "rd"),
    list("code" = "SdnrL", "ringed" = TRUE, "leg" = "L", "pit" = FALSE, "bto" = "R", "first" = "Sd", "second" = "nr"),
    list("code" = "PonR", "ringed" = TRUE, "leg" = "R", "pit" = FALSE, "bto" = "L", "first" = "P", "second" = "on"),
    list("code" = "WY*L", "ringed" = TRUE, "leg" = "L", "pit" = TRUE, "bto" = "R", "first" = "W", "second" = "Y*",
         "pit_pos" = "second"),
    list("code" = "Y*WL", "ringed" = TRUE, "leg" = "L", "pit" = TRUE, "bto" = "R", "first" = "Y*", "second" = "W",
         "pit_pos" = "first"),
    list("code" = "ryOR", "ringed" = TRUE, "leg" = "R", "pit" = FALSE, "bto" = "L", "first" = "ry", "second" = "O"),
    list("code" = "SgNL", "ringed" = TRUE, "leg" = "L", "pit" = FALSE, "bto" = "R", "first" = "Sg", "second" = "N" ),
    list("code" = "BDR", "ringed" = TRUE, "leg" = "R", "pit" = FALSE, "bto" = "L", "first" = "B", "second" = "D"),
    list("code" = "BDL", "ringed" = TRUE, "leg" = "L", "pit" = FALSE, "bto" = "R", "first" = "B", "second" = "D"),
    list("code" = "gnBL", "ringed" = TRUE, "leg" = "L", "pit" = FALSE, "bto" = "R", "first" = "gn", "second" = "B"),
    list("code" = "ngDL", "ringed" = TRUE, "leg" = "L", "pit" = FALSE, "bto" = "R", "first" = "ng", "second" = "D"),
    list("code" = "ryFR", "ringed" = TRUE, "leg" = "R", "pit" = FALSE, "bto" = "L", "first" = "ry", "second" = "F"),
    ## When Unlisted users enter values as we can not extract anything
    list("code" = "Unlisted", "ringed" = TRUE, "leg" = "", "pit" = FALSE, "bto" = "", "first" = "", "second" = ""),
    list("code" = "None", "ringed" = FALSE, "leg" = "", "pit" = FALSE, "bto" = "None", "first" = "", "second" = "")
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
    "None returns FALSE/''/NA"
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
