library(lottie)
library(patrick)
library(testthat)

known_rings <- c("B", "D", "F", "G", "M", "N", "O", "P", "R", "U", "W", "Y", "Sd", "Sg", "Sn", "Sp", "bm", "dy", "gd", "ng",
           "nr", "od", "on", "rd", "ry", "B*", "G*", "N*", "R*", "W*", "Y*")
valid_codes <- c(
    "BDL", "BDR", ## Length 3
    "SgNL", "ryOR", ## Length 4, first ring is two characters
    "WY*L", "PonR", ## Length 4, first ring is one character
    "SdnrL", "bmrdR" ## Length 5
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
    lottie::extract_rings( code = "BDL", valid_codes = valid_codes, known_rings = known_rings)
  ),
  expected_rings= c(
    list("code" = "bmrdR", "leg" = "R", "pit" = FALSE, "bto" = "L", "first" = "bm", "second" = "rd"),
    list("code" = "SdnrL", "leg" = "L", "pit" = FALSE, "bto" = "R", "first" = "Sd", "second" = "nr"),
    list("code" = "PonR", "leg" = "R", "pit" = FALSE, "bto" = "L", "first" = "P", "second" = "on"),
    list("code" = "WY*L", "leg" = "L", "pit" = TRUE, "bto" = "R", "first" = "W", "second" = "Y*",
         "pit_pos" = "second"),
    list(" code" = "WY*L"," leg" = "L"," pit" = TRUE," bto" = "R"," first" = "W"," second" = "Y*"," pit_pos" = "second"),
    list("code" = "ryOR", "leg" = "R", "pit" = FALSE, "bto" = "L", "first" = "ry", "second" = "O"),
    list("code" = "SgNL", "leg" = "L", "pit" = FALSE, "bto" = "R", "first" = "Sg", "second" = "N" ),
    list("code" = "BDR", "leg" = "R", "pit" = FALSE, "bto" = "L", "first" = "B", "second" = "D"),
    list( "code" = "BDL", "leg" = "L", "pit" = FALSE, "bto" = "R", "first" = "B", "second" = "D")
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
      "3-character (left leg)"
  )
)
