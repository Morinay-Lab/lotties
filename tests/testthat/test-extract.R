library(DBI)
library(RSQLite)
library(testthat)
## Make a dummy database with some dummy data that can be tested
db_path <- ":memory:"
con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
overwrite <- TRUE
RSQLite::dbWriteTable(conn = con,
                      name = "Test1",
                      data.frame("a" = c(1, 2, 3),
                                 "b" = c("x", "y", "z")),
                      overwrite = overwrite)
RSQLite::dbWriteTable(conn = con,
                      name = "Test2",
                      data.frame("m" = c(4, 5, 6),
                                 "n" = c("p", "q", "r")),
                      overwrite = overwrite)

testthat::test_that("Extract data from database", {
  db_extract <- lottie::extract_table_to_csv(table="Test1",
                                             con=con,
                                             path=testthat::test_path("test1.csv"))
  testthat::expect_true(file.exists("test.csv"), )
})


testthat::test_that("Extract all tables in a list from a database", {
  exported <- lottie::extract_all_tables(con=con, tables=c("Test1", "Test2"), path=testthat::test_path("./"))
  print(paste0("exported :", exported))
  assertthat::are_equal(exported, list("Test1" = "test1.csv", "Test2" = "test2.csv"))
  testthat::expect_true(file.exists("test1.csv"))
  testthat::expect_true(file.exists("test2.csv"))
})
