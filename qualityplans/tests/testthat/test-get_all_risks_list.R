source("R/utilities.R")
source("R/quality_risks.R")

test_config_file <- "data/testconfig.yaml"
test_file_paths <- get_file_paths(test_config_file)
test_division_names <- get_division_names(test_file_paths, test_config_file)

dummy_output <- get_all_risks_list(test_file_paths, test_division_names, test_config_file)
expected_division_names <- c("file_main", "fileA", "fileB", "fileC")
expected_risk_names <- c("qr_1", "qr_2")
expected_col_names <- c("Time", "help")


testthat::test_that("Check data output is list", {
  testthat::expect_type(dummy_output, "list")
})

testthat::test_that("Check data output is list", {
  testthat::expect_type(dummy_output[1], "list")
})

testthat::test_that("Check data output is list", {
  testthat::expect_type(dummy_output[[1]][[1]], "list")
})

testthat::test_that("Check data output is of expected dimensions", {
  testthat::expect_equal(dim(dummy_output[[1]][[1]]),c(5,2))
})


testthat::test_that("Check data output is of expected dimensions", {
  testthat::expect_equal(names(dummy_output),expected_division_names)
})

testthat::test_that("Check data output is of expected dimensions", {
  testthat::expect_equal(names(dummy_output[[1]]),expected_risk_names)
})

testthat::test_that("Check data output is of expected dimensions", {
  testthat::expect_equal(names(dummy_output[[1]][[1]]),expected_col_names)
})
