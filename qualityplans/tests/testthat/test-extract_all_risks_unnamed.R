source("R/utilities.R")
source("R/quality_risks.R")

test_config_file <- "data/testconfig.yaml"
test_file_paths <- get_file_paths(test_config_file)


dummy_output <- extract_all_risks_unnamed(test_file_paths, test_config_file)

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

