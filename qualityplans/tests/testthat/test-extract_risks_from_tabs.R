source("R/utilities.R")
#source("R/quality_risks.R")

testconfig_file <- "data/testconfig.yaml"
testdata_directory <- "data/test_file_main.xlsx"
testdata_tabs <- c("Quality Risk 1", "Quality Risk 2")



dummy_output <- extract_risks_from_tabs(testdata_directory, testdata_tabs, testconfig_file)
expected_output_list1 <- list(tibble::tibble(Time=c("Money", "People", "Funding", "Code", "Programs"), help=c(1, NA, 500, NA, NA)))
expected_output_list2 <- list(tibble::tibble(Time=c("Money", "People", "Funding", "Code", "Programs"), help=c(6000, NA, 2, NA, NA)))

#initiate a test
testthat::test_that("Check data input is as expected", {
  testthat::expect_equal(dummy_output[1],expected_output_list1)
})

testthat::test_that("Check data input is as expected", {
  testthat::expect_equal(dummy_output[2],expected_output_list2)
})

testthat::test_that("Check data input is of expected dimensions", {
  testthat::expect_equal(dim(dummy_output[[1]]),c(5,2))
})

testthat::test_that("Check data input is of expected dimensions", {
  testthat::expect_equal(dim(dummy_output[[2]]),c(5,2))
})

