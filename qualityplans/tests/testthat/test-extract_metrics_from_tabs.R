source("R/utilities.R")

testconfig_file <- "data/testconfig.yaml"
testdata_file <- "data/test_file_main.xlsx"
testdata_error <- "data/test_fileB.xlsx"


dummy_output_all <- extract_metrics_from_tabs(testdata_file, testconfig_file)
dummy_output_1 <- dummy_output_all[[1]]
dummy_output_2 <- dummy_output_all[[2]]
dummy_output_3 <- dummy_output_all[[3]]
dummy_output_4 <- dummy_output_all[[4]]

dummy_output_error <- extract_metrics_from_tabs(testdata_error, testconfig_file)

expected_output_1 <- data.frame(V1=c("Q1", rep(NA,14)),
                                V2=as.character(c(rep(NA,15))),
                                V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                     "July", "August", "September", "October", "November", "December"),
                                V4=c("Number", 1, 2, NA, 4, rep(NA,10)),
                                V5=c("Details", "Detail 1a", "Detail 1b", "Detail 1c", "detail 1d", rep(NA, 10)))

expected_output_2 <- data.frame(V1=c("Q2", rep(NA,14)),
                                V2=as.character(c(rep(NA,15))),
                                V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                     "July", "August", "September", "October", "November", "December"),
                                V4=c("Number", 0, NA, 3, "0 (1 out)", rep(NA,10)),
                                V5=c("Details", "Detail 2a", NA, "Detail 2c", "Detail 2d", rep(NA, 10)))

expected_output_3 <- data.frame(V1=c("Q3", rep(NA,14)),
                                V2=as.character(c(rep(NA,15))),
                                V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                     "July", "August", "September", "October", "November", "December"),
                                V4=c("Number", NA, 0, NA, NA, rep(NA,10)),
                                V5=c("Details", NA, NA, "dEtail 3c", "Detail 3d", rep(NA, 10)))

expected_output_4 <- data.frame(V1=c("Q4", rep(NA,14)),
                                V2=as.character(c(rep(NA,15))),
                                V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                     "July", "August", "September", "October", "November", "December"),
                                V4=c("Number", 0, 7, "&", "3$", rep(NA,10)),
                                V5=c("Details", 0, NA, "Detail 4c", NA, rep(NA, 10)))

expected_output_all <- list(expected_output_1,
                            expected_output_2,
                            expected_output_3,
                            expected_output_4)

#initiate a test
testthat::test_that("Check data input is as expected", {
  testthat::expect_equal(dummy_output_all,expected_output_all)
})

testthat::test_that("Check data input is of expected dimensions", {
  testthat::expect_equal(dim(dummy_output_1),c(15,5))
})

testthat::test_that("Check data input is of expected dimensions", {
  testthat::expect_equal(dim(dummy_output_2),c(15,5))
})

testthat::test_that("Check data input is of expected dimensions", {
  testthat::expect_equal(dim(dummy_output_3),c(15,5))
})

testthat::test_that("Check data input is of expected dimensions", {
  testthat::expect_equal(dim(dummy_output_4),c(15,5))
})


testthat::test_that("Check data input error if missing", {
  testthat::expect_error(dummy_output_error)
})

