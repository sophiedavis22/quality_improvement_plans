source("R/utilities.R")

test_config_file <- "data/testconfig.yaml"

dummy_months <- c("November 2021", "December 2021", "January 2022", "February 2022", "March 2022", "April 2022", "May 2022",
                  "June 2022", "July 2022", "August 2022", "September 2022", "October 2022", "November 2022", "December 2022")
dummy_data <- list(main=data.frame(month=factor(dummy_months, levels=dummy_months),
                                   m1=c(1, 2, 0, 4, rep(NA,10)),
                                   details_m1=c("Detail 1a", "Detail 1b", "Detail 1c", "detail 1d", rep(NA, 10)),
                                   m2=c(0, 0, 3, 0, rep(NA,10)),
                                   details_m2=c("Detail 2a", NA, "Detail 2c", "Detail 2d", rep(NA, 10)),
                                   m3=c(0, 0, 0, 0, rep(NA,10)),
                                   details_m3=c(0, NA, "dEtail 3c", "Detail 3d", rep(NA, 10)),
                                   m4=c(0, 7, 0, 0, rep(NA,10)),
                                   details_m4=c(0, NA, "Detail 4c", NA, rep(NA, 10))),
                   div_A=data.frame(month=factor(dummy_months, levels=dummy_months),
                                    m1=c(5, 6, 7, 0, rep(NA,10)),
                                    details_m1=c("Detail 1a", "Detail 1b", "Detail 1c", "detail 1d", rep(NA, 10)),
                                    m2=c(55, 0, 33, 66, rep(NA,10)),
                                    details_m2=c("Detail 2a", NA, "Detail 2c", "Detail 2d", rep(NA, 10)),
                                    m3=c(20, 0, 0, 0, rep(NA,10)),
                                    details_m3=c(NA, NA, "dEtail 3c", "Detail 3d", rep(NA, 10)),
                                    m4=c(3, 7, 0, 0, rep(NA,10)),
                                    details_m4=c(0, NA, "Detail 4c", NA, rep(NA, 10))))

dummy_data_error <- list(main=data.frame(month=factor(dummy_months, levels=dummy_months),
                                         m1=c(1, 2, 0, 4, rep(NA,10)),
                                         details_m1=c("Detail 1a", "Detail 1b", "Detail 1c", "detail 1d", rep(NA, 10)),
                                         m2=c(0, 0, 3, NA, rep(NA,10)),
                                         details_m2=c("Detail 2a", NA, "Detail 2c", "Detail 2d", rep(NA, 10)),
                                         m3=c(0, 0, 0, 0, rep(NA,10)),
                                         details_m3=c(0, NA, "dEtail 3c", "Detail 3d", rep(NA, 10)),
                                         m4=c(0, 7, 0, 0, rep(NA,10)),
                                         details_m4=c(0, NA, "Detail 4c", NA, rep(NA, 10))),
                         div_A=data.frame(month=factor(dummy_months, levels=dummy_months),
                                          m1=c(5, 6, 7, 0, rep(NA,10)),
                                          details_m1=c("Detail 1a", "Detail 1b", "Detail 1c", "detail 1d", rep(NA, 10)),
                                          m2=c(55, NA, 33, 66, rep(NA,10)),
                                          details_m2=c("Detail 2a", NA, "Detail 2c", "Detail 2d", rep(NA, 10)),
                                          m3=c(20, 0, 0, 0, rep(NA,10)),
                                          details_m3=c(NA, NA, "dEtail 3c", "Detail 3d", rep(NA, 10)),
                                          m4=c(3, 7, 0, 0, rep(NA,10)),
                                          details_m4=c(0, NA, "Detail 4c", NA, rep(NA, 10))))

dummy_output <- recent_metric_table(dummy_data, test_config_file)

dummy_output_error <- recent_metric_table(dummy_data_error, test_config_file)

expected_output <- data.frame(Question=c("M1", "M2", "M3", "M4"),
                              v1=c(6,55,20,3),
                              v2=c(8,0,0,14))
colnames(expected_output) <- c("Question",
                            paste0("Previous month (November 2021)"),
                            paste0("Latest month (December 2021)"))




testthat::test_that("Check data input is as expected", {
  testthat::expect_equal(dummy_output,expected_output)
})

testthat::test_that("Check data output is list", {
  testthat::expect_type(dummy_output, "list")
})

testthat::test_that("Check data input is as expected", {
  testthat::expect_error(dummy_output_error)
})

