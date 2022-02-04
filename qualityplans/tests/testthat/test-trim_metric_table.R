source("R/utilities.R")

test_config_file <- "data/testconfig.yaml"


dummy_data_1 <- data.frame(V1=c("Q1", rep(NA,14)),
                                 V2=as.character(c(rep(NA,15))),
                                 V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"),
                                 V4=c("Number", 1, 2, NA, 4, rep(NA,10)),
                                 V5=c("Details", "Detail 1a", "Detail 1b", "Detail 1c", "detail 1d", rep(NA, 10)))

dummy_data_2_error <- data.frame(V1=c("Q2", rep(NA,14)),
                                 V2=as.character(c(rep(NA,15))),
                                 V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"),
                                 V4=c("Number", 0, NA, 3, "0 (1 out)", rep(NA,10)),
                                 V5=c("Details", "Detail 2a", NA, "Detail 2c", "Detail 2d", rep(NA, 10)))
dummy_data_2 <- data.frame(V1=c("Q2", rep(NA,14)),
                                 V2=as.character(c(rep(NA,15))),
                                 V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"),
                                 V4=c("Number", 0, NA, 3, 66, rep(NA,10)),
                                 V5=c("Details", "Detail 2a", NA, "Detail 2c", "Detail 2d", rep(NA, 10)))

dummy_data_3 <- data.frame(V1=c("Q3", rep(NA,14)),
                                 V2=as.character(c(rep(NA,15))),
                                 V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"),
                                 V4=c("Number", NA, 0, NA, NA, rep(NA,10)),
                                 V5=c("Details", NA, NA, "dEtail 3c", "Detail 3d", rep(NA, 10)))

dummy_data_4_error <- data.frame(V1=c("Q4", rep(NA,14)),
                                 V2=as.character(c(rep(NA,15))),
                                 V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"),
                                 V4=c("Number", 0, 7, "&", "3$", rep(NA,10)),
                                 V5=c("Details", 0, NA, "Detail 4c", NA, rep(NA, 10)))
dummy_data_4 <- data.frame(V1=c("Q4", rep(NA,14)),
                           V2=as.character(c(rep(NA,15))),
                           V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                "July", "August", "September", "October", "November", "December"),
                           V4=c("Number", 0, 7, NA, NA, rep(NA,10)),
                           V5=c("Details", 0, NA, "Detail 4c", NA, rep(NA, 10)))



dummy_data_error <- list(dummy_data_1,
                       dummy_data_2_error,
                       dummy_data_3,
                       dummy_data_4_error)

dummy_data_all <- list(dummy_data_1,
                         dummy_data_2,
                         dummy_data_3,
                         dummy_data_4)



dummy_output_error <- trim_metric_table(dummy_data_error, test_config_file)
conf <- read_config(test_config_file)
conf$metric_months
dummy_output_all <- trim_metric_table(dummy_data_all, test_config_file)



expected_output_1 <- data.frame(month=c("November", "December", "January", "February", "March", "April", "May", "June",
                                   "July", "August", "September", "October", "November", "December"),
                              count=c(1, 2, NA, 4, rep(NA,10)),
                              details=c("Detail 1a", "Detail 1b", "Detail 1c", "detail 1d", rep(NA, 10)))
expected_output_2 <- data.frame(month=c("November", "December", "January", "February", "March", "April", "May", "June",
                                        "July", "August", "September", "October", "November", "December"),
                                count=c(0, NA, 3, 66, rep(NA,10)),
                                details=c("Detail 2a", NA, "Detail 2c", "Detail 2d", rep(NA, 10)))
expected_output_3 <- data.frame(month=c("November", "December", "January", "February", "March", "April", "May", "June",
                                        "July", "August", "September", "October", "November", "December"),
                                count=c(NA, 0, NA, NA, rep(NA,10)),
                                details=c(NA, NA, "dEtail 3c", "Detail 3d", rep(NA, 10)))
expected_output_4 <- data.frame(month=c("November", "December", "January", "February", "March", "April", "May", "June",
                                        "July", "August", "September", "October", "November", "December"),
                                count=c(0, 7, NA, NA, rep(NA,10)),
                                details=c(0, NA, "Detail 4c", NA, rep(NA, 10)))

expected_output_all <- list(expected_output_1,
                            expected_output_2,
                            expected_output_3,
                            expected_output_4)



#initiate a test
testthat::test_that("Check data input is as expected", {
  testthat::expect_equal(dummy_output_all,expected_output_all)
})


testthat::test_that("Check data input error if count not numeric", {
  testthat::expect_error(dummy_output_error)
})


