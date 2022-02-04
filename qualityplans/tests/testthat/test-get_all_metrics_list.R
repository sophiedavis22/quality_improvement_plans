source("R/utilities.R")

test_config_file <- "data/testconfig.yaml"
test_file_paths <- c("data/test_file_main.xlsx", "data/test_fileA.xlsx")
test_file_paths_error <- get_file_paths(test_config_file)
test_division_names <- c("DivisionMain", "DivisionA")

dummy_output_all <- get_all_metrics_list(test_file_paths, test_division_names, test_config_file)
dummy_output_error <- get_all_metrics_list(test_file_paths_error, test_config_file)


expected_output_1m <- data.frame(V1=c("Q1", rep(NA,14)),
                                 V2=as.character(c(rep(NA,15))),
                                 V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"),
                                 V4=c("Number", 1, 2, NA, 4, rep(NA,10)),
                                 V5=c("Details", "Detail 1a", "Detail 1b", "Detail 1c", "detail 1d", rep(NA, 10)))

expected_output_2m <- data.frame(V1=c("Q2", rep(NA,14)),
                                 V2=as.character(c(rep(NA,15))),
                                 V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"),
                                 V4=c("Number", 0, NA, 3, "0 (1 out)", rep(NA,10)),
                                 V5=c("Details", "Detail 2a", NA, "Detail 2c", "Detail 2d", rep(NA, 10)))

expected_output_3m <- data.frame(V1=c("Q3", rep(NA,14)),
                                 V2=as.character(c(rep(NA,15))),
                                 V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"),
                                 V4=c("Number", NA, 0, NA, NA, rep(NA,10)),
                                 V5=c("Details", NA, NA, "dEtail 3c", "Detail 3d", rep(NA, 10)))

expected_output_4m <- data.frame(V1=c("Q4", rep(NA,14)),
                                 V2=as.character(c(rep(NA,15))),
                                 V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"),
                                 V4=c("Number", 0, 7, "&", "3$", rep(NA,10)),
                                 V5=c("Details", 0, NA, "Detail 4c", NA, rep(NA, 10)))
expected_output_1a <- data.frame(V1=c("Q1", rep(NA,14)),
                                 V2=as.character(c(rep(NA,15))),
                                 V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"),
                                 V4=c("Number", 220, 2, NA, 4, rep(NA,10)),
                                 V5=c("Details", "Detail 1a", "Detail 1b", "Detail 1c", "detail 1d", rep(NA, 10)))

expected_output_2a <- data.frame(V1=c("Q2", rep(NA,14)),
                                 V2=as.character(c(rep(NA,15))),
                                 V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"),
                                 V4=c("Number", 5000, NA, 3, "0 (1 out)", rep(NA,10)),
                                 V5=c("Details", "Detail 2a", NA, "Detail 2c", "Detail 2d", rep(NA, 10)))

expected_output_3a <- data.frame(V1=c("Q3", rep(NA,14)),
                                 V2=as.character(c(rep(NA,15))),
                                 V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"),
                                 V4=c("Number", 999, 0, NA, NA, rep(NA,10)),
                                 V5=c("Details", NA, NA, "dEtail 3c", "Detail 3d", rep(NA, 10)))

expected_output_4a <- data.frame(V1=c("Q4", rep(NA,14)),
                                 V2=as.character(c(rep(NA,15))),
                                 V3=c("Period", "November", "December", "January", "February", "March", "April", "May", "June",
                                      "July", "August", "September", "October", "November", "December"),
                                 V4=c("Number", 666, 7, "&", "3$", rep(NA,10)),
                                 V5=c("Details", 0, NA, "Detail 4c", NA, rep(NA, 10)))




expected_output_all <- list(DivisionMain=list(expected_output_1m,
                                              expected_output_2m,
                                              expected_output_3m,
                                              expected_output_4m),
                            DivisionA=list(expected_output_1a,
                                           expected_output_2a,
                                           expected_output_3a,
                                           expected_output_4a))

#initiate a test
testthat::test_that("Check data input is as expected", {
  testthat::expect_equal(dummy_output_all,expected_output_all)
})

testthat::test_that("Check data input error if missing", {
  testthat::expect_error(dummy_output_error)
})



