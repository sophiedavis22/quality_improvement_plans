dummy_months <- c("November 2021", "December 2021", "January 2022", "February 2022", "March 2022", "April 2022", "May 2022",
                  "June 2022", "July 2022", "August 2022", "September 2022", "October 2022", "November 2022", "December 2022")
dummy_data <- list(main=data.frame(month=factor(dummy_months, levels=dummy_months),
                                            m1=c(1, 2, NA, 4, rep(NA,10)),
                                            details_m1=c("Detail 1a", "Detail 1b", "Detail 1c", "detail 1d", rep(NA, 10)),
                                            m2=c(0, NA, 3, 66, rep(NA,10)),
                                            details_m2=c("Detail 2a", NA, "Detail 2c", "Detail 2d", rep(NA, 10)),
                                            m3=c(NA, 0, NA, NA, rep(NA,10)),
                                            details_m3=c(NA, NA, "dEtail 3c", "Detail 3d", rep(NA, 10)),
                                            m4=c(0, 7, NA, NA, rep(NA,10)),
                                            details_m4=c(0, NA, "Detail 4c", NA, rep(NA, 10))),
                   div_A=data.frame(month=factor(dummy_months, levels=dummy_months),
                                    m1=c(5, 6, 7, NA, rep(NA,10)),
                                    details_m1=c("Detail 1a", "Detail 1b", "Detail 1c", "detail 1d", rep(NA, 10)),
                                    m2=c(55, NA, 33, 66, rep(NA,10)),
                                    details_m2=c("Detail 2a", NA, "Detail 2c", "Detail 2d", rep(NA, 10)),
                                    m3=c(20, 0, NA, NA, rep(NA,10)),
                                    details_m3=c(NA, NA, "dEtail 3c", "Detail 3d", rep(NA, 10)),
                                    m4=c(3, 7, NA, NA, rep(NA,10)),
                                    details_m4=c(0, NA, "Detail 4c", NA, rep(NA, 10))))


dummy_output <- combine_metrics(dummy_data)

expected_months <- c("November 2021", "December 2021", "January 2022", "February 2022", "March 2022", "April 2022", "May 2022",
                     "June 2022", "July 2022", "August 2022", "September 2022", "October 2022", "November 2022", "December 2022")
expected_output <- data.frame(division=c(rep("main", 4), rep("div_A", 4)),
                                  month=factor(rep(expected_months[1:4],2), levels=expected_months),
                                  m1=c(1, 2, NA, 4, 5, 6, 7, NA),
                                  details_m1=c("Detail 1a", "Detail 1b", "Detail 1c", "detail 1d","Detail 1a", "Detail 1b", "Detail 1c", "detail 1d"),
                                  m2=c(0, NA, 3, 66, 55, NA, 33, 66),
                                  details_m2=c("Detail 2a", NA, "Detail 2c", "Detail 2d", "Detail 2a", NA, "Detail 2c", "Detail 2d"),
                                  m3=c(NA, 0, NA, NA, 20, 0, NA, NA),
                                  details_m3=c(NA, NA, "dEtail 3c", "Detail 3d", NA, NA, "dEtail 3c", "Detail 3d"),
                                  m4=c(0, 7, NA, NA, 3, 7, NA, NA),
                                  details_m4=c(0, NA, "Detail 4c", NA, 0, NA, "Detail 4c", NA))


#initiate a test
testthat::test_that("Check data input is as expected", {
  testthat::expect_equal(dummy_output,expected_output)
})

testthat::test_that("Check data output is list", {
  testthat::expect_type(dummy_output, "list")
})

