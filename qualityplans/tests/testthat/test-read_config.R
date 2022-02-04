testconfig_file <- "data/testconfig.yaml"
expected_output <- list(directory = "qualityplans/data",
                        null_values = c("test", "NA", "-", "- ", "NA", "NA ", "N/A", "N/A ", "na", "na ", "n/a", "n/a ", "N/a", "N/a ",
                                       "none", "None", "NONE", "none ", "None ", "NONE "),
                        metric_ranges = c("B4:P8", "B10:P14", "B17:P21", "B24:P28"),
                        reporting_months = list(this_month="Test Month", previous_month="NA"))
dummy_output <- read_config(config_file = testconfig_file)

testthat::test_that("Check config input is as expected", {
  testthat::expect_equal(dummy_output,expected_output)
})
