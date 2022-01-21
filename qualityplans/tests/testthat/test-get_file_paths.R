testconfig_file <- "data/testconfig.yaml"

dummy_output <- get_file_paths(config_file = testconfig_file)
dummy_output

testthat::test_that("Check output is as character", {
  testthat::expect_type(dummy_output, "character")
})

