testconfig_file <- "data/testconfig.yaml"

test_file_paths <- get_file_paths(config_file = testconfig_file)
dummy_output <- get_division_names(test_file_paths, config_file = testconfig_file)

dummy_output

testthat::test_that("Check output is as character", {
  testthat::expect_type(dummy_output, "character")
})

