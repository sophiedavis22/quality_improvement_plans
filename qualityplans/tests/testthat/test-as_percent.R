test_vector <- c(1.2, 0.5, 0.103, 7, 0.1501)
expected_output <- c("120%", "50%", "10%","700%", "15%")
dummy_output <- as_percent(test_vector)
typeof(expected_output)
testthat::test_that("Check output as percentage", {
  testthat::expect_equal(dummy_output, expected_output)
})

dummy_output_error <- as_percent(expected_output)
testthat::test_that("Check error message appears", {
  testthat::expect_error(dummy_output_error)
})

