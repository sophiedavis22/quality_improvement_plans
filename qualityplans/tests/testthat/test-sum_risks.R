dummy_data_num <- tibble::tibble(Dimension=c("A","B","C"),
                         v1=c(1,NA,3),
                         v2=c(4,5,NA))

dummy_data_char <- tibble::tibble(Dimension=c("A","B","C"),
                                 v1=c("1", NA, "3"),
                                 v2=c("x", "y", NA))

frequency <- tibble::tibble(frequency=c(2,1,1))
dummy_output_num <- sum_risks(dummy_data_num)
dummy_output_char <- sum_risks(dummy_data_char)

typeof(dummy_output_num)

expected_output_num <- tibble(cbind(dummy_data_num, frequency))
expected_output_char <- tibble(cbind(dummy_data_char, frequency))


testthat::test_that("Check data output is list", {
  testthat::expect_type(dummy_output_num, "list")
})

testthat::test_that("Check data output is list", {
  testthat::expect_type(dummy_output_char, "list")
})

testthat::test_that("Check data output is of expected dimensions", {
  testthat::expect_equal(dummy_output_num,expected_output_num)
})

testthat::test_that("Check data output is of expected dimensions", {
  testthat::expect_equal(dummy_output_char,expected_output_char)
})

