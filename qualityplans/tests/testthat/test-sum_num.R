dummy_data_num <- tibble::tibble(v1=c(1,NA,3),
                                 v2=c(4,5,NA))

dummy_data_char <- tibble::tibble(v1=c("1", NA, "3"),
                                  v2=c("x", "y", NA))

total <- tibble::tibble(total=c(5,5,3))
dummy_output_num <- sum_num(dummy_data_num)
dummy_output_char <- sum_num(dummy_data_char)

expected_output_num <- tibble::tibble(cbind(dummy_data_num, total))
expected_output_char <- tibble::tibble(cbind(dummy_data_char, total))

testthat::test_that("Check data output is list", {
  testthat::expect_type(dummy_output_num, "list")
})

testthat::test_that("Check data produces error due to value type", {
  testthat::expect_error(dummy_output_char)
})

testthat::test_that("Check data output is as expected", {
  testthat::expect_equal(dummy_output_num,expected_output_num)
})



