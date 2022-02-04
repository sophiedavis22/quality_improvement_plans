dummy_data_list_a <- data.frame(dimension=c("Money", "People", "Time", "Tech", "Speed"),
                                     qr_1=c("more", "less", NA, NA, "faster"),
                                     qr_2=c("some", "fewer", "additional", NA, "slower"),
                                     total=c(2,2,1,0,2))

dummy_data_list_b <- data.frame(dimension=c("Money", "People", "Time", "Tech", "Speed"),
                                     qr_1=c(NA, "varies", "sun", NA, NA),
                                     qr_2=c("rain", "interview", "negative", NA, "steady"),
                                     total=c(1,2,2,0,1))

dummy_data <- list(A_division=dummy_data_list_a,
                   B_division=dummy_data_list_b)

dummy_output <- create_dim_table(dummy_data)


expected_output <- data.frame(dimension=c("Money", "People", "Time", "Tech", "Speed"),
                                  A_division=c(2,2,1,0,2),
                                  B_division=c(1,2,2,0,1),
                                  total=c(3,4,3,0,3))


testthat::test_that("Check data output is list", {
  testthat::expect_type(dummy_output, "list")
})

testthat::test_that("Check data output is as expected", {
  testthat::expect_equal(dummy_output,expected_output)
})
