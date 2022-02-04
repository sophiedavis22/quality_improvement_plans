source("R/utilities.R")
source("R/quality_risks.R")

dummy_list_1 <- list(Resource=c("Money", "People", "Time", "Tech", "Speed"),
                     Solution=c("more", "less", NA, NA, "faster"))
dummy_list_2 <- list(Resource=c("Money", "People", "Time", "Tech", "Speed"),
                     Solution=c("some", "fewer", "additional", NA, "slower"))

dummy_list <- list(
  div_1=list(
    qr_1=dummy_list_1,
    qr_2=dummy_list_2))

expected_output <- data.frame(dimension=c("Money", "People", "Time", "Tech", "Speed"),
                                  qr_1=c("more", "less", NA, NA, "faster"),
                                  qr_2=c("some", "fewer", "additional", NA, "slower"),
                                  total=c(2,2,1,0,2))


dummy_output<- merge_risks(dummy_list, 1)


testthat::test_that("Check data output is list", {
  testthat::expect_type(dummy_output, "list")
})

testthat::test_that("Check data output is of expected dimensions", {
  testthat::expect_equal(dummy_output,expected_output)
})
