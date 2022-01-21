source("R/utilities.R")
source("R/quality_risks.R")

dummy_list_a1 <- list(Resource=c("Money", "People", "Time", "Tech", "Speed"),
                     Solution=c("more", "less", NA, NA, "faster"))
dummy_list_a2 <- list(Resource=c("Money", "People", "Time", "Tech", "Speed"),
                     Solution=c("some", "fewer", "additional", NA, "slower"))

dummy_list_b1 <- list(Resource=c("Money", "People", "Time", "Tech", "Speed"),
                      Solution=c(NA, "varies", "sun", NA, NA))
dummy_list_b2 <- list(Resource=c("Money", "People", "Time", "Tech", "Speed"),
                      Solution=c("rain", "interview", "negative", NA, "steady"))

dummy_list <- list(
  div_a=list(
    qr_1=dummy_list_a1,
    qr_2=dummy_list_a2),
  div_b=list(
    qr_1=dummy_list_b1,
    qr_2=dummy_list_b2)
  )

expected_output_list_a <- data.frame(dimension=c("Money", "People", "Time", "Tech", "Speed"),
                                 qr_1=c("more", "less", NA, NA, "faster"),
                                 qr_2=c("some", "fewer", "additional", NA, "slower"),
                                 total=c(2,2,1,0,2))

expected_output_list_b <- data.frame(dimension=c("Money", "People", "Time", "Tech", "Speed"),
                                 qr_1=c(NA, "varies", "sun", NA, NA),
                                 qr_2=c("rain", "interview", "negative", NA, "steady"),
                                 total=c(1,2,2,0,1))

expected_output_list <- list(expected_output_list_a,
                             expected_output_list_b)

dummy_output <- merge_all_risks(dummy_list)


testthat::test_that("Check data output is list", {
  testthat::expect_type(dummy_output, "list")
})

testthat::test_that("Check data output is of expected dimensions", {
  testthat::expect_equal(dummy_output,expected_output_list)
})


