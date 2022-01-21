test_list <- list(apple=c(1,2),
                  banana=c("x", "y", "z"),
                  cherry=c("a1", "b2", "c3", "d4"))


test_new_names <- c("artichoke", "broccoli", "cabbage")
dummy_output <- assign_list_names(test_list, test_new_names)

testthat::test_that("Check list has new names", {
  testthat::expect_equal(names(dummy_output),test_new_names)
})


test_names_too_long <- c("artichoke", "broccoli", "cabbage", "durian")
dummy_output_error <- assign_list_names(test_list, test_names_too_long)

testthat::test_that("Check error message appears", {
  testthat::expect_error(dummy_output_error)
})

