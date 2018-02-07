context("test-dplyr_custom_functions.R")

test_that("duplicates returns duplicates", {
  df <- tibble::tibble(x = 1:10, y = c(rep(10, 4), 11, 11, 13:16), z = c(20,21,21,21,22:26, 26))
  expect_equal(
    nrow(duplicates(df, y)),
    6
  )
  expect_equal(
    nrow(duplicates(df, z)),
    5
  )
  expect_equal(
    nrow(duplicates(df, y, z)),
    3
  )
})
