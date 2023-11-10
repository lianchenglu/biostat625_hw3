# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(hw3cca)

test_that("cca_simple function", {
  # Create a multivariable data set
  set.seed(123)
  x <- matrix(rnorm(1000000), ncol=2)
  y <- matrix(rnorm(1000000), ncol=2)

  # Using R's built-in cancor function to get the baseline results,
  # only the corrected correlations, x and y coefficients are obtained
  benchmark <- cancor(x, y)[1:3]
  # Get test results using the cca_simple to be tested
  result <- cca_simple(x, y)

  # Test to match benchmark results to test results
  expect_equal(result$cor, benchmark$cor, tolerance = 0.0001)
  expect_equal(result$xcoef, benchmark$xcoef, tolerance = 0.0001)
  expect_equal(result$ycoef, benchmark$ycoef, tolerance = 0.0001)

  # Test for errors and exceptions, such as dimensions of x and y that do not match
  x_mismatched_dim = matrix(rnorm(500), ncol = 5)
  expect_error(cca_simple(x_mismatched_dim, y))

  # Test the case where x and y are empty matrices
  x_null = matrix(ncol=5)
  y_null = matrix(ncol=5)
  expect_error(cca_simple(x_null, y_null))

  # Time test the cca simp function using system time
  start.time <- Sys.time()
  result <- cca_simple(x, y)
  end.time <- Sys.time()
  cat('Time of test set 1 of cca_simple: ', end.time - start.time, '\n')
  # Time test R's built-in cancor function using system time
  start.time <- Sys.time()
  benchmark <- cancor(x, y)[1:3]
  end.time <- Sys.time()
  cat('Time of test set 1 of cancor: ', end.time - start.time, '\n')

  # Create a multivariable data set
  set.seed(123)
  x <- matrix(rnorm(10000000), ncol=5)
  y <- matrix(rnorm(10000000), ncol=5)

  # Using R's built-in cancor function to get the baseline results,
  # only the corrected correlations, x and y coefficients are obtained
  benchmark <- cancor(x, y)[1:3]
  # Get test results using the cca_simple to be tested
  result <- cca_simple(x, y)

  # Test to match benchmark results to test results
  expect_equal(result$cor, benchmark$cor, tolerance = 0.0001)
  expect_equal(abs(result$xcoef), abs(benchmark$xcoef), tolerance = 0.0001)
  expect_equal(abs(result$ycoef), abs(benchmark$ycoef), tolerance = 0.0001)

  # Test for errors and exceptions, such as dimensions of x and y that do not match
  x_mismatched_dim = matrix(rnorm(500), ncol = 5)
  expect_error(cca_simple(x_mismatched_dim, y))

  # Test the case where x and y are empty matrices
  x_null = matrix(ncol=5)
  y_null = matrix(ncol=5)
  expect_error(cca_simple(x_null, y_null))
  # Time test the cca simp function using system time
  start.time <- Sys.time()
  result <- cca_simple(x, y)
  end.time <- Sys.time()
  cat('Time of test set 2 of cca_simple: ', end.time - start.time, '\n')
  # Time test R's built-in cancor function using system time
  start.time <- Sys.time()
  benchmark <- cancor(x, y)[1:3]
  end.time <- Sys.time()
  cat('Time of test set 2 of cancor: ', end.time - start.time, '\n')
})
