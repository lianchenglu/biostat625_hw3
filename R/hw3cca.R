#' Matrix Square Root
#'
#' This function calculates the square root of a matrix.
#'
#' @param m A matrix which we want to find the square root of.
#'
#' @return The square root of the input matrix m.
#'
#' @examples
#' matrixsqrt <- function(m) {
#'
#'    eig <- eigen(m)
#'    Q <- eig$vectors
#'    rsqrtD <- sqrt(eig$values)
#'    return(Q %*% diag(rsqrtD) %*% t(Q))
#' }
#'
#' @export

matrixsqrt <- function(m) {
  eig <- eigen(m)
  Q <- eig$vectors
  rsqrtD <- sqrt(eig$values)
  return(Q %*% diag(rsqrtD) %*% t(Q))
}

#' Simple Canonical Correlation Analysis
#'
#' This function conducts the simple Canonical Correlation Analysis (CCA) between two sets of variables.
#'
#' @param x is a matrix/dataframe where each column is a variable and each row is an observation.
#' @param y is another matrix/dataframe where each column is a variable and each row is an observation.
#'
#' @return A list containing:
#' - "cor": The canonical correlation coefficients
#' - "xcoef": The canonical vector of x standardized
#' - "ycoef": The canonical vector of y standardized
#'
#' @examples
#' x <- matrix(rnorm(2000), 1000, 2)
#' y <- matrix(rnorm(2000), 1000, 2)
#' res <- cca_simple(x, y)
#' print(res)

#' x <- matrix(rnorm(200000), 40000, 5)
#' y <- matrix(rnorm(200000), 40000, 5)
#' res <- cca_simple(x, y)
#' print(res)
#'
#' x <- matrix(rnorm(2000000), 400000, 5)
#' y <- matrix(rnorm(2000000), 400000, 5)
#' res <- cca_simple(x, y)
#' print(res)
#'
#' system.time(cca_simple(x,y)) # Time of my function
#' system.time(cancor(x,y)[1:3]) # Time of the R build-in cca function
#' @export

cca_simple <- function(x, y) {
  # centralizer the data
  x <- scale(x, center = TRUE, scale = FALSE)
  y <- scale(y, center = TRUE, scale = FALSE)

  # Calculate the covariance matrix
  cov_xy <- t(x) %*% y / (nrow(x))
  cov_xx <- cov(x)
  cov_yy <- cov(y)

  # Calculate the inverse matrix of the square root
  cov_xx_inv_sqrt <- solve(matrixsqrt(cov_xx))
  cov_yy_inv_sqrt <- solve(matrixsqrt(cov_yy))

  # Calculate the best linear combination to maximize the correlation between the two sets of variables
  P <- cov_xx_inv_sqrt %*% cov_xy %*% cov_yy_inv_sqrt
  svd_decomposition <- svd(P)  # Singular value decomposition

  # Calculate canonical vector and standardized
  x_coef <-
    cov_xx_inv_sqrt %*% svd_decomposition$u / sqrt(nrow(x) - 1)
  y_coef <-
    cov_yy_inv_sqrt %*% svd_decomposition$v / sqrt(nrow(y) - 1)

  # Calculate canonical correlation coefficients
  cov_xy <- cov(x, y)
  A_star <-
    cov_xx_inv_sqrt %*% cov_xy %*% solve(cov_yy) %*% t(cov_xy) %*% cov_xx_inv_sqrt
  A <- eigen(A_star)
  can_cor <- sqrt(A$values)

  return(list(
    cor = can_cor,
    xcoef = x_coef,
    ycoef = y_coef
  ))
}
