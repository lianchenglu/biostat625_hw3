cca_simple <-
function(x, y) {
  # centralizer the data
  x <- scale(x, center = TRUE, scale = FALSE)
  y <- scale(y, center = TRUE, scale = FALSE)

  # Calculate the covariance matrix
  cov_xy <- t(x) %*% y / (nrow(x))
  cov_x <- cov(x)
  cov_y <- cov(y)

  # Calculate the inverse matrix of the square root
  cov_x_inv_sqrt <- solve(matrixsqrt(cov_x))
  cov_y_inv_sqrt <- solve(matrixsqrt(cov_y))

  # Calculate the best linear combination to maximize the correlation between the two sets of variables
  P <- cov_x_inv_sqrt %*% cov_xy %*% cov_y_inv_sqrt
  svd_decomposition <- svd(P)  # Singular value decomposition

  # Calculate canonical vector and standardized
  x_coef <-
    cov_x_inv_sqrt %*% svd_decomposition$u / sqrt(nrow(x) - 1)
  y_coef <-
    cov_y_inv_sqrt %*% svd_decomposition$v / sqrt(nrow(y) - 1)

  # Calculate canonical correlation coefficients
  cov_xy <- cov(x, y)
  A_star <-
    cov_x_inv_sqrt %*% cov_xy %*% solve(cov_y) %*% t(cov_xy) %*% cov_x_inv_sqrt
  A <- eigen(A_star)
  can_cor <- sqrt(A$values)

  # Remove any NaN values from the canonical correlations
  can_cor <- can_cor[!is.nan(can_cor)]

  return(list(
    cor = can_cor,
    xcoef = x_coef,
    ycoef = y_coef
  ))
}
