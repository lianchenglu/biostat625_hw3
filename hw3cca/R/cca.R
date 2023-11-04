cca <-
function(x, y) {
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
    can_cor = can_cor,
    x_coef = x_coef,
    y_coef = y_coef
  ))
}
