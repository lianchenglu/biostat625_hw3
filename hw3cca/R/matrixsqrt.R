matrixsqrt <-
function(m) {
  eig <- eigen(m)
  Q <- eig$vectors
  rsqrtD <- sqrt(eig$values)
  return(Q %*% diag(rsqrtD) %*% t(Q))
}
