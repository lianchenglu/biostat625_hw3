library(hw3cca)
set.seed(123)

x <- matrix(rnorm(2000), 1000, 2)
y <- matrix(rnorm(2000), 1000, 2)
cca_simple(x, y)
cancor(x,y)[1:3]

x <- matrix(rnorm(3000), 1000, 3)
y <- matrix(rnorm(2000), 1000, 2)
cca_simple(x, y)
cancor(x,y)[1:3]

x <- matrix(rnorm(20000), 10000, 2)
y <- matrix(rnorm(30000), 10000, 3)
cca_simple(x, y)
cancor(x,y)[1:3]

x <- matrix(rnorm(200000), 40000, 5)
y <- matrix(rnorm(200000), 40000, 5)
cca_simple(x, y)
cancor(x,y)[1:3]


x <- matrix(rnorm(200000), 5000, 40)
y <- matrix(rnorm(200000), 5000, 40)
cca_simple(x, y)
cancor(x,y)[1:3]


x <- matrix(rnorm(2000000), 400000, 5)
y <- matrix(rnorm(2000000), 400000, 5)
cca_simple(x, y)
cancor(x,y)[1:3]
system.time(cca_simple(x,y))
system.time(cancor(x,y)[1:3])
