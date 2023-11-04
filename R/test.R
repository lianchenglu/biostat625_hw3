library(hw3cca)
set.seed(123)
x <- matrix(rnorm(30), 10, 2)
y <- matrix(rnorm(20), 10, 2)
cca_simple(x, y)
cancor(x,y)[1:3]


x <- matrix(rnorm(3000), 1000, 2)
y <- matrix(rnorm(2000), 1000, 2)
cca_simple(x, y)
cancor(x,y)[1:3]


x <- matrix(rnorm(200000), 40000, 5)
y <- matrix(rnorm(200000), 40000, 5)
cca_simple(x, y)
cancor(x,y)[1:3]


x <- matrix(rnorm(200000), 4000, 50)
y <- matrix(rnorm(200000), 4000, 50)
res1 <- cca_simple(x, y)
print(res1)
cancor(x,y)


x <- matrix(rnorm(2000000), 400000, 5)
y <- matrix(rnorm(2000000), 400000, 5)
res1 <- cca_simple(x, y)
print(res1)
system.time(cca_simple(x,y))
system.time(cancor(x,y))
