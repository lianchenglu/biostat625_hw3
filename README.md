
# hw3cca

<!-- badges: start -->
[![R-CMD-check](https://github.com/lianchenglu/biostat625_hw3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lianchenglu/biostat625_hw3/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of hw3cca is to replicate some of the functionality of the existing R package. 
This is a simple method to calculate CCA. It is designed to calculate the CCA when the number of input X and Y columns is the same. The correlation generated is correct when the number of columns is different, but the number of rows or columns for the correlation coefficient will be different.

## Installation

You can install the development version of hw3cca from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lianchenglu/biostat625_hw3")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(hw3cca)
## basic example code
set.seed(123)
x <- matrix(rnorm(2000), 1000, 2)
y <- matrix(rnorm(2000), 1000, 2)
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
```

