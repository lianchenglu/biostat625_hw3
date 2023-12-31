% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hw3cca.R
\name{cca_simple}
\alias{cca_simple}
\title{Simple Canonical Correlation Analysis}
\usage{
cca_simple(x, y)
}
\arguments{
\item{x}{is a matrix/dataframe where each column is a variable and each row is an observation.}

\item{y}{is another matrix/dataframe where each column is a variable and each row is an observation.}
}
\value{
A list containing:
- "cor": The canonical correlation coefficients
- "xcoef": The canonical vector of x standardized
- "ycoef": The canonical vector of y standardized
}
\description{
This function conducts the simple Canonical Correlation Analysis (CCA) between two sets of variables.
}
\examples{
x <- matrix(rnorm(2000), 1000, 2)
y <- matrix(rnorm(2000), 1000, 2)
res <- cca_simple(x, y)
print(res)
x <- matrix(rnorm(200000), 40000, 5)
y <- matrix(rnorm(200000), 40000, 5)
res <- cca_simple(x, y)
print(res)

x <- matrix(rnorm(2000000), 400000, 5)
y <- matrix(rnorm(2000000), 400000, 5)
res <- cca_simple(x, y)
print(res)

system.time(cca_simple(x,y)) # Time of my function
system.time(cancor(x,y)[1:3]) # Time of the R build-in cca function
}
