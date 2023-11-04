\name{cca1}
\alias{cca1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cca1(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y) 
{
    x <- scale(x, center = TRUE, scale = FALSE)
    y <- scale(y, center = TRUE, scale = FALSE)
    cov_xy <- t(x) \%*\% y/(nrow(x))
    cov_xx <- cov(x)
    cov_yy <- cov(y)
    eigcovxx <- eigen(cov_xx)
    Qxx <- eigcovxx$vectors
    rsqrtDxx <- sqrt(eigcovxx$values)
    cov_xx_inv_sqrt <- solve(Qxx \%*\% diag(rsqrtDxx) \%*\% t(Qxx))
    eigcovyy <- eigen(cov_yy)
    Qyy <- eigcovyy$vectors
    rsqrtDyy <- sqrt(eigcovyy$values)
    cov_yy_inv_sqrt <- solve(Qyy \%*\% diag(rsqrtDyy) \%*\% t(Qyy))
    P <- cov_xx_inv_sqrt \%*\% cov_xy \%*\% cov_yy_inv_sqrt
    svd_decomposition <- svd(P)
    x_coef <- cov_xx_inv_sqrt \%*\% svd_decomposition$u/sqrt(nrow(x) - 
        1)
    y_coef <- cov_yy_inv_sqrt \%*\% svd_decomposition$v/sqrt(nrow(y) - 
        1)
    cov_xy <- cov(x, y)
    A_star <- cov_xx_inv_sqrt \%*\% cov_xy \%*\% solve(cov_yy) \%*\% 
        t(cov_xy) \%*\% cov_xx_inv_sqrt
    A <- eigen(A_star)
    can_cor <- sqrt(A$values)
    return(list(can_cor = can_cor, x_coef = x_coef, y_coef = y_coef))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
