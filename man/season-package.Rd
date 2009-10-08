\name{season-package}
\alias{season-package}
\alias{season}
\docType{package}
\title{Tools for uncovering and estimating seasonal patterns}
\description{
The package contains graphical methods for displaying seasonal data and
regression models for detecting and estimating seasonal patterns. The
regression models can be applied to normal, Poisson or binomial
dependent data distributions. Tools are available for both time series
data (equally spaced in time) and survey data (unequally spaced in
time).


Sinusoidal (parametric) seasonal patterns are available
(\code{\link{cosinor}}, \code{\link{nscosinor}}), as well as models for
monthly data (\code{\link{monthglm}}), and the case-crossover method to
control for seasonality (\code{\link{casecross}}).

%More about what it does (maybe more than one line)
%~~ A concise (1-5 lines) description of the package ~~
}
\details{
\code{season} aims to fill an important gap in the \R software by providing a range of tools for analysing seasonal data. The examples are based on health data, but the functions are equally applicable to any data with a seasonal pattern.

In the next version we hope to make the casecross and nscosinor functions faster.

\tabular{ll}{
Package: \tab season\cr
Type: \tab Package\cr
Version: \tab 0.2-2\cr
Date: \tab 2009-10-20\cr
License: \tab GPL (>= 3)\cr
}
%~~ An overview of how to use the package, including the most important ~~
%~~ functions ~~
}
\author{Adrian Barnett <a.barnett@qut.edu.au>\cr
Peter Baker <p.baker1@uq.edu.au>

Maintainer: Adrian Barnett <a.barnett@qut.edu.au>
}
\references{Barnett, A.G., Dobson, A.J. (2010) \emph{Analysing Seasonal Health Data}. Springer.}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%~~ the R documentation directory ~~
\keyword{ package
ts
models}
\examples{
%%## see examples for \code{\link{cosinor}}, \code{\link{monthglm}},
%%\code{\link{nscosinor}} and \code{\link{casecross}}
## see examples for \link{cosinor}, \link{monthglm}, \link{nscosinor} and \link{casecross}
}
