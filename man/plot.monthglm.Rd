\name{plot.monthglm}
%%\alias{monthglm}
\alias{plot.monthglm}
\title{Plot of monthly estimates}
\description{Plots the estimated from a generalized linear model with a categorical independent variable of month.}
\usage{
\method{plot}{monthglm}(x,...)
}
\arguments{
  \item{x}{a \code{monthglm} object produced by \code{monthglm}.}
  \item{\dots}{additional arguments passed to the plot.}
}
\seealso{\code{monthglm}}
\author{Adrian Barnett \email{a.barnett@qut.edu.au}}
\examples{
data(CVD)
mmodel<-monthglm(formula=cvd~1,data=CVD,family=poisson(),
offsetpop=pop/100000,offsetmonth=TRUE,refmonth=6)
plot(mmodel)
}
