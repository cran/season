\name{plot.Cosinor}
%%\alias{cosinor}
\alias{plot.Cosinor}
\title{Plot the results of a cosinor}
\description{Plots the residuals and fitted sinusoid from a \code{Cosinor} object produced by \code{cosinor}.}
\usage{
\method{plot}{Cosinor}(x,...)
}
\arguments{
  \item{x}{a \code{Cosinor} object produced by \code{cosinor}.}
  \item{\dots}{additional arguments passed to the sinusoid plot.}
}
\details{The code prodcudes the following two plots: a) residual checks using \code{seasrescheck}, b) the fitted sinusoid based on the intercept and sinusoid. 
The y-axis is on the scale of probability if the link function is \sQuote{logit} or \sQuote{cloglog}. If the analysis was based on monthly data then month is shown on the x-axis. If the analysis was based on daily data then time is shown on the x-axis.}
\seealso{\code{cosinor}, \code{summary.Cosinor}, \code{seasrescheck}}
\author{Adrian Barnett \email{a.barnett@qut.edu.au}}

