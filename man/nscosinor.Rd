\name{nscosinor}
\alias{nscosinor}
%%\alias{print.nsCosinor}
%%\alias{summary.nsCosinor}
%%\alias{plot.nsCosinor}
\title{Non-stationary cosinor}
\description{Decompose a time series using a non-stationary cosinor for the seasonal pattern.}
\usage{nscosinor(data,response,cycles,niters=1000,burnin=500,tau,inits,lambda=1/12,
div=50,monthly=TRUE)

%%\method{print}{nsCosinor}(x, ...)
%%\method{summary}{nsCosinor}(x, ...)
%%\method{plot}{nsCosinor}(x, ...)
}
\arguments{
  \item{data}{a data frame.}
  \item{response}{response variable.}
  \item{cycles}{vector of cycles in units of time, e.g., for a six and twelve month pattern \code{cycles=c(6,12)}.}
  \item{niters}{total number of MCMC samples (default=1000).}
  \item{burnin}{number of MCMC samples discarded as a burn-in (default=500).}
  \item{tau}{vector of smoothing parameters, tau[1] for trend, tau[2] for 1st seasonal parameter, tau[3] for 2nd seasonal parameter, etc.}
  \item{inits}{vector of initial values for the MCMC chain.}
  \item{lambda}{distance between observations (lambda=1/12 for monthly data, default).}
  \item{div}{divisor at which MCMC sample progress is reported (default=50).}
  \item{monthly}{TRUE for monthly data.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{This model is designed to decompose an equally spaced time series into a trend, season(s) and noise. 
A seasonal estimate is estimated as \eqn{s_t=A_t\cos(\omega_t-P_t)},
where \emph{t} is time, \eqn{A_t} is the non-stationary amplitude,
\eqn{P_t} is the non-stationary phase and \eqn{\omega_t} is the
frequency.

A non-stationary seasonal pattern is one that changes over time, hence
this model gives potentially very flexible seasonal estimates.

The frequency of the seasonal estimate(s) are controlled by \code{cycle}. 
The cycles should be specified in units of time.
If the data is monthly, then setting \code{lambda=1/12} and \code{cycles=12} will fit an annual seasonal pattern.
If the data is daily, then setting \code{lambda=} \code{1/365.25} and \code{cycles=365.25} will fit an annual seasonal pattern.
Specifying \code{cycles=} \code{c(182.6,365.25)} will fit two seasonal patterns, one with a twice-annual cycle, and one with an annual cycle.

The estimates are made using a forward and backward sweep of the Kalman filter. 
Repeated estimates are made using Markov chain Monte Carlo (MCMC).
For this reason the model can take a long time to run (we aim to improve this in the next version).
To give stable estimates a reasonably long sample should be used (\code{niters}), and the possibly poor initial estimates should be discarded (\code{burnin}).
}
\value{
Returns an object of class \dQuote{nsCosinor} with the following parts:
\item{call}{the original call to the nscosinor function.} 
\item{time}{the year and month for monthly data.} 
\item{trend}{mean trend and 95\% confidence interval.} 
\item{season}{mean season(s) and 95\% confidence interval(s).} 
\item{oseason}{overall season(s) and 95\% confidence interval(s). This will be the same as \code{season} if there is only one seasonal cycle.} 
\item{fitted}{fitted values, based on trend + season(s).} 
\item{residuals}{residuals based on mean trend and season(s).}
\item{n}{the length of the series.}
\item{chains}{MCMC chains (of class \code{\link[coda]{mcmc}}) of variance estimates: standard error for overall noise (std.error), phase, amplitude, standard error for season(s) (std.season).}
\item{cycles}{vector of cycles in units of time.}
}
\references{Barnett, A.G., Dobson, A.J. (2010) \emph{Analysing Seasonal Health Data}. Springer.

Barnett, A.G., Dobson, A.J. (2004) Estimating trends and seasonality in coronary heart disease \emph{Statistics in Medicine}. 23(22) 3505--23.}
\author{Adrian Barnett \email{a.barnett@qut.edu.au}}
\seealso{\code{plot.nsCosinor}, \code{summary.nsCosinor}}
\examples{
data(CVD)
# adjust the number of CVD deaths to account for 
# the unequal number of deaths in the months
CVD$adj<-CVD$cvd*((365.25/12)/CVD$ndaysmonth)
# model to fit an annual pattern to the monthly cardiovascular disease data
f<-c(12)
inits<-c(1)
tau<-c(130,10)
\dontrun{res12<-nscosinor(data=CVD,response=adj,cycles=f,niters=5000,
burnin=1000,tau=tau,inits=inits)
summary(res12)
plot(res12)
plot(res12$chains$amp)}
\dontshow{res12<-nscosinor(data=CVD,response=adj,cycles=f,niters=50,burnin=10,tau=tau,inits=inits)}
}
