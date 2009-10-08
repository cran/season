\name{plotCircular}
\alias{plotCircular}
\title{Circular plot}
\description{A circular plot useful for visualising monthly or weekly data.}
\usage{plotCircular(radii1,radii2=NULL,spokes=NULL,scale=0.8,labels,
stats=TRUE,dp=1,clockwise=TRUE,spoke.col='black',lines=FALSE,\dots)
}
\arguments{
  \item{radii1}{variable to plot, the radii of the segments (or petals) are proportional to this variable.}
  \item{radii2}{2nd variable to plot (optional), the radii of the segments are plotted in gray.}
  \item{spokes}{spokes that overlay segments, for example standard errors (optional, should have the same length as \code{radii1}).}
  \item{scale}{scale the overall size of the segments (default=0.8).}
  \item{labels}{optional labels to appear at the ends of the segments (should have the same length as \code{radii1}).}
  \item{stats}{put radii values at the ends of the segments (TRUE/FALSE).}
  \item{dp}{decimal places for statistics, default=1.}
  \item{clockwise}{plot in a clockwise direction, default=TRUE.}
  \item{spoke.col}{spoke colour, default=black.}
  \item{lines}{add dotted lines to separate petals, default=FALSE.}
  \item{\dots}{additional arguments to \code{plot}}
}
\details{A circular plot can be useful for spotting the shape of the seasonal pattern. This function can be used to plot any circular patterns, e.g., weekly or monthly. The number of segments will be the length of the variable \code{radii1}.}
\references{Fisher, N.I. (1993) \emph{Statistical Analysis of Circular Data}. Cambridge University Press, Cambridge.}
\author{Adrian Barnett \email{a.barnett@qut.edu.au}}
\examples{
# months (dummy data)
plotCircular(radii1=seq(1,12,1),scale=0.7,labels=month.abb,dp=0)
# weeks (random data)
daysoftheweek<-c('Monday','Tuesday','Wednesday','Thursday','Friday',
'Saturday','Sunday')
weekfreq<-table(round(runif(100,min=1,max=7)))
plotCircular(radii1=weekfreq,labels=daysoftheweek,dp=0)
# Observed number of AFL players with expected values
data(AFL)
plotCircular(radii1=AFL$players,radii2=AFL$expected,scale=0.72,labels=month.abb,
dp=0,lines=TRUE)
}
