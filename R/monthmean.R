# monthmean.R
# function to calculate monthly mean or adjusted monthly mean for count data
# Assumes that data contains two variables: month and year
# November 2009

monthmean<-function(data,resp,pop=NULL,adjmonth=FALSE){
  attach(data,warn.conflicts=FALSE)
# checks
  if (is.null(data)==TRUE){stop("must have an input data set (data)")}
  if (is.null(resp)==TRUE){stop("must have an input variable (resp)")}
  nnn<-names(data)
  if (any(nnn=='year')==FALSE){stop("data set must contain a variable with the 4 digit year called 'year'")}
  if (any(nnn=='month')==FALSE){stop("data set must contain a variable with the numeric month called 'month'")}
# calculations
  days<-flagleap(data) # get the number of days in each month
  mean<-vector(length=12,mode='numeric')
  if (adjmonth=='thirty') adjf=30  
  if (adjmonth=='average') adjf=365.25/12
  if (is.null(pop)==TRUE) adjp=1 else adjp=pop # population adjustment
  for (i in 1:12){
     if (adjmonth!=FALSE) mean[i]<-mean(resp[data$month==i]*(adjf/days$ndaysmonth[i])/adjp)
     else mean[i]<-mean(resp[data$month==i]/adjp) # no monthly adjustment
  }
  detach(data)
# return
  toret<-list()
  toret$mean<-as.vector(mean)
  class(toret)<-'Monthmean'
  return(toret)
}

# example
# mmean<-monthmean(data=CVD,resp=cvd,pop=pop/100000,plot=TRUE)
# adjmean<-monthmean(data=elderly,resp=cvd,adjmonth='average',pop=pop/100000,plot=TRUE)
