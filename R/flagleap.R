# flagleap.R
# function to flag leap years/months in a range of dates
# March 2009, updated April 2010

flagleap<-function(data,report=TRUE,matchin=FALSE){
  attach(data,warn.conflicts=FALSE)
  n<-nrow(data)
  yrmon<-year+((month-1)/12); # used later
  startyr<-min(data$year)
  stopyr<-max(data$year)
  startdate<-as.numeric(ISOdate(startyr,1,1))/(60*60*24) # start on 1st Jan
  stopdate<-as.numeric(ISOdate(stopyr,12,31))/(60*60*24) # stop on 31st Dec
  ndays=stopdate-startdate+1
  if (report==TRUE){cat("Total number of days = ", ndays, "\n")}
  z<-vector(length=ndays,mode='numeric')
  index<-0
  for (i in startdate:stopdate){ # loop through start to stop dates in days
     index<-index+1
     z[index]<-i*(60*60*24) # convert to seconds
  }
  conv<-as.POSIXct(z,origin="1970-01-01") # convert to a date
  ndaysyear<-table(format(conv,'%Y')) # number of days per year
  ndaysmonth<-table(format(conv,'%Y/%m')) # number of days per month (per year)
  year<-as.numeric(substr(names(ndaysmonth),1,4))
  month<-as.numeric(substr(names(ndaysmonth),6,7))
  days<-as.data.frame(cbind(year,month,as.numeric(ndaysmonth)))
  names(days)<-c('year','month','ndaysmonth')
# Match start and end times
  if(matchin==TRUE){
     yrmon.days<-days$year+((days$month-1)/12);
     index<-yrmon.days>=min(yrmon)&yrmon.days<=max(yrmon);
     cat('Number of index times ',sum(index),'\n');
     days<-days[index,]
  }
# Finish
  detach(data)
  return(days)
}

# example
# days<-flagleap(data=elderly)


