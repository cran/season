# invyrfraction.R
# convert fraction of the year into a date (day and month)
# month on a scale of [1,13)
# type =  monthly/daily
# April 2009

invyrfraction<-function(frac,type='daily',text=TRUE){
  n<-length(frac)
  if(sum(frac<0)+sum(frac>1)>0){stop('Fraction must be between 0 and 1')}
  if (type=='daily'){
     yrlength<-365.25; 
     day<-(frac*yrlength)+1;
     day=day-(365*as.numeric(day>365)); # avoid values > 365
     day=max(day,1); # avoid values < 1
     date<-strptime(day,'%j');
     day<-as.numeric(format(date,'%d')); # Day of the month as decimal number (01–31)
     month<-format(date,'%B'); # Month name
     if (text==TRUE){daym<-paste('Month =',month,', day =',day)}
     if (text==FALSE){
        monthnum<-as.numeric(format(date,'%m')); # Month number
        if (any(c(1,3,5,7,8,10,12)==monthnum)){mnthlength=31}
        if (any(c(4,6,9,11)==monthnum)){mnthlength=30}
        if (monthnum==2){mnthlength=29} # assume leap year
        daym<-monthnum+((day-1)/mnthlength)
     } # 
  }
  if (type=='monthly'){
     month<-(frac*12)+1;
     if (text==TRUE){daym<-paste('Month =',month)}
     if (text==FALSE){daym<-month}
  }
  return(daym)
}

