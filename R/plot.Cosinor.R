## plot.Cosinor.R
## Oct 2009
## NB: Needs a rewrite at some stage - ... is not used so
##     plot not as flexible as it might be

plot.Cosinor<-function(x,...){
## Checks
  if (class(x)!="Cosinor"){stop("Object must be of class 'Cosinor'")} 
  op <- par(no.readonly = TRUE) # the whole list of settable par's.
  f<-as.formula(x$call$formula)
  parts<-paste(f)
  ylab<-parts[2]
## residual check ##
  seasrescheck(x$residuals)
## plot sinusoid ##
  dev.new()          
  o<-order(x$date)
  par(mai=c(0.8,0.8,0.1,0.1)) # c(bottom, left, top, right)
  if (x$call$link!='logit'&x$call$link!='cloglog'){
     plot(x$date[o],x$fitted.values[o],type='l',xaxt='n',xlab='Time',ylab=ylab,...)
     if (x$call$type=='monthly'){
        m.abb=substr(month.abb,1,1)
        axis(side=1,at=1:12,labels=m.abb)
        points(x$date[o],x$fitted.values[o],pch=19)
     }
     if (x$call$type=='daily'){
        years<-as.numeric(names(table(format(x$date,'%Y'))))
        firsts<-as.numeric(ISOdate(month=1,day=1,year=years))/(24*60*60)
        axis(side=1,at=firsts,labels=years)
        rug(x$date[o])
     }
  }
  if (x$call$link=='logit'|x$call$link=='cloglog'){
     ylab<-paste('Probability(',ylab,')',sep='')
     plot(x$date[o],x$fitted.values[o],type='l',xaxt='n',col='black',xlab='Time',ylab=ylab,...)
     if (x$call$type=='monthly'){
        m.abb=substr(month.abb,1,1)
        axis(side=1,at=1:12,labels=m.abb)
        points(x$date[o],x$fitted.values[o],pch=19)
     }
     if (x$call$type=='daily'){
        years<-as.numeric(names(table(format(x$date,'%Y'))))
        firsts<-as.numeric(ISOdate(month=1,day=1,year=years))/(24*60*60)
        axis(side=1,at=firsts,labels=years)
        rug(x$date[o])
     }
  }
  par(op) # restore graphic settings
}
