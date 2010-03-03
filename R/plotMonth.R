# plotMonth.R
# function to plot monthly results by month
# assumes the data are in order
# assumes there are numeric variables for month and year

plotMonth<-function(data,resp,panels=12, ...){
  cat("Using this one!!!\n")
  if (panels!=1&panels!=12){stop("panels must be 1 or 12")}
  op <- par(no.readonly = TRUE) # the whole list of settable par's.
  attach(data, warn.conflicts = FALSE)
                                        # version with 1 panel
  if (panels==1){
    range<-c(min(resp,na.rm=TRUE),max(resp,na.rm=TRUE)*1.1) # increase max range to accomodate legend
    par(lwd=2,mai=c(0.80,0.82,0.12,0.22)) # c(bottom, left, top, right)
    select<-data$month==1
    plot(year[select],resp[select],lty=1,type="l",main="",
         xlab="Year",ylab="",ylim=range,bty='n')
    box(lwd=1)
    for (i in 2:12){
      select<-data$month==i
      lines(year[select],resp[select],lty=i,type="l")
    }
    leg.txt<-substr(month.name,1,1)
    legend(xjust=0.5,x=(min(year)+max(year))/2,
           y=max(resp,na.rm=TRUE)*1.1,leg.txt,lty=(1:12),
           ncol=6,box.lwd=1,cex=0.79)
  } # end of panels if
  ## version with 12 panels
  if (panels==12){
    ## monname<-c('January','February','March','April','May',
    ##           'June','July','August','September','October',
    ##           'November','December')
    monname <- month.name
    par(mfrow=c(3,4),lwd=2,mai=c(0.10,0.12,0.42,0.12)) # c(bottom, left, top, right)
    for (i in 1:12){
      select<-data$month==i
      plot(year[select],resp[select],lty=1,type="l",main=month.name[i],
           xlab="",ylab="",xaxt='n',yaxt='n',bty='n', ...)
      box(lwd=1)
    }
  } # end of panels if
  detach(data)
  par(op) # restore graphic settings
}
