## plot.nsCosinor.R
## Plots results from nsCosinor
plot.nsCosinor<-function(x, ...){

  op <- par(no.readonly = TRUE) # the whole list of settable par's.

  ## basic variables
  cycles<-x$cycles;
  k<-length(cycles);
  month<-(12*(x$time-floor(x$time)))+1
  ## Residuals plots
  par(mfrow=c(2,2))
  if(x$call$monthly==FALSE){
    par(lwd=1,mai=c(0.32,0.62,0.82,0.12),cex=0.7) # c(bottom, left, top, right)
    plot(x$time,x$residual,main="Time series",ylab="Residual") # 
  }
  if(x$call$monthly==TRUE){
    par(lwd=1,mai=c(0.32,0.62,0.82,0.12),cex=0.7) # c(bottom, left, top, right)
    boxplot(x$residual~month,boxwex=0.4,col="grey90",xaxt='n',
            xlab="",ylab="Residual",main="Residuals by month")
    m.abb<-substr(month.abb,1,1)
    axis(side=1,at=1:12,label=m.abb)
  }
  par(lwd=1,mai=c(0.32,0.32,0.82,0.12),cex=0.7) # c(bottom, left, top, right)
  cpgram(x$residual,main="Cumulative periodogram")
  par(lwd=1,mai=c(0.32,0.62,0.82,0.12),cex=0.7) # c(bottom, left, top, right)
  hist(x$residual, col="grey80", freq=FALSE,main='Residual histogram') # Residual histogram
  qqnorm(x$residual) # Q-Q plot
  qqline(x$residual)
  ## Trend and season plots
  dev.new()          
  par(mfrow=c(k+1,1),las=1, mar=c(4, 4, 2, 4), col.axis="black")
                                        # season
  smat<-as.matrix(x$season)
  par(mai=c(0.1,0.8,0.1,0.1)) # c(bottom, left, top, right)
  for (index in 1:k){
    mean<-smat[,(index*3)-2]
    lower<-smat[,(index*3)-1]
    upper<-smat[,(index*3)]
    ylim=c(min(lower),max(upper))
    plot(x$time,mean,type='l',col="black",ylim=ylim,xaxt='n',ylab='',xlab='')
    lines(x$time,lower,type='l',col="grey50")
    lines(x$time,upper,type='l',col="grey50")
    box(bty="u")
    ytext<-paste("Season, cycle=",cycles[index],sep="")
    mtext(ytext, side=2, line=3,las=0,col='black')
  }
                                        # trend
  par(mai=c(0.6,0.8,0.1,0.1)) # c(bottom, left, top, right)
  rangey<-c(x$trend$mean,x$trend$lower,x$trend$upper)
  ylim=range(rangey);
  plot(x$time,x$trend$mean,type='l',col="black",xlab='',ylab='',bty='u',ylim=ylim)
  lines(x$time,x$trend$lower,type='l',col="grey50")
  lines(x$time,x$trend$upper,type='l',col="grey50")
  mtext("Trend", side=2, line=3,las=0,col='black')
  ## Plot MCMC chains (turned off)
  plot.mcmc=FALSE
  if(plot.mcmc==TRUE){
    dev.new()          
    xaxis<-(x$call$burnin:x$call$niters)
    par(mfrow=c(2,1),las=1, mar=c(4, 4, 2, 4), col.axis="black")
    plot(xaxis,x$chains$std.error[(x$call$burnin+1):(x$call$niters+1)],type='l',xlab="",ylab="std(Error)",lwd=2,col=5,bty="n")
    plot.new()
    plot.window(c(x$call$burnin,x$call$niters),range(x$chains$std.season[(x$call$burnin+1):(x$call$niters+1),1:k]))
    for (index in 1:k){
      lines(xaxis,x$chains$std.season[(x$call$burnin+1):(x$call$niters+1),index],col=index+1,lwd=2)
    }
    xticks<-xscale.components.default(c(x$call$burnin,x$call$niters)) # From lattic
    yticks<-xscale.components.default(c(min(x$chains$std.season[(x$call$burnin+1):(x$call$niters+1),1:k]),max(x$chains$std.season[(x$call$burnin+1):(x$call$niters+1),1:k]))) # From lattic
    axis(1, at=xticks$bottom$ticks$at)
    axis(2, at=yticks$bottom$ticks$at)
    mtext("std(season)", side=2, line=3,las=0,col='black')
    mtext("Iteration", side=1,line=3,las=0,col='black')
    legend((x$call$niters-x$call$burnin)/2, max(x$chains$std.season[(x$call$burnin+1):(x$call$niters+1),1:k]),legend=cycles, bty="y",col=2:(k+1),lty=1,lwd=2,horiz=TRUE,title="Cycle=")
  }
  par(op) # restore graphic settings
}
