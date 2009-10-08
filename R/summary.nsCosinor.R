## summary.nsCosinor.R
## Summarise results from nsCosinor
summary.nsCosinor<-function(object, ...){

  ## Checks
  if (class(object)!="nsCosinor"){stop("Object must be of class 'nsCosinor'")} 

  ## basic variables
  k<-length(object$cycles);
  cycles<-object$cycles;

  ## Statistics ###
  s.std.error<-summary(object$chains$std.error)
  l<-as.numeric(s.std.error$quantiles[1])
  u<-as.numeric(s.std.error$quantiles[5])
  m<-as.numeric(s.std.error$statistics[1])
  errorstats<-c(m,l,u)
  if(k==1){
    s.std.season<-summary(object$chains$std.season)
    wstats<-matrix(ncol=1,nrow=k)
    l<-as.numeric(s.std.season$quantiles[1])
    u<-as.numeric(s.std.season$quantiles[5])
    m<-as.numeric(s.std.season$statistics[1])
    wstats<-c(m,l,u)
    ampstats<-matrix(ncol=1,nrow=k)
    phasestats<-matrix(ncol=1,nrow=k)
    s.amp<-summary(object$chains$amplitude)
    l<-as.numeric(s.amp$quantiles[1])
    u<-as.numeric(s.amp$quantiles[5])
    m<-as.numeric(s.amp$statistics[1])
    ampstats<-c(m,l,u)
    pstat<-ciPhase(as.vector(object$chains$phase))
    phasestats<-c(pstat$mean,pstat$lower,pstat$upper)
  } # end of k=1
  if(k>=2){
    s.std.season<-summary(object$chains$std.season)
    wstats<-matrix(ncol=3,nrow=k)
    for (index in 1:k){ 
      l<-as.numeric(s.std.season$quantiles[index,1])
      u<-as.numeric(s.std.season$quantiles[index,5])
      m<-as.numeric(s.std.season$statistics[index,1])
      wstats[index,]<-c(m,l,u)
    }
    ampstats<-matrix(ncol=3,nrow=k)
    phasestats<-matrix(ncol=3,nrow=k)
    s.amp<-summary(object$chains$amplitude)
    for (index in 1:k){ 
      l<-as.numeric(s.amp$quantiles[index,1])
      u<-as.numeric(s.amp$quantiles[index,5])
      m<-as.numeric(s.amp$statistics[index,1])
      ampstats[index,]<-c(m,l,u)
      phase.chain<-as.vector(object$chains$phase[,index])
      pstat<-ciPhase(phase.chain)
      phasestats[index,]<-c(pstat$mean,pstat$lower,pstat$upper)
    }
  } # end of k>=2

  ## store the statistics
  ret<-list()
  ret$cycles<-cycles
  ret$niters<-object$call$niters
  ret$burnin<-object$call$burnin
  ret$tau<-object$call$tau
  ret$stats$errorstats<-errorstats
  ret$stats$wstats<-wstats
  ret$stats$ampstats<-ampstats
  ret$stats$phasestats<-phasestats
  class(ret)<-'summary.nsCosinor'
  ret # uses print.summary.nsCosinor
} # end of function
