print.summary.Cosinor <- function(x, ...){
  ## report results
  if (class(x)!="summary.Cosinor"){
    stop("Object must be of class 'summary.Cosinor'")
  } 

  cat('Cosinor test\n')
  cat('Number of observations =',x$n,'\n')
  cat('Amplitude =',x$amp,x$amp.scale,'\n')
  cat('Phase:',x$phase,'\n')
  cat('Low point:',x$lphase,'\n')
  cat('Significant seasonality based on adjusted significance level of',
      x$alpha/2,' = ',x$significant,'\n', ...)
}
