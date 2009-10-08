## summary.casecross.R
## Summarise results from casecross
## Oct 2009
summary.casecross<-function(object, ...){

  ## Check
  if (class(object)!="casecross"){stop("Object must be of class 'casecross'")} 

  ## output results
  cat('Time-stratified case-crossover with a stratum length of',
      object$call$stratalength,'days\n')
  if(object$call$matchdow==TRUE){cat('Matched on day of the week\n')}
  if(object$call$matchconf!=''){
    cat('Matched on',object$call$matchconf,'plus/minus',
        object$call$confrange,'\n')
  }
  cat('Number of case days with available control days',object$ncases,'\n')
  cat('Average number of control days per case day',object$ncontrols,'\n')
  cat('\nParameter Estimates:\n')
  s<-summary(object$c.model)
  print(s$coef, ...)
}
