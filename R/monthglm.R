## monthglm.R
## fit a GLM using month as a factor
## option to add offset to control for uneven number of days
## March 2009

monthglm<-function(formula,data,family=gaussian(),refmonth=1,
                   offsetmonth=FALSE,offsetpop=NULL){
  attach(data,warn.conflicts=FALSE)
  ## checks
  n<-names(data)
  if (any(n=='month')==FALSE){stop("data set must contain months as integer using a variable called 'month'")}
### was  if (is.integer(month)==FALSE&is.numeric(month)==FALSE){stop("month variable must be an integer or numeric")}
    if (is.integer(data$month)==FALSE&is.numeric(data$month)==FALSE){stop("month variable must be an integer or numeric")}  # GUESS ONLY
  if (refmonth<1|refmonth>12){stop("Reference month must be between 1 and 12")}
  ## original call with defaults (see amer package)
  ans <- as.list(match.call())
  frmls <- formals(deparse(ans[[1]]))
  add <- which(!(names(frmls) %in% names(ans)))
  call<-as.call(c(ans, frmls[add]))
  ## Transform month numbers to names
### was  months.u<-as.factor(month)
  months.u<-as.factor(data$month)  
  nums<-as.numeric(nochars(levels(months.u))) # Month numbers
  levels(months.u)[nums]<-month.abb[nums]
  months<-relevel(months.u,ref=month.abb[refmonth]) # set reference month
  ## prepare data/formula
  parts<-paste(formula)
  f<-as.formula(paste(parts[2],parts[1],parts[3:length(formula)],'+months'))
  dep<-parts[2] # dependent variable
  index<-sum((names(data)==dep)*(1:ncol(data)))
  slimdata<-data[,index]
  days<-flagleap(data=data,report=FALSE,matchin=T) # get the number of days in each month
  l<-nrow(data)
  if(is.null(offsetpop)==FALSE){poff=offsetpop} else{poff=rep(1,l)}
  if(offsetmonth==TRUE){moff=days$ndaysmonth/(365.25/12)} else{moff=rep(1,l)} # days per month divided by average month length
###  data$off<-log(poff*moff)
  off<-log(poff*moff)  # 
  fit<-glm(formula=f,data=data,family=family,offset=off)
  detach(data)
  ## return
  toret<-list()
  toret$call<-call
  toret$glm<-fit
  toret$fitted.values<-fitted(fit)
  toret$residuals<-residuals(fit)
  class(toret)<-'monthglm'
  return(toret)
}
