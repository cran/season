# cosinor.R
# cosinor function using a GLM
# available link functions = identity, log, logit, cloglog
# date = date for daily data, month for monthly data
# type =  monthly/daily
# phase results based on 1 cycle per year
# Oct 2011

cosinor<-function(formula, date, data,family=gaussian(), alpha=0.05,
                  cycles=1, rescheck=FALSE, type='daily', offsetmonth=FALSE,
                  offsetpop=NULL,text=TRUE){

  ## checks
  if (!is.logical(offsetmonth)){
    stop("Error: 'offsetmonth' must be of type logical")}
  if (type!='daily'&type!='monthly'){stop("type must be daily or monthly")}
  attach(data,warn.conflicts=FALSE)
  on.exit(detach(data))
  if (type=='daily'&class(date)!='Date'){
    stop("date variable must be of class Date when type='daily'")}
  if (alpha<=0|alpha>=1){stop("alpha must be between 0 and 1")}

  ## original call with defaults (see amer package)
  link<-family$link
  ans <- as.list(match.call())
  frmls <- formals(deparse(ans[[1]]))
  add <- which(!(names(frmls) %in% names(ans)))
  call<-as.call(c(ans, frmls[add],link=link))

  ## make the formula
  parts<-paste(formula)
  f<-as.formula(paste(parts[2],parts[1],parts[3:length(formula)],'+cosw+sinw'))
  dep<-parts[2] # dependent variable
  index<-sum((names(data)==dep)*(1:ncol(data)))
  slimdata<-data[,index]

  ## get the year fraction
  frac<-yrfraction(date,type=type)
  data$cosw<-cos(frac*2*pi*cycles)
  data$sinw<-sin(frac*2*pi*cycles)
  newdata<-data.frame(cosw=data$cosw,sinw=data$sinw) # used later
  poff=rep(1,nrow(data))
  if(is.null(offsetpop)==FALSE){poff=offsetpop}
  moff=rep(1,nrow(data))
  if(offsetmonth==TRUE){
    days<-flagleap(data=data,report=FALSE) # get the number of days in each month
    moff=days$ndaysmonth/(365.25/12) # days per month divided by average month length
  }
  offset<-log(poff*moff)
                                        # generalized linear model
  model<-glm(f,data=data,family=family,offset=offset)
  s<-summary(model)
  res<-residuals(model)

  ## create predicted data (intercept + sinusoid)
  cnames<-row.names(s$coefficients)
  cindex<-sum(as.numeric(cnames=='cosw')*(1:length(cnames)))
  sindex<-sum(as.numeric(cnames=='sinw')*(1:length(cnames)))
  fitted=fitted(model) # standard fitted values
  pred<-s$coefficients[1,1]+(s$coefficients[cindex,1]*newdata$cosw)+
    (s$coefficients[sindex,1]*newdata$sinw)
                                        # back-transform
  if (s$family$link=='log'){pred<-exp(pred)}
  if (s$family$link=='logit'){pred<-exp(pred)/(1+exp(pred))}
  if (s$family$link=='cloglog'){pred<-1-exp(-exp(pred))}
                                        # return
  toret<-list()
  toret$call<-call
  toret$glm<-s
  toret$fitted.plus<-fitted
  toret$fitted.values<-pred
  toret$residuals<-res
  toret$date<-date
  class(toret)<-'Cosinor'
  return(toret)
}
