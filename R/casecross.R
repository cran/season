## casecross.R
## time-stratified case-crossover
## Oct 2009
## assumes day of week is called 'dow'
## assumes date variable is called 'date'

casecross<-function(formula,data,exclusion=2,stratalength=28,matchdow=FALSE,usefinalwindow=FALSE,matchconf='',confrange=0,stratamonth=FALSE){
  attach(data, warn.conflicts = FALSE)
  ## Checks
  if (class(data$date)!="Date"){stop("date variable must be in date format, see ?Dates")} 
  if (exclusion<0){stop("Minimum value for exclusion is zero")} 
  parts<-paste(formula)
  dep<-parts[2] # dependent variable
  indep<-parts[3] # dependent variable
  if (length(formula)<=2){stop("Must be at least one independent variable")} 
  ## original call with defaults (see amer package)
  ans <- as.list(match.call())
  frmls <- formals(deparse(ans[[1]]))
  add <- which(!(names(frmls) %in% names(ans)))
  call<-as.call(c(ans, frmls[add]))
  ## Slim down the data 
  if (matchdow==TRUE){
    f<-as.formula(paste(parts[2],parts[1],parts[3],'+date+dow'))
  }
  if (matchdow==FALSE){
    f<-as.formula(paste(parts[2],parts[1],parts[3],'+date'))
  }
  if (substr(matchconf,1,1)!=""){
    if (matchdow==TRUE){
      f<-as.formula(paste(dep,"~",indep,'+date+dow+',matchconf))
    }
    if (matchdow==FALSE){
      f<-as.formula(paste(dep,"~",indep,'+date+',matchconf))
    }
    fconf<-as.formula(paste(dep,"~",matchconf))
    confdata<-model.frame(fconf,data=data,na.action=na.omit)[,2]
  }
  datatouse<-model.frame(f,data=data,na.action=na.omit) # remove cases with missing covariates
  datediff<-as.numeric(datatouse$date)-min(as.numeric(data$date)) # use minimum data in entire sample
  time<-as.numeric(datediff)+1 # used as strata number
  ## Create strata
  if (stratamonth==TRUE){
    month<-as.numeric(format(datatouse$date,'%m'));
    year<-as.numeric(format(datatouse$date,'%Y'));
    matchday<-as.numeric(format(datatouse$date,'%d'));
    yrdiff<-year-min(year);
    windownum<-(yrdiff*12)+month;
  }
  detach(data) # no longer needed
  if (stratamonth==FALSE){
    ## Get the earliest time and difference all dates from this time
    ## Increase strata windows in jumps of 'stratalength'
    windownum<-floor(datediff/stratalength)+1
    nwindows<-floor(nrow(data)/stratalength)+1
    matchday<-datediff-((windownum-1)*stratalength)+1 # Day number in strata
    ## Exclude the last window if it is less than 'stratalength'
    lastwindow<-datatouse[datatouse$windownum==nwindows,]
    if (nrow(lastwindow)>0){ # only apply to data sets with some data in the final window
      lastlength<-max(time[windownum==nwindows])-min(time[windownum==nwindows])+1
      if (lastlength<stratalength&usefinalwindow==FALSE) datatouse<-datatouse[windownum<nwindows,]
    }
  }
  ## Create the case data
  n<-nrow(datatouse)
  cases<-datatouse
  cases$case<-1 # binary indicator of case
  cases$timex<-1 # Needed for conditional logistic regression
  cases$windownum<-windownum
  cases$time<-time
  cases$diffdays<-NA
  cases$matchday<-matchday
  posout<-sum(as.numeric(names(datatouse)==as.character(f[2]))*(1:ncol(datatouse))) # get the position of the dependent variable
  cases$outcome<-datatouse[,c(posout)]
  nonzerocases<-cases[cases$outcome>0,] # days with one or more cases
  ## Expand control days to cover all case days
  ncases<-nrow(nonzerocases)
  cat('Matching controls to cases, this may take some time','\n')
  for (c in 1:ncases){ # case loop
    selected.case<-nonzerocases[c,] # individual case details
    controls<-cases[cases$windownum==selected.case$windownum,] # get all controls in the same window
    controls$outcome<-selected.case$outcome # replace control numbers with case number
    controls$case<-0 # binary indicator of case
    controls$time<-selected.case$time # strata number
    controls$timex<-2 # Needed for conditional logistic regression
    controls$diffdays<-abs(controls$matchday-selected.case$matchday)
    controls<-controls[controls$diffdays>exclusion,] # remove the exclusion window
                                        # match on day of the week
    if (matchdow==TRUE) controls<-controls[controls$dow==selected.case$dow,] 
                                        # match on a confounder
    if (substr(matchconf,1,1)!=""){
      row<-sum(as.numeric(names(selected.case)==matchconf)*(1:length(names(selected.case))))
      lower<-as.numeric(selected.case[row]-confrange)
      upper<-as.numeric(selected.case[row]+confrange)
      controls<-controls[controls[,row]>=lower&controls[,row]<=upper,] 
    }
                                        # combine cases and controls
    if (c==1){finished<-rbind(nonzerocases,controls)}
    if (c>1){finished<-rbind(finished,controls)}
    if ((c%%500)==0){cat('Case number ',c,'\n')}
  }
  ## Count the number of controls without a case
  onlycntl<-finished[finished$case==0,]
  ncases<-nrow(table(onlycntl$time))
  ncontrols<-round(mean(as.numeric(table(onlycntl$time))),1)
  ## Run the conditional logistic regression
  finalformula<-as.formula(paste('Surv(timex,case)~',indep,'+strata(time)'))
  c.model<-coxph(finalformula,
                 weights=finished$outcome, 
                 data=finished,method=c("breslow"))
  toret<-list()
  toret$finished<-finished # temporary
  toret$call<-call
  toret$c.model<-c.model
  class(toret$c.model)<-"coxph"
  toret$ncases<-ncases
  toret$ncontrols<-ncontrols
  class(toret)<-'casecross'
  return(toret)
}
