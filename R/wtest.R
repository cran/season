# wtest.R
# Walter's test of seasonality
# April 2009

wtest<-function(cases,offset,data,alpha=0.05){
  attach(data,warn.conflicts = FALSE)
  j<-(0:11)/12
  N=sum(cases)
  M=sum(offset)
  W=sum(sqrt(cases))
# observed values
  c=sqrt(cases)*cos(j*2*pi)
  cbar=sum(c/W)
  s=sqrt(cases)*sin(j*2*pi)
  sbar=sum(s/W)
  d=sqrt( (cbar^2) + (sbar^2) )
# expected values
  D=sum(sqrt(offset))
  c=sqrt(offset)*cos(j*2*pi)
  Ecbar=sum(c/D)
  s=sqrt(offset)*sin(j*2*pi)
  Esbar=sum(s/D)
# variance	
  D=sum(sqrt(N*offset/M))
  c=cos(j*2*pi)*cos(j*2*pi)
  Vc=0.25*sum(c)/(D^2)
  s=sin(j*2*pi)*sin(j*2*pi)
  Vs=0.25*sum(s)/(D^2)
# test statistic
  # criti
  test=(((cbar-Ecbar)^2)/Vc)+(((sbar-Esbar)^2)/Vs)
  q=1-alpha
  xnull=qchisq(q,df=2) # null limit (critical value)
  pvalue<-1-pchisq(test,df=2)
  cat('Walter`s test\n')
  cat('test statistic',test,'p-value',pvalue,'\n')
  detach(data)
  return(list(test=test,pvalue=pvalue))
}
