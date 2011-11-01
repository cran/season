# summary.Cosinor.R
# Oct 2011

summary.Cosinor<-function(object, digits=2, ...){
## Checks
  if (class(object)!="Cosinor"){stop("Object must be of class 'Cosinor'")} 
 cnames<-row.names(object$glm$coefficients)
 cindex<-sum(as.numeric(cnames=='cosw')*(1:length(cnames)))
 sindex<-sum(as.numeric(cnames=='sinw')*(1:length(cnames)))
# amplitude and phase
 amp<-sqrt((object$glm$coefficients[cindex,1]^2)+(object$glm$coefficients[sindex,1]^2))
 addition<-''
 link<-object$glm$family$link
 if (is.null(object$glm$family$link)){link<-' '}
 if (link=='logit'){
   p1<-exp(object$glm$coefficients[1,1])/(1+exp(object$glm$coefficients[1,1])) # back-transform amp
   p2<-exp(object$glm$coefficients[1,1]+amp)/(1+exp(object$glm$coefficients[1,1]+amp)) # back-transform amp
   amp<-p2-p1
   addition<-"(probability scale)"
 }
 if (link=='cloglog'){
   p1<-1-exp(-exp(object$glm$coefficients[1,1]))
   p2<-1-exp(-exp(object$glm$coefficients[1,1]+amp))
   amp<-p2-p1
   addition<-"(probability scale)"
 }
 if (link=='log'){
   amp<-exp(object$glm$coefficients[1,1]+amp)-exp(object$glm$coefficients[1,1])
   addition<-"(absolute scale)"
 }
 phaser<-phasecalc(cosine=object$glm$coefficients[cindex,1],sine=object$glm$coefficients[sindex,1]);
# convert radian phase to a date
 phase<- invyrfraction(frac=phaser/(2*pi),type=object$call$type,text=object$call$text)
# reverse phase (low)
 lphaser<-phaser+pi;
 if (lphaser<0) { lphaser<-lphaser+(2*pi)} # first put in 0 to 2pi range
 if (lphaser>(2*pi)) { lphaser<-lphaser-(2*pi)}
 lphase<- invyrfraction(frac=lphaser/(2*pi),type=object$call$type,text=object$call$text)
# statistical signficance
 toreport<-rbind(object$glm$coefficients[cindex,],object$glm$coefficients[sindex,])
 significant<- as.logical(sum(toreport[,4]<(object$call$alpha/2)))
# returns
 ret<-list()
 ret$n=length(object$residuals)
 ret$amp=amp
 ret$amp.scale=addition
 ret$phase=phase
 ret$lphase=lphase
 ret$significant=significant
 ret$alpha=object$call$alpha
 ret$digits=digits
 ret$text=object$call$text # display phase as text (TRUE/FALSE)
 class(ret) <- "summary.Cosinor"
 ret # uses print.summary.Cosinor
}
