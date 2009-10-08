# plotCircular.R
# circular plot
# March 2009

plotCircular<-function(radii1,radii2=NULL,spokes=NULL,scale=0.8,labels,stats=TRUE,dp=1,clockwise=TRUE,spoke.col='black',lines=FALSE, ...){
op <- par(no.readonly = TRUE) # the whole list of settable par's.
bins<-length(radii1)
clockstart=pi/2 # default clock start at 12 o'clock
half<- 2*pi/(bins*2) # for moving text/spokes half-way round
if (clockwise==TRUE) mult=-1 else mult=1
# First plot a circle (of radius 1) as a frame
detail<-200 # number that controls graphical detail of cheeses
circle<-matrix(nrow=detail+1,ncol=2,data=0)
frac<-1/detail
for (i in 1:(detail+1)){
   circle[i,1]<-1*cos(2*pi*i*frac)
   circle[i,2]<-1*sin(2*pi*i*frac)
}
plot(circle,type='l',col='black',bty='n',yaxt='n',main='',xlab='',ylab='',xlim=c(-1,1),ylim=c(-1,1),xaxt='n', ...)
# scale the radii to the maximum multiplied by the user-defined scale
# draw the cheeses
for (cheeseno in 1:bins){
   if(is.null(radii2)==TRUE){
    scaled1<-scale*radii1/max(radii1)
    cheese<-matrix(nrow=103,ncol=2,data=0)
    start<-2*pi*((cheeseno-1)/bins)+clockstart
    frac<-1/100
    for (i in 0:100){
       cheese[i+2,1]<-mult*scaled1[cheeseno]*cos((2*pi*i*frac/bins)+start)
       cheese[i+2,2]<-scaled1[cheeseno]*sin((2*pi*i*frac/bins)+start)
    }
    polygon(cheese,density=0,angle=0,lty=1,lwd=1,border="black") 
  }
# plot with two segments #
# 1st pattern
   if(is.null(radii2)==FALSE){
    allradii<-c(radii1,radii2)
    scaled1<-scale*radii1/max(allradii)
    scaled2<-scale*radii2/max(allradii)
    cheese1<-matrix(nrow=53,ncol=2,data=0)
    cheese2<-matrix(nrow=53,ncol=2,data=0)
    start<-2*pi*((cheeseno-1)/bins)+clockstart
    frac<-1/100
    for (i in 0:50){
       cheese1[i+1,1]<-mult*scaled1[cheeseno]*cos((2*pi*i*frac/bins)+start)
       cheese1[i+1,2]<-scaled1[cheeseno]*sin((2*pi*i*frac/bins)+start)
    }
    polygon(cheese1,density=0,angle=0,lty=1,lwd=1,border="black") 
 # 2nd pattern
    start<-2*pi*((cheeseno-1)/bins)+clockstart
    for (i in 51:100){
       cheese2[i+1-50,1]<-mult*scaled2[cheeseno]*cos((2*pi*i*frac/bins)+start)
       cheese2[i+1-50,2]<-scaled2[cheeseno]*sin((2*pi*i*frac/bins)+start)
    }
    polygon(cheese2,density=100,angle=0,lty=1,lwd=1,border="black",col='gray') 
   } 
}
# add the text
if (is.null(labels)==FALSE&stats==FALSE){
   for (cheeseno in 1:bins){
      x<-mult*0.92*cos((2*pi*cheeseno/bins)+start+half)
      y<-0.92*sin((2*pi*cheeseno/bins)+start+half)
      text(x,y,labels[cheeseno])
   }
}
# add the labels with stats
if (is.null(labels)==FALSE&stats==TRUE){
   clabel2<-formatC(radii1, format="f", digits=dp) # convert to character
   for (cheeseno in 1:bins){
      x<-mult*0.86*cos((2*pi*cheeseno/bins)+start+half)
      y<-0.86*sin((2*pi*cheeseno/bins)+start+half)
      label1<-labels[cheeseno]
      label<-paste(label1,"\n",clabel2[cheeseno])
      text(x,y,label)
   }
}
# add spokes representing uncertainty
if (is.null(spokes)==FALSE){
   scaleds<-scale*spokes/max(spokes)
   halfcheese<-(2*pi)/(bins*2);
   for (cheeseno in 1:bins){
      spokes<-matrix(data=0,nrow=2,ncol=2)
      spokes[2,1]<-mult*scaleds[cheeseno]*cos((2*pi*cheeseno/bins)+start+half)
      spokes[2,2]<-scaleds[cheeseno]*sin((2*pi*cheeseno/bins)+start+half)
      lines(spokes,pch=0,type='l',col=spoke.col,lty=1,lwd=1.5) 
   }
} # end of spokes
# add dotted lines to separate months
if (lines==TRUE){
   halfcheese<-(2*pi)/(bins*2);
   for (cheeseno in 1:bins){
      breaks<-matrix(data=0,nrow=2,ncol=2)
      breaks[2,1]<-cos((2*pi*cheeseno/bins)+start)
      breaks[2,2]<-sin((2*pi*cheeseno/bins)+start)
      lines(breaks,pch=0,type='l',lty=3,lwd=1) 
   }
} # end of lines
par(op) # restore graphic settings
} # end of function

# examples
#radii<-c(8,7,6,5,4,3.5,2)
#plotCircular(radii,scale=0.7,clockwise=TRUE,labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
#plotCircular(radii,scale=0.8,clockwise=FALSE,labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

#radii<-c(12,11,6,5,4,3,2)
#plotCircular(radii,scale=0.8,clockwise=TRUE,labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

