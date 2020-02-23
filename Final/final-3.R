rm(list=ls())
graphics.off()
#####

compare.MH<-function(x0,error,n){
xn<-x0
collect<-numeric(n)
collect[1]<-xn
conv<-FALSE
loop<-1
while(!conv){
  loop<-loop+1
  y<-runif(1,xn-error,xn+error)
  min.xy<-min(exp(-1/2*y^2+1/2*xn^2),1)
  u<-runif(1,0,1)
  if(u<=min.xy){xn<-y} else{xn<-xn}
  collect[loop]<-xn
  if(loop==n){conv<-TRUE}
}
plot(collect,type="l",col="red",main = paste("X0 =",x0,"epsilon =",error))
return(collect)
}
a<-compare.MH(0,1,10000)

