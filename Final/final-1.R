rm(list=ls())
graphics.off()
#####
initialpar<-0.5
beforepar<-initialpar
est.y3<-function(n,theta){
  y3<-numeric(n)
 for(j in 1:n){
    y<-0
    for(i in 1:125){
    u1<-runif(1,0,1)
    if(u1<=theta){y<-y+1}
    }
    y3[j]<-y
 }
  return(mean(y3))
}
initial.y3t<-(beforepar/4)/(beforepar/4+1/2)
before.y3t<-initial.y3t
conv<-FALSE
loop<-1
while(!conv){
  y3n<-est.y3(10000,before.y3t)
  newpar<-((34+y3n)/38)/((34+y3n)/38+1)
  afterpar<-newpar
  cat("now is:",loop,"th loop and mean of y3 is:",y3n,"\n")
  cat("now is:",loop,"th loop and (n+1)th theta is:",afterpar,"(n+1)th theta - nth theta is:",abs(afterpar-beforepar),"\n")
  check.before<-log(factorial(72)/(factorial(38)*factorial(34)))+38*log(1/2*(1-beforepar))+34*log(beforepar/4)
  check.after<-log(factorial(72)/(factorial(38)*factorial(34)))+38*log(1/2*(1-afterpar))+34*log(afterpar/4)
  cat("nth function value is:",check.before,"n+1th function value is:",check.after,"\n")
  if(check.after>=check.before){cat("this loop had increasing","\n")}else{cat(" the function value not increasing","\n");break}
  if(abs(afterpar-beforepar)<10^-6){
    conv<-TRUE 
    cat("the final estimator of theta is:",afterpar,"\n")
    break} else{loop<-loop+1;beforepar<-afterpar;before.y3t<-(beforepar/4)/(beforepar/4+1/2)}
}
