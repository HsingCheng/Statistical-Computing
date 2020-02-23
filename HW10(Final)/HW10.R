rm(list=ls())
graphics.off()
initialpar<-0.5
beforepar<-initialpar
conv<-FALSE
loop<-1
while(!conv){
  newpar<-1/((38/(34+125*beforepar/(beforepar+2)))+1)
  afterpar<-newpar
  cat("now is:",loop,"th loop and (n+1)th theta is:",afterpar,"(n+1)th theta - nth theta is:",abs(afterpar-beforepar),"\n")
  if(abs(afterpar-beforepar)<10^-7){
  conv<-TRUE 
  cat("the final estimator of theta is:",afterpar,"\n")
   break} else{loop<-loop+1}
  beforepar<-afterpar
}

#####
rm(list=ls())
graphics.off()
#####
#(i)
N<-c(3062,587,284,103,33,4,2)
N1<-3062*0+1*587+284*2+3*103+4*33+5*4+2*6
lamda_hat<-N1/4075
p<-c()
for(i in 1:7){
  p<-c(p,dpois((i-1),lamda_hat))
}
a<-0
for(j in 1:7){
  a<-a+(N[i]-N1*p[i])^2/(N1*p[i])
}
if(pchisq(a,6,lower.tail = FALSE)<0.05){
  cat("the kolmogorov-Smirnov test p-value is:",pchisq(a,6,lower.tail = FALSE),"\n")
  cat("we reject H0","\n")
} else{
  cat("the kolmogorov-Smirnov test p-value is:",pchisq(a,6,lower.tail = FALSE),"\n")
  cat("we didn't reject H0","\n")
}
#####
#(ii)
rm(list=ls())
graphics.off()
y0<-rep(0,3062)
y1<-rep(1,587)
y2<-rep(2,284)
y3<-rep(3,103)
y4<-rep(4,33)
y5<-rep(5,4)
y6<-rep(6,2)
initial.p<-0.75
initial.u<-0.4
x<-c(y0,y1,y2,y3,y4,y5,y6)


computez1<-function(i,p,u,x){
  y<-(u^x*exp(-u))/factorial(x)
  if(i <=3062){
    return(p/(p+(1-p)*y))
  } else{return(0)}
}
computez2<-function(i,p,u,x){
  y<-(u^x*exp(-u))/factorial(x)
  if(i <=3062){
    return(((1-p)*y)/(p+(1-p)*y))
  } else{return(1)}
}
before.p<-initial.p
before.u<-initial.u

collect.p<-c()
collect.u<-c()
loop<-1
conv<-FALSE
while(!conv){
sum.z1<-0
sum.z2<-0
sum.xz2<-0
  for(i in 1:length(x)){
  comp.z1<-computez1(i,before.p,before.u,x[i])
  comp.z2<-computez2(i,before.p,before.u,x[i])
  sum.z1<-sum.z1+comp.z1
  sum.z2<-sum.z2+comp.z2
  sum.xz2<-sum.xz2+x[i]*comp.z2
  }
  after.p<-sum.z1/length(x)
  after.u<-sum.xz2/sum.z2
  collect.p<-c(collect.p,after.p)
  collect.u<-c(collect.u,after.u)
  cat("now loop is:",loop,"(n+1)th p is:",after.p,"(n+1)th u is:",after.u,"\n")
  if(abs(after.p-before.p)<10^-6 && abs(after.u-before.u)<10^-6){conv<-TRUE} else{
    loop<-loop+1
    before.p<-after.p
    before.u<-after.u
    }
}