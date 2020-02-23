rm(list=ls())
graphics.off()
###############
n<-1
f<-function(x1,x2){
  (x1-2)^4+(x1-2*x2)^2
}
h<-10^-6
df.x1<-function(x1,x2){
  y<-(f(x1+h,x2)-f(x1,x2))/h
  return(y)
}
df.x2<-function(x1,x2){
  y<-(f(x1,x2+h)-f(x1,x2))/h
  return(y)
}
df.x1.x1<-function(x1,x2){
  y<-(df.x1(x1+h,x2)-df.x1(x1,x2))/h
  return(y)
}
df.x1.x2<-function(x1,x2){
  y<-(df.x1(x1,x2+h)-df.x1(x1,x2))/h
  return(y)
}
df.x2.x1<-function(x1,x2){
  y<-(df.x2(x1+h,x2)-df.x2(x1,x2))/h
  return(y)
}
df.x2.x2<-function(x1,x2){
  y<-(df.x2(x1,x2+h)-df.x2(x1,x2))/h
  return(y)
}
gradient<-function(x1,x2){
  y<-matrix(c(df.x1(x1,x2),df.x2(x1,x2)),nrow = 2)
  return(y)
}
hessian<-function(x1,x2)
{
  y<-matrix(c(df.x1.x1(x1,x2),df.x1.x2(x1,x2),df.x2.x1(x1,x2),df.x2.x2(x1,x2)),nrow = 2,byrow = TRUE)
  return(y)
}
mini<-function(x1,x2,alpha){
  y<-((x1-alpha*gradient(x1,x2)[1,])-2)^4+((x1-alpha*gradient(x1,x2)[1,])-2*(x2-alpha*gradient(x1,x2)[2,]))^2
  return(y)
}
dmini<-function(x1,x2,alpha){
  y<-(mini(x1,x2,alpha+h)-mini(x1,x2,alpha))/h
  return(y)
}
walk<-function(x1,x2){
  conv<-FALSE
  n<-1
  h<-10^-6
  xa<-0
  while(!conv){
    xb<-xa
    xn<-xb-dmini(x1,x2,xb)/((dmini(x1,x2,xb+h)-dmini(x1,x2,xb))/h)
    xa<-xn
    if(abs(mini(x1,x2,xa))<10^-10 | n>150){conv<-TRUE}
    # cat("now is",n,"iteration and x value is:",xa,"and f(x) is:",f(xa),"\n")
    n<-n+1
  }
  return(xa)
}
#####
#Steepest descent method
#####
error<-10^-10
xb<-matrix(c(0,3),nrow=2)
conv<-FALSE
while (!conv){
  xn<-xb-walk(xb[1,],xb[2,])*gradient(xb[1,],xb[2,])
  cat("the length of walk is:",walk(xb[1,],xb[2,]),"\n")
  xa<-xn
  r<-sqrt((xb[1,]-xa[1,])^2+(xb[2,]-xa[2,])^2)
  cat("now iteration time is:",n,"the distance of value is:",r,"the function value is:",f(xa[1,],xa[2,]),"\n")
  if(r<error| n>150) (conv<-TRUE)
  xb<-xa
  n<-n+1
}

#####
#Newton's method 
#####
error<-10^-10
xb<-matrix(c(0,3),nrow=2)
conv<-FALSE
while (!conv){
  xn<-xb-solve(hessian(xb[1,],xb[2,]))%*%gradient(xb[1,],xb[2,])
  xa<-xn
  r<-sqrt(sum((xb-xa)^2))
  cat("now iteration time is:",n,"the distance of value is:",r,"the function value is:",gradient(xa[1,],xa[2,]),"\n")
  if(r<error| n>150) (conv<-TRUE)
  xb<-xa
  n<-n+1
}