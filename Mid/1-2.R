rm(list=ls())
f<-function(z)
{
  1/0.6396319*(1+((z-0.25)/0.5)^2)^-1.5
}
I<-integrate(f,0,1)
g<-function(x)
{
  4.1*(1/sqrt(2*pi))*exp(-x^2/2)
}
k<-function(x){
  if(abs(x)<1)
  {return(1-abs(x))} else{return(0)}
}
conv<-FALSE
n<-2
while(!conv){
x<-c()
while(length(x)<n)
{
  u<-runif(1,0,1)
  y<-u
  if(u<f(y)/1.6)
  {
    x<-c(x,y)
  }
  
}
bn<-0.05
xx<-0
for(i in x){
  xx<-xx+k((0.4-i)/bn)
}
xx<-xx/(n*bn)
cat("now n :",n,"xx :",xx,"abs(I$value-xx):",abs(I$value-xx),"\n")
if(abs(I$value-xx)<0.0001){conv<-TRUE}
  n<-n+1
}
l<-function(x)
{
  (1-abs((0.4-x)/0.8))
}