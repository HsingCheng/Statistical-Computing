#3
f<-function(x)
{
  exp(-x^2)
}
n<-50000
u<-runif(n,0,1)
cat("area is :",(1/n)*sum(f(u)),"\n")

#4 
f<- function(x) {
  x^(-1/3)+(x/10)
}
n<-50000
u<-runif(n,0,1)
p2<-function(x)
{
  (2/3)*x^(-1/3)
}
p2in<-function(x)
{
  x^(2/3)
}
xi<-p2in(u)

#compute f(x)/p1
Ip1_hat<-(1/n)*sum(f(u))

#compute f/p2
Ip2_hat<-(1/n)*sum(f(xi)/p2(xi))
