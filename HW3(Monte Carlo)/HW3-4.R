#4 
f<- function(x) {
  x^(-1/3)+(x/10)
}

p2<-function(x)
{
  (2/3)*x^(-1/3)
}

p2in<-function(x)
{
  x^(3/2)
}

n<-50000
u<-runif(n,0,1)
x<-p2in(u)
#compute f(x)/p1
Ip1_hat<-(1/n)*sum(f(u))
I<-integrate(f,0,1)
#compute f/p2
Ip2_hat<-(1/n)*sum(f(x)/p2(x))

plot(x,p2(x))
plot(p2)
