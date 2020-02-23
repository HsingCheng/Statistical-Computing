#3
f<-function(x)
{
  exp(-x^2)
}
n<-50000
u<-runif(n,0,1)
cat("area is :",(1/n)*sum(f(u)),"\n")

plot(u,f(u),type = "l",lwd= 1.2)
