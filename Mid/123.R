u<-runif(1000,0,1)
inv<-function(x)
{
  tan(pi*(x-1/2))
}
t<-inv(u)
x<-c()
for(i in 1:1000)
{
  x[i]<-rnorm(1,t[i],1)
}

f<-function(x,t)
{
  exp(-(x-t)^2/2)*(1/(1+t^2))*1/(pi*sqrt(2*pi))
}
hist(f(x,t),breaks = 30)
