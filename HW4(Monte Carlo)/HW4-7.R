dev.new()
opar <- par(mfrow = c(2,3), oma = c(0, 0, 2.7, 0))
#generate normal data
for(n in c(100,500,1000,5000,10000,100000))
{
z0<-2
x<-c()
while(length(x)<n){
  z<-numeric(3)
for(j in 1:3)
{
  zi<-z0
  z0<-(16807*zi)%%(2^31-1)
  z[j]<-z0/(2^31-1)
}
 y<- -log(z[1])
 e<-exp(-(y-1)^2/2)
 if(z[2]<=e && z[3]<1/2)
 {
   x<-c(x,-y)
 }
 else if (z[2]<=e)
 {
   x<-c(x,y)
 }
}
#monte carlo integerate
#x~N(0,1) #
options(digits = 15)
f<-function(x)
{
  x^4*1/sqrt(2*pi)*exp(-x^2/2)
}
I<-integrate(f,-Inf,Inf)
g<-function(x)
{
  1/sqrt(2*pi)*exp(-x^2/2)
}
#compute
I_hat<-(1/n)*sum(f(x)/g(x))
cat("the real integration must be :",I$value,"\n")
cat("under n :",n,"I_hat is :",I_hat,"\n")
qqnorm(x,main = paste("qqnorm of n :",n))
}