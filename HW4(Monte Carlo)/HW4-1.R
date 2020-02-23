f<- function(x) {
  x^(-1/3)+(x/10)
}

for(n in c(100,500,1000,10000,50000))
{
a<-c()
for(i in 1:1000)
{
u<-runif(n,0,1)
Ip1_hat<-(1/n)*sum(f(u))
a<-c(a,Ip1_hat)
}
cat("under n:",n,"variance of handwriting :",0.720833333333334*1/n,"\n")
cat("under n:",n,"variance of Ip1_hat :",var(a),"\n")
}
#hand compute var(Ip1_hat)=0.720833333333334*1/n

####
#hand comput var(Ip2_hat)=0.002*1/n
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

opar<-par(mfrow = c(2,3), oma = c(0, 0, 2.7, 0))
for(n in c(100,500,1000,10000,50000))
{
  b<-c()
for(i in 1:1000){
u<-runif(n,0,1)
x<-p2in(u)
Ip2_hat<-(1/n)*sum(f(x)/p2(x))
b<-c(b,Ip2_hat)
}
cat("under n:",n,"variance of handwriting :",0.002*1/n,"\n")
cat("under n:",n,"variance of Ip2_hat :",var(b),"\n")
plot(x,p2(x),main = paste("use points draw p2 function under n:",n))
}
plot(p2,main ="plot p2 function")
