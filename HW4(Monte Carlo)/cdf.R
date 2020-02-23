rm(list = ls())
graphics.off()
opar<-par(mfrow = c(2,2), oma = c(0, 0, 2.7, 0))
for(n in c(100,500,1000,10000)){
xx<-c()
x<-runif(n,0,1)
#x<-rnorm(n,0,1)
k<-min(x)
while(k<max(x)){
j<-0
#c.d.f choose uniform
for(i in 1:length(x))
{
  if(x[i]<=k)
  {
    j<-j+1
  }
}
k<-k+(max(x)-min(x))/n
xx<-c(xx,(1/n)*j)
}
plot(xx,main = paste("under n :",n,"uniform plot"))
}