rm(list=ls())
graphics.off()
f<-function(n)
{
  pbn<-((3/5)/(n*1/25*0.211571))^(1/5)
  MISE<-(pbn)^4/100*0.211571 + 1/(n*pbn)*3/5
  return(MISE)
}
c<-numeric(100)
for(i in 1:100)
{
  c[i]<-f(50*i)
}
op<-function(x)
{
  x^-1
}
plot(op,xlim = c(0,4500),ylim = c(0,0.02))
for(i in 1:100)
{
  points(x=i*50,y=c[i])
}
