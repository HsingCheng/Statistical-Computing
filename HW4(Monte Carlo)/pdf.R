rm(list = ls())
graphics.off()
for(n in c(100,500,1000,10000)){
  opar<-par(mfrow = c(2,3), oma = c(0, 0, 2.7, 0))
  for(bn in c(1,0.5,0.1,0.05,0.01)){
    xx<-c()
  xi<-runif(n,0,1)
  #xi<-rnorm(n,0,1)
  x<-min(xi)
  while(x<max(xi)){
    j<-0
    #c.d.f choose uniform
    for(i in 1:length(xi))
    {
      if(xi[i]>=x-bn && xi[i]<=x+bn)
      {
        j<-j+1
      }
    }
    x<-x+(max(xi)-min(xi))/n
    xx<-c(xx,((1/(2*bn*n))*j))
  }
  plot(xx,main = paste("under n:",n,"bn:",bn,"uniform plot"))
  }
}