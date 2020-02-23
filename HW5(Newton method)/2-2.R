rm(list=ls())
graphics.off()
data<-faithful
data1<-data[,1]
data2<-data[,2]
ker2<-function(x)
{
  if(abs(x)<1)
  { return((3/4)*(1-x^2))} else{return(0)}
}
data<-as.matrix(data)
n<-dim(data)[1]
j31<-seq(min(data1),max(data1),(max(data1)-min(data1))/272)
j32<-seq(min(data2),max(data2),(max(data2)-min(data2))/272)
bn1<-0.21 ;bn2<-2.48
# m<-matrix(0,nrow = length(j31),ncol = 273)
c3<-c()
for (l in j31) {
  for(k in j32)
  {
    to<-0
    for(n in 1:272)
    {
      a<-(ker2((l-data[n,1])/bn1)/bn1)*(ker2((k-data[n,2])/bn2)/bn2)
      to<-to+a
    }
    c3<-c(c3,to/n)
  }
}
d<-c()
for(i in 1:length(j31))
{
  s<-cbind(j31,j32[i])
  d<-rbind(d,s)
}
library(scatterplot3d)
scatterplot3d(d[,1],d[,2],c3 ,color = "blue",type = "p",xlab = "X1",ylab = "X2",zlab = "value of f_hat",main = "joint density of two column")
