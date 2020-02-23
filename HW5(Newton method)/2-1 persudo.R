rm(list=ls())
graphics.off()
data<-faithful
data1<-data[,1]
data2<-data[,2]
ker<-function(x,xi,n,bn)
{
  to<-0
  for(i in xi){
    v <- abs((x-i)/bn)
    if(v < 1){
      s<-(3/4)*(1-v^2)
      to<-to+s
    } else{to<-to}
  }
  return(1/(n*bn)*to)
}
j1<-data1
b1<-seq(0.01,1,by =0.01)
c1<-c()
for(bn in b1){
s<-1
for(i in 1:length(j1))
  {
    a<-ker(j1[i],data1[-i],length(data1),bn)
    s<-s*a
  }
c1<-c(c1,s)
}
cat("the situation of max of persudo likelihood:",which(c1==max(c1)),"max value is:",max(c1),"\n")
cat("from above to find bn:",b1[which(c1==max(c1))],"\n")

collect<-function(bn){
  j1<-seq(min(data1),max(data1),(max(data1)-min(data1))/272)
  c1<-c()
  for(i in j1)
  {
    a<-ker(i,data1,length(data1),bn)
    c1<-c(c1,a)
  }
  return(c1)
}
f1<-collect(b1[which(c1==max(c1))])
plot(f1,col="red",type = "l",ylim =c(0,max(f1)),main ="plot of density of first column",ylab = "y",xlab = "x")

#######
rm(list=ls())
graphics.off()
data<-faithful
data1<-data[,1]
data2<-data[,2]
ker<-function(x,xi,n,bn)
{
  to<-0
  for(i in xi){
    v <- abs((x-i)/bn)
    if(v < 1){
      s<-(3/4)*(1-v^2)
      to<-to+s
    } else{to<-to}
  }
  return(1/(n*bn)*to)
}
j2<-data2
b2<-seq(0.01,3,by =0.01)
c2<-c()
for(bn in b2){
  s<-0
  for(i in 1:length(j2))
  {
    a<-ker(j2[i],data2[-i],length(data2),bn)
    s<-s+log(a)
  }
  c2<-c(c2,s)
}
cat("the situation of max of persudo likelihood:",which(c2==max(c2)),"max value is:",max(c2),"\n")
cat("from above to find bn:",b2[which(c2==max(c2))],"\n")

collect2<-function(bn){
  j2<-seq(min(data2),max(data2),(max(data2)-min(data2))/272)
  c1<-c()
  for(i in j2)
  {
    a<-ker(i,data2,length(data2),bn)
    c1<-c(c1,a)
  }
  return(c1)
}
g1<-collect2(b2[which(c2==max(c2))])
plot(g1,col="red",type = "l",ylim =c(0,max(g1)),main ="plot of density of second column",ylab = "y",xlab = "x")
