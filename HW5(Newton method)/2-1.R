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
  }
  }
  return(1/(n*bn)*to)
}
collect<-function(bn){
j1<-seq(1,6,(max(data1)-min(data1))/272)
c1<-c()
for(i in j1)
{
a<-ker(i,data1,length(data1),bn)
c1<-c(c1,a)
}
return(c1)
}
f1<-collect(0.08)
f2<-collect(0.5)
f3<-collect(0.05)
plot(f1,col="red",type = "l",ylim =c(0,max(f3)),main ="plot of density of first column",ylab = "y",xlab = "x")
legend("topright",                                # 表示在右上角
       pch = "l",                                   # pch代表點的圖案
       col = c("red", "blue", "green"),           # col代表顏色 
       legend = c("bn=1", "bn=0.5", "bn=0.05") # 顏色所對應的名稱
)
points(f2,type ="l",col ="blue")
points(f3,type ="l",col ="green")
####

collect2<-function(bn){
j2<-seq(40,100,(max(data2)-min(data2))/272)
c2<-c()
for(i in j2)
{
  a<-ker(i,data2,length(data2),bn)
  c2<-c(c2,a)
}
return(c2)
}
g1<-collect2(1)
g2<-collect2(0.5)
g3<-collect2(0.05)
g4<-collect2(3)
plot(g1,col="red",type = "l",ylim =c(0,0.1),main ="plot of density of second column",ylab = "y",xlab = "x")
legend("topright",                                # 表示在右上角
       pch = "l",                                   # pch代表點的圖案
       col = c("red", "blue","black"),           # col代表顏色 
       legend = c("bn=1", "bn=0.5", "bn =3") # 顏色所對應的名稱
)
points(g2,type ="l",col ="blue")
# points(g3,type ="l",col ="green")
points(g4,type = "l",col ="black")
