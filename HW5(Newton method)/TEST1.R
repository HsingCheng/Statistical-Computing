rm(list=ls())
graphics.off()
data<-faithful
data1<-data[,1]
data2<-data[,2]
ker2<-function(x1,x2,xi,xj,bn1,bn2)
{ v<-(x1-xi)/bn1
  w<-(x2-xj)/bn2
  if(abs(v)<1){a<-(3/4)*(1-v^2)} else{a<-0}
  if(abs(w)<1){b<-(3/4)*(1-w^2)} else{b<-0}
  return((a/bn1)*(b/bn2))
}
data<-as.matrix(data)
n<-dim(data)[1]
j31<-data1
j32<-data2
b1<-seq(0.5,1,by =0.5) 
b2<-seq(0.5,1,by =0.5)
# m<-matrix(0,nrow = length(j31),ncol = 273)
c3<-matrix(rep(0,length(b1)*length(b2)),
           nrow = length(b1),
           ncol = length(b2))
b1<-0.5
b2<-0.5
for(bn1 in b1){
  for(bn2 in b2){
    s<-1 
    j<-1
    to<-0
    for (l in j31){
      for(k in j32){
        data1<-as.vector(data[-j,1])
        data2<-as.vector(data[-j,2])
        for(i in 1:length(data1)){
          a<-ker2(l,k,data1[i],data2[i],bn1,bn2)
          to<-to+a
        }
        s<-s*to/length(data1)
        j<-j+1
      }
    }
  c3[b1,b2]<-s
  }
}
d<-c()
for(i in 1:length(j31))
{
  s<-cbind(j31,j32[i])
  d<-rbind(d,s)
}