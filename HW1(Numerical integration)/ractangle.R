#Rectangle rule
f<-function(x)
{
  exp(-x^2)
}
rectangle<-function(n)
{
i<-(1-0)/n
total<-0
k<-0
while(k<1)
{
  area<-f(k)*i
  total<-total+area
  cat("now i =",k,"\n")
  cat("this location area is ",area,"now the total area is :",total,"\n")
  k<-k+i
}
cat("the area is :",total,"\n")
return(total)
}
#collect all choose of n
answer<-c()
for(i in c(2,4,6,8,16,32,64,128))
{
rectangle(i)
answer<-c(answer,rectangle(i))  
}

