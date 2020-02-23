f<-function(x)
{
  (1/0.6396319)*(1+((x-0.25)/0.5)^2)^-1.5
}
I<-integrate(f,0,0.7)
#rectangle
rectangle<-function(a,b,n)
{
  i<-(b-a)/n
  total<-0
  k<-0
  while(k<b)
  {
    area<-f(k)*i
    total<-total+area
    #cat("now i =",k,"\n")
    #cat("this location area is ",area,"now the total area is :",total,"\n")
    k<-k+i
  }
  #cat("the area is :",total,"\n")
  return(total)
}
#compute n
n<-2
conv<-FALSE
options(digits = 16)
while(!conv)
{
  I_hat<-rectangle(0,0.7,n)
  if(abs(I$value-I_hat)<0.0001) (conv <- TRUE)
  n<-n+1
  cat("now n is:",n,"\n")
}
cat("I area is:",I$value,"I_hat of rectangle is:",rectangle(0,0.7,1669),"\n")
cat("|I-I_hat| is :",abs(I$value-rectangle(0,0.7,1669)),"\n")
#n=1669