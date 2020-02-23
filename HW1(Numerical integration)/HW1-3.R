######
#trapezoidal rule
f<-function(x)
{
  exp(-x)*(cos(x^2)^2)
}
options(digits = 20)
I<-integrate(f,0,Inf)
n<-42670
a<-0
b<-50000
Ihat<-0
  i<-(b-a)/n
  Ihat<-0
  k<-0
  while(k<b)
  {
    area<-((f(k)+f(k+i))*i)/2
    Ihat<-Ihat+area
    k<-k+i
  }
cat("I is :",I$value,"\n")
cat("the n is :",n,"under this n the total area is :",Ihat,"\n")
cat(" |I-Ihat| is ",abs(Ihat-I$value),"\n")


