f<-function(x){
  -x^4+3*x^2+2
}
conv<-FALSE
xa<- -2
n<-1
h<-0.05
while(!conv){
  xb<-xa
  xn<-xb-f(xb)/((f(xb+h)-f(xb))/h)
  xa<-xn
  if(f(xa)<10^-10 | n>150){conv<-TRUE}
  cat("now is",n,"iteration and x value is:",xa,"and f(x) is:",f(xa),"\n")
  n<-n+1
}