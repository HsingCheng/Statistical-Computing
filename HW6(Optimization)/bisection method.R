f<-function(x){
  -x^4+3*x^2+2
}

a<- -3
b<- 3 
n<-1
conv<-FALSE
while(!conv){
  xa<-a
  xb<-b
  xn<-(xa+xb)/2
  
  if(f(xa)*f(xn)<0){
    b<-xn
  } else if(f(xn)*f(xb)<0){
    a<-xn} else{cat("it had wrong event!","\n")}
  
  if(f(xn)==0){
    cat("now n is:",n,"and find root is :",xn,"f(xn) is:",f(xn),"\n")
  } else if(abs(a-b)<10^-10 | n>150){conv<-TRUE}
  cat("now is",n,"iteration and xa value is:",a,"and xb is:",b,"\n")
  cat("f(xa)*f(xb) is:",f(a)*f(b),"\n")
  cat("middle value is:",(a+b)/2,"middle function value is:",f((a+b)/2),"\n\n")
  n<-n+1
}