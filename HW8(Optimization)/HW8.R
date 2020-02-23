f<-function(x){
  x^4-14*x^3+60*x^2-70*x
}
plot(c(-1,3),c(-25,5),type = "n")
a<--1
b<-3
c<-1.5
n<-1
conv<-FALSE
while(!conv){
  if(abs(b-c)>abs(a-c)){
    y<-(b+c)/2
    if(f(y)>=f(c)){
      b<-y
    } else{
      a<-c
      c<-y
    }
  } else{
    y<-(a+c)/2
    if(f(y)>=f(c)){
      a<-y
    } else{
      b<-c
      c<-y
    }
  }
  points(a,f(a),col="red",type ="p",pch = 1)
  points(b,f(b),col="blue",type ="p",pch=1)
  if(abs(a-b)<10^-5 | n>150){conv<-TRUE}
  cat("now is",n,"iteration and xa value is:",a,"and xb is:",b,"\n")
  cat("middle value is:",(a+b)/2,"middle function value is:",f((a+b)/2),"\n\n")
  n<-n+1
}


####
f<-function(x){
  x^4-14*x^3+60*x^2-70*x
}
df<-function(x)
{
  h<-10^-5
  y<-(f(x+h)-f(x))/h
  return(y)
}

conv<-FALSE
xa<- 10
n<-1
h<-10^-5
while(!conv){
  xb<-xa
  xn<-xb-df(xb)/((df(xb+h)-df(xb))/h)
  xa<-xn
  if(abs(df(xa))<10^-10 | n>150){conv<-TRUE}
  cat("now is",n,"iteration and x value is:",xa,"and df(x) is:",df(xa),"\n")
  n<-n+1
}







