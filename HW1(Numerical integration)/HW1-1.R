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



#trapezoidal rule
f<-function(x)
{
  exp(-x^2)
}
trapezoidal<-function(n)
{
  i<-(1-0)/n
  total<-0
  k<-0
  while(k<1)
  {
    area<-((f(k)+f(k+i))*i)/2
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
  trapezoidal(i)
  answer<-c(answer,trapezoidal(i))  
}

#simpson's rule
f<-function(x)
{
  exp(-x^2)
}
simpson<-function(a,b,n)
{
  h<-(b-a)/n
  total<-0
  if(n==2)
  {
    area<-(h/3)*(f(a)+4*f((a+b)/2)+f(b))
    total<-total+area
    cat("n = ",n,"h = ",h,"\n")
    cat(" the total area is :",total,"\n")
  }
  else
  {
    xj<-seq.int(a,b,length.out = n+1)
    
    xj<-xj[-(n+1)]
    area<-(h/3)*(f(a)+(4*sum(f(xj[seq(2, length(xj), 2)])))+(2*sum(f(xj[seq(3, length(xj), 2)])))+f(b))
    total<-total + area
    cat("n = ",n,"h = ",h,"\n")
    cat(" the total area is :",total,"\n")
  }
  return(total)
}
#collect all choose of n
answer<-c()
for(i in c(2,4,6,8,16,32,64,128))
{
  simpson(a=0,b=1,i)
  answer<-c(answer,simpson(a=0,b=1,i))  
}

