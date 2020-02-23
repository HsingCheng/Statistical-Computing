rm(list=ls())
graphics.off()
######
n<-5000
f<-function(x,u,sigma){
  (1/sqrt(2*pi*sigma))*exp(-(x-u)^2/(2*sigma))
}
compute.z1<-function(x,p1,p2,p3,u1,sigma1,u2,sigma2,u3,sigma3){
  y<-p1*f(x,u1,sigma1)/(p1*f(x,u1,sigma1)+p2*f(x,u2,sigma2)+p3*f(x,u3,sigma3))
  return(y)
}
compute.z2<-function(x,p1,p2,p3,u1,sigma1,u2,sigma2,u3,sigma3){
  y<-p2*f(x,u2,sigma2)/(p1*f(x,u1,sigma1)+p2*f(x,u2,sigma2)+p3*f(x,u3,sigma3))
  return(y)
}
compute.z3<-function(x,p1,p2,p3,u1,sigma1,u2,sigma2,u3,sigma3){
  y<-p3*f(x,u3,sigma3)/(p1*f(x,u1,sigma1)+p2*f(x,u2,sigma2)+p3*f(x,u3,sigma3))
  return(y)
}

generate.normal<-function(n){
  Z<-numeric(n)
  conv<-FALSE
  loop<-0
  while(!conv){
    u1<-runif(1,0,1)
    u2<-runif(1,0,1)
    u3<-runif(1,0,1)
    y<- -log(u1)
    if(u2<=exp(-(y-1)^2/2)){
      if(u3<1/2){loop<-loop+1;Z[loop]<- -y} else{loop<-loop+1;Z[loop]<-y}
    }
    if(loop==n){conv<-TRUE}
  }
  return(Z)
}

x1<-generate.normal(n*0.5)*1+2
x2<-generate.normal(n*0.3)*1+3
x3<-generate.normal(n*0.2)
x<-c(x2,x3,x1)

before.p1<-0.22
before.p2<-0.28
before.p3<-0.5
before.u1<-1
before.u2<-5
before.u3<-3
before.sigma1<-3
before.sigma2<-2
before.sigma3<-1.5

before.par<-c(before.p1,before.p2,before.p3,before.u1,before.u2,before.u3,before.sigma1,before.sigma2,before.sigma3)
loop<-1
conv<-FALSE
while(!conv){
  zz1<-numeric(n);zz2<-numeric(n);zz3<-numeric(n);zz1.sigma<-numeric(n);zz2.sigma<-numeric(n);zz3.sigma<-numeric(n);zz1.u<-numeric(n);zz2.u<-numeric(n);zz3.u<-numeric(n)
  for(i in 1:n){
    z1<-compute.z1(x[i],before.p1,before.p2,before.p3,before.u1,before.sigma1,before.u2,before.sigma2,before.u3,before.sigma3)
    zz1[i]<-z1
    zz1.u[i]<-z1*x[i]
    z2<-compute.z2(x[i],before.p1,before.p2,before.p3,before.u1,before.sigma1,before.u2,before.sigma2,before.u3,before.sigma3)
    zz2[i]<-z2
    zz2.u[i]<-z2*x[i]
    z3<-compute.z3(x[i],before.p1,before.p2,before.p3,before.u1,before.sigma1,before.u2,before.sigma2,before.u3,before.sigma3)
    zz3[i]<-z3
    zz3.u[i]<-z3*x[i]
  }
  after.p1<-sum(zz1)/n
  after.p2<-sum(zz2)/n
  after.p3<-sum(zz3)/n
  after.u1<-sum(zz1.u)/sum(zz1)
  after.u2<-sum(zz2.u)/sum(zz2)
  after.u3<-sum(zz3.u)/sum(zz3)
  z11<-numeric(n);z22<-numeric(n);z33<-numeric(n)
  for(i in 1:n){
  z1<-compute.z1(x[i],before.p1,before.p2,before.p3,before.u1,before.sigma1,before.u2,before.sigma2,before.u3,before.sigma3)
  zz1.sigma[i]<-z1*(x[i]-after.u1)^2
  z11[i]<-compute.z1(x[i],before.p1,before.p2,before.p3,before.u1,before.sigma1,before.u2,before.sigma2,before.u3,before.sigma3)
  
  z2<-compute.z2(x[i],before.p1,before.p2,before.p3,before.u1,before.sigma1,before.u2,before.sigma2,before.u3,before.sigma3)
  zz2.sigma[i]<-z2*(x[i]-after.u2)^2
  z22[i]<-compute.z2(x[i],before.p1,before.p2,before.p3,before.u1,before.sigma1,before.u2,before.sigma2,before.u3,before.sigma3)
  
  z3<-compute.z3(x[i],before.p1,before.p2,before.p3,before.u1,before.sigma1,before.u2,before.sigma2,before.u3,before.sigma3)
  zz3.sigma[i]<-z3*(x[i]-after.u3)^2
  z33[i]<-compute.z3(x[i],before.p1,before.p2,before.p3,before.u1,before.sigma1,before.u2,before.sigma2,before.u3,before.sigma3)
  }
  after.sigma1<-sum(zz1.sigma)/sum(z11)
  after.sigma2<-sum(zz2.sigma)/sum(z22)
  after.sigma3<-sum(zz3.sigma)/sum(z33)
  
  after.par<-c(after.p1,after.p2,after.p3,after.u1,after.u2,after.u3,after.sigma1,after.sigma2,after.sigma3)
  
  cat("now is:",loop,"th loop and nth par. is:",before.par,"n+1th is",after.par,"\n")
  cat("the error is:",max(abs(after.par-before.par)),"\n\n")
  if(max(abs(after.par-before.par))<10^-5){conv<-TRUE} else{
    loop<-loop+1
    before.p1<-after.p1
    before.p2<-after.p2
    before.p3<-after.p3
    before.u1<-after.u1
    before.u2<-after.u2
    before.u3<-after.u3
    before.sigma1<-after.sigma1
    before.sigma2<-after.sigma2
    before.sigma3<-after.sigma3
    before.par<-after.par
    }
  
}
