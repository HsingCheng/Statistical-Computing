#while loop
x <- c(1:100)
even<-c()
odd<-c()
i=1
while(i < length(x)+1)
{
  if(x[i]%%2==0)
  {
    cat("材",i,"竚琌案计",x[i],"\n")
    even<-c(even,x[i])
  }
  else{cat("材",i,"竚琌计",x[i],"\n")
    odd<-c(odd,x[i])
    }
  i=i+1
}

#for loop
x <- c(1:100)
even<-c()
odd<-c()
a=1
for(i in x)
{
  if(i%%2==0)
  {
    cat("材",a,"竚琌案计",i,"\n")
    even<-c(even,i)
  }
  else{cat("材",a,"竚琌计",i,"\n")
    odd<-c(odd,i)
  }
  a=a+1
}