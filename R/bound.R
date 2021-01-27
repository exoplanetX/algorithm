bound<-function(x,lower,upper){
  n=length(x)
  for(i in 1:n){
    while(x[i]>upper[i]|x[i]<lower[i]){
      x[i]=runif(1,min=lower[i],max=upper[i])
    }
  }
  return(x)
}
