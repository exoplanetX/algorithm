route2matrix <- function(r){
  n=length(r)
  m=matrix(0,nrow=n,ncol = n)
  for(i in 1:(n-1)){
    m[i,r[i+1]]=1
  }
  return(m)
}
