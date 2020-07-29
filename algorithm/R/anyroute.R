anyroute <- function(nodes){
  n=nrow(nodes)
  r=sample(1:n,n,replace = F)
  return(r)
}
