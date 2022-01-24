#' @export
initialize <- function(d,lower,upper){
  p=rep(NA,d)
  for(i in 1:d){
    p[i]=runif(1,min=lower[d],max=upper[d])
  }
  return(p)
}
