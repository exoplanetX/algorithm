distances <- function(A,B){
  d=sqrt((A[1]-B[1])^2+(A[2]-B[2])^2)
  return(as.numeric(d))
}
