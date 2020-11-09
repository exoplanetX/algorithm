totdist <- function(r,nodes){
  n=nrow(nodes)
  tot=0
  for(i in 1:(length(r)-1)){
    tot=tot+distances(nodes[r[i],],nodes[r[i+1],])
  }
  tot=tot+distances(nodes[r[length(r)],],nodes[1,])
  return(tot)
}
