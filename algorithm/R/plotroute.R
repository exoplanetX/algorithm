plotroute <- function(nodes,r=anyroute(nodes)){
  plot(nodes,pch=16,cex=0.5)
  for(i in 1:(length(r)-1)){
    lines(nodes[r,])
  }
}
