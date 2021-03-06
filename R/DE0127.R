#' @param d
#' @param f
#' @param swarm
#' @param iteration
#' @param Cr Crossover rate
#' @param ff scaling factor
#'
DE0127 <- function(d,f,swarm=200,iteration=800,
               Cr=0.5,ff=0.5,
               lower=rep(-10,d),upper=rep(10,d)
               ){
#----initialization------
  r=1:swarm
  fitness=c()
  x <- matrix(nrow = swarm,ncol = d)
  v <- matrix(nrow = swarm,ncol = d)
  u <- matrix(nrow = swarm,ncol = d)
  for(i in 1:swarm) x[i,] <- initialize(d,lower,upper)
#---searching process-----
  for(k in 1:iteration){
    for(i in 1:swarm){
      rnd=sample(r[-i],3)
      v[i,] <- x[rnd[1]]+ff*(x[rnd[2],]-x[rnd[3],])
      #v <- x[rnd[1]]+ff*(x[rnd[2]]-x[rnd[3]])
      u[i,] <- x[i,]
      rndint=sample(1:d,sample(1:d,1))
      for(j in 1:length(rndint))if(runif(1)<Cr) u[i,rndint[j]] <- v[i,rndint[j]]
      if(f(u[i,])<f(x[i,])) x[i,] <- u[i,]
    }
  }
#---return the result------
  for(i in 1:swarm)  fitness[i]=f(x[i,])
  if(d==2) plot(x,xlim = c(-10,10),ylim = c(-10,10))
  return(list(gbest=x[which.min(fitness),],fitness=min(fitness)))

}
