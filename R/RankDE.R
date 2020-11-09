#' @param d
#' @param f
#' @param swarm
#' @param iteration
#' @param Cr Crossover rate
#' @param ff scaling factor
#'
RankDE <- function(d,f,swarm=200,iteration=800,
               Cr=0.5,ff=0.5,
               lower=rep(-10,d),upper=rep(10,d)
){
  #----initialization------
  r=1:swarm
  fitness=c()
  x <- matrix(nrow = swarm,ncol = d)
  v <- matrix(nrow = swarm,ncol = d)
  u <- matrix(nrow = swarm,ncol = d)
  for(i in 1:swarm){
    x[i,] <- initialize(d,lower,upper)
    fitness[i] <- f(x[i,])
  }
  #---searching process-----
  for(k in 1:iteration){
    p=order(fitness)/sum(1:swarm)
    for(i in 1:swarm){
      rnd=sample(r[-i],3,prob = p[-i])
      v[i,] <- x[rnd[1]]+ff*(x[rnd[2]]-x[rnd[3]])
      u[i,] <- x[i,]
      rndint=sample(1:d,sample(1:d,1))
      for(j in 1:length(rndint))if(runif(1)<Cr) u[i,rndint[j]] <- v[i,rndint[j]]
      if(f(u[i,])<f(x[i,])) x[i,] <- u[i,]
      fitness[i] <- f(x[i,])
    }
  }
  #---return the result------
  for(i in 1:swarm)  fitness[i]=f(x[i,])
  return(list(gbest=x[which.min(fitness),],fitness=min(fitness)))

}
