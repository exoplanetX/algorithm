#' @param d
#' @param f
#' @param swarm
#' @param iteration
#' @param Cr Crossover rate
#' @param ff scaling factor
#'
DE <- function(d,f,swarm=200,iteration=300,
               Cr=0.5,ff=0.5,
               lower=rep(-10,d),upper=rep(10,d)
               ){
  r=1:swarm
  x <- matrix(nrow = swarm,ncol = d)
  fitness=c()
  for(i in 1:swarm){
    x[i,] <- initialize(d,lower,upper)
#    fit[i] <- f(x[i,])
  }
  v <- matrix(nrow = swarm,ncol = d)
  u <- matrix(nrow = swarm,ncol = d)
  for(k in 1:iteration){
    for(i in 1:swarm){
      rnd=sample(r[-i],3)
      v[i,] <- x[rnd[1]]+ff*(x[rnd[2]]-x[rnd[3]])
      u[i,] <- x[i,]
      rndint=sample(1:d,1)
      if(runif(1)<Cr) u[i,rndint] <- v[i,rndint]
      if(f(u[i,])<f(x[i,]))  x[i,] <- u[i,]
    }
  }

  for(i in 1:swarm)  fitness[i]=f(x[i,])
  gbest=min(fitness)
  gposition=x[which.min(fitness),]


  return(list(gbest=gbest,gposition=gposition))
}
