#' @export
#' @param d
#' @param f
#' @param swarm
#' @param iteration
#' @param Cr Crossover rate
#' @param ff scaling factor
#'
DE <- function(d,f,swarm=20,iteration=80,
               Cr=0.5,ff=0.5,lower = c(-0.99,-0.99),upper = c(0.99,0.99)){
  #----initialization------
  r=1:swarm
  fitness=c()
  x <- matrix(nrow = swarm,ncol = d)
  v <- matrix(nrow = swarm,ncol = d)
  u <- matrix(nrow = swarm,ncol = d)

  for(i in 1:swarm){
    x[i,] <- algorithm::initialize(d,lower,upper)
  }

  #---searching process-----
  for(k in 1:iteration){
    for(i in 1:swarm){
      rnd=sample(r[-i],3)
      v[i,] <- x[rnd[1]]+ff*(x[rnd[2],]-x[rnd[3],])
      u[i,] <- x[i,]

      rndint=sample(1:d,sample(1:d,1))
      for(j in 1:length(rndint)) if(runif(1)<Cr) u[i,rndint[j]] <- v[i,rndint[j]]
      #      u[i,]=check0(u[i,],lower,upper)
      u[i,] <- bound(u[i,],lower,upper)
      tryCatch(
        {
          if(f(u[i,])<f(x[i,])) x[i,] <- u[i,]
        },error=function(err){

        }
      )

    }
  }
  #---return the result------
  for(i in 1:swarm)  fitness[i]=f(x[i,])
  gbest=x[which.min(fitness),]
  print(paste("gbest is",gbest))
  return(gbest)

}
