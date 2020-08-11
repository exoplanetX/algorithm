#' @param nodes coordinates of cities
#' @param n number of cities(nodes)
#' @param m distance matrix of each pair of cities
#' @param tau pheromones of the routes
#' @param Q pheromones constant in each ant
#' @param rho volatilization coefficient of tau
aco <- function(nodes){
  nodes=cities
  swarm=100
  iteration=300
  n=nrow(nodes)
  Q=100
  rho=0.9
  gbest=rep(100000,iteration)

  groute=c()
  L=rep(NA,swarm)
  r=array(NA,dim = c(swarm,n,iteration))
  m=matrix(nrow=n,ncol=n)
  for(i in 1:n){
    for(j in 1:n){
      m[i,j]=distances(cities[i,],cities[j,])
    }
  }
  tau=matrix(1,nrow=n,ncol=n)

  for(i in 1:iteration){
    d_tau=array(0,dim=c(n,n,swarm))
    for(k in 1:swarm){
      r[k,,i]=route(cities,tau,m)
      L[k]=totdist(r[k,,i],nodes)
      d_tau[,,k]= (Q/L[k])*route2matrix(r[k,,i])
    }
    if(i==1){
      if(gbest[1]>min(L)){
        gbest[i] =  min(L)
        groute = r[which.min(L),,i]
      }
    }else{
      if( gbest[i-1]>min(L) ){
        gbest[i] =  min(L)
        groute = r[which.min(L),,i]
      }else{
        gbest[i]=gbest[i-1]
      }
    }


    tau=rho*tau
    for(k in 1:swarm) tau=tau+d_tau[,,k]
  }
  return(gbest[iteration])
}
