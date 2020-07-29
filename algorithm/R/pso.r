#' @param f
#' @param iteration
#' @param swarm quantity of swarm
#' @param d  dimension of variable
#' @param g global optimal position
#' @param p local optimal position
#' @param fitness best fitness route in process
#' @param fit particle's fitness recording in each iteration
#' @param w inertia weight
#' @param c1 local search weight
#' @param c2 global search weight
pso <- function(d,f,w=0.8,c1=2,c2=2,
                swarm=300,iteration=100,
                bounded=FALSE,
                lower=rep(-10,d),upper=rep(10,d)){
#-----definition of coefficient-------
  v=array(dim = c(swarm,d,iteration))
  x=array(dim = c(swarm,d,iteration))
  fit=array(dim = c(swarm,iteration))
  p=array(dim = c(d,iteration))
  g=array(dim = c(d,iteration))
  fitness=rep(0,iteration)
#-----initialize process------
  for(i in 1:swarm){
    x[i,,1] <- initialize(d,lower,upper) #runif(d,min = -10,max = 10)
    v[i,,1] <- initialize(d,rep(-2,d),rep(2,d))  #runif(d)
    fit[i,1] <- f(x[i,,1])
  }
#----particle searching iteration---
  for (k in 1:iteration) {
    for(i in 1:swarm)  fit[i,k] <- f(x[i,,k])
    p[,k]=x[which.min(fit[,k]),,k]
    if(k==1){
      g[,1]=p[,1]
      fitness[1]=f(g[,1])
    }else{
      if(f(g[,k-1])>f(p[,k])){
        g[,k]=p[,k]
      }else{
        g[,k]=g[,k-1]
      }
      fitness[k]=f(g[,k])
    }
##-----update particle position and velocity---
    if(k==iteration) break
    for(i in 1:swarm){
      v[i,,k+1]=w*v[i,,k]+c1*runif(d)*(p[,k]-x[i,,k])+c2*runif(d)*(g[,k]-x[i,,k])
      x[i,,k+1]=x[i,,k]+v[i,,k+1]
##-----check the bound of new position---
      if(bounded==TRUE){
        if(!(all(x[i,,k+1]<upper)&all(x[i,,k+1]>lower))){
          x[i,,k+1]=initialize(d,lower,upper)
        }
        # for(j in 1:d){
        #   if(x[i,j,k+1]>upper[j]){
        #     x[i,j,k+1]=upper[j]
        #   }else{
        #     if(x[i,j,k+1]<lower[j]) x[i,j,k+1]=lower[j]
        #   }
        # }

      }

    }
  }
##-----output the search result---
  plot(1:iteration,fitness,type = 'l')
  if(d==2){
    dev.new()
    plot(0,0,xlim = c(-10,10),ylim = c(-10,10))
    for(i in 1:swarm){
      points(x[i,1,iteration-1],x[i,2,iteration-1],col='red')
    }
  }
  return(list(gbest=g[,iteration],fitness=f(g[,iteration])))
}
