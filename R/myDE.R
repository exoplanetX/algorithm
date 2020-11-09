#' @param d
#' @param f
#' @param swarm
#' @param iteration
#' @param Cr Crossover rate
#' @param ff scaling factor
#'
myDE <- function(d,f,swarm=200,iteration=800,
               Cr=0.5,ff=0.5,
               lower=rep(-10,d),upper=rep(10,d)){

  # d=2
  # f=Rastrigin
  # swarm=200
  # iteration=800
  # Cr=0.5
  # ff=0.5
  # lower=rep(-10,d)
  # upper=rep(10,d)
  #----initialization------
  r=1:swarm
  fitness=c() #群体中点适应度
  denorm=c() #与最优状态差距度量距离
  vfitness=c() #r1 r2 r3的适应度
  x <- matrix(nrow = swarm,ncol = d)
  v <- matrix(nrow = swarm,ncol = d)
  u <- matrix(nrow = swarm,ncol = d)
  reb <- matrix(nrow = swarm,ncol = d)
  rebb <- matrix(nrow=swarm,ncol=d)
  for(i in 1:swarm){
    x[i,] <- initialize(d,lower,upper)
    fitness[i] <- f(x[i,])
  }
  #---searching process-----
  for(k in 1:iteration){
    # fitness前50%建立tabu禁忌表，圈内部交叉，圈外部随意

    #……待完成
    gbest=which.min(fitness)
    for(i in 1:swarm)  denorm[i]=greyincidence::norms(x[i,]-x[gbest,])
    orderlist=order(denorm)
    ordertest=orderlist<swarm/2
    disperson=sd(denorm[orderlist<(swarm/2)])
    farest=max(denorm)
    #随机抽取r1 r2 r3
    for(i in 1:swarm){

      if(orderlist[i] < (swarm/2) ){
        samplelist=which(orderlist<(swarm/2))
        p=1-orderlist[samplelist]/sum(orderlist[samplelist])
        rnd=sample(samplelist,3,prob = p)
      }else{
        p=1-orderlist/sum(1:swarm)
        rnd=sample(r[-i],3,prob = p[-i])
      }
    #mutation
      v[i,] <- x[rnd[1],]+ff*(x[rnd[2],]-x[rnd[3],])
      reb[i,] <- x[rnd[1],]+1.2*(x[rnd[3],]-x[rnd[2],])
      if(reb[i,]>upper || reb[i,]<lower) reb[i,] <- initialize(d,lower,upper)
    #crossover
      for(k in 1:3) vfitness[k] <- f(x[rnd[k]])
      u[i,] <- x[i,]
      rebb[i,] <- x[i,]
      rndint=sample(1:d,sample(1:d,1))
      for(j in 1:length(rndint)){
        if(runif(1)<Cr){
          u[i,rndint[j]] <- v[i,rndint[j]]
          rebb[i,rndint[j]] <- reb[i,rndint[j]]  #产生反向搜索
        }
      }

      if( f(u[i,]) < f(x[i,]) ){
        x[i,] <- u[i,]
        x[rnd[which.max(vfitness)],] <-  reb[i,] #x[rnd[1],]+ff*( x[rnd[3],] - x[rnd[2],] )
      }
      fitness[i] <- f(x[i,])
    }
  }
  #---return the result------
  for(i in 1:swarm)  fitness[i]=f(x[i,])
  print(x[which.min(fitness),])
  print(min(fitness))
  if(d==2) plot(x)
  #return(list(gbest=x[which.min(fitness),],fitness=min(fitness)))

}
