route <- function(nodes,tau,m){
  n=nrow(nodes)
  tabu=rep(T,n)
  tabu[1]=F
  alpha=0.5
  belta=0.5
  r=rep(NA,n)
  r[1]=1
  origin=1
  p=c()
  o=origin
  i=1
  while(length(which(tabu==T))>1){
    i=i+1
    d=which(tabu==T)
    s=tau[o,d]^alpha*(1/m[o,d])^belta
    for(j in 1:length(d)) p[j]=s[j]/sum(s)
    r[i]=sample(d,1,prob=p[1:length(d)])
    tabu[r[i]]=F
    o=r[i]
  }
  r[n]=which(tabu==T)
  return(r)
}
