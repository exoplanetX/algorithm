plot3d <- function(f){
  x <- seq(-10,10,leng=30)
  y=x
  z=matrix(0,nrow=length(x),ncol = length(y))
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      z[i,j]=f(c(x[i],y[j]))
    }
  }
  dev.new()
  persp(x,y,z,
        theta = 30,phi = 30,expand = 0.5,
        col=rainbow(1000)
        )
}
