#' @param d
#' @param f
#' @param swarm
#' @param iteration
#' @param Cr Crossover rate
#' @param ff scaling factor
#'
jDE <- function(d,f,swarm=200,iteration=300,
                Cr=0.5,ff=0.5,
                lower=rep(-10,d),upper=rep(10,d)
){
  r=1:swarm
  fitness=c()
  x <- matrix(nrow = swarm,ncol = d)
  v <- matrix(nrow = swarm,ncol = d)
  u <- matrix(nrow = swarm,ncol = d)
}
