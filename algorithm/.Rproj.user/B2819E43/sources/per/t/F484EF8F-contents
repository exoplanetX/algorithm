#' test functions
#'
Rastrigin <- function(x) 20+sum(x^2)-sum(10*cos(2*pi*x))

f2 <- function(x) { r <- sqrt(sum(x^2)); 10 * sin(r)/r }

sphere <- function(x) sum(x^2)


#Schwefel <- function(x,y) -x*sin(sqrt(abs(x)))-y*sin(sqrt(abs(y)))
Schwefel <- function(x) sum( -x*sin(sqrt(abs(x))) )

Ackley <- function(x){
  a=20
  b=0.2
  c=2*pi
  n=2
  -a*exp(-b*sqrt( 1/n* sum(x*x) )) - exp(1/n* sum( cos(c*x) ) )+a+exp(1)
}


