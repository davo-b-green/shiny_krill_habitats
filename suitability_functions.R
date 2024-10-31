sigmoid <- function(x,t,s){
  f.x <- 1/(1+(exp(t*((x)-s))))
  return(f.x)
}

lognormal <- function(x,dp,ep){
  f.x <- exp(dp-0.5*ep*ep-((log(x+1)-dp)^2.0)/(2.0*ep*ep))/(x+1)
  return(f.x)
}

holl <- function(x,cp){
  f.x <- (x*x)/(cp+x*x)
  return(f.x)
}