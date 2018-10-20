roulette <- function(vec){
  x <- length(vec)
  vec2 <- sample(vec,x,replace = FALSE, rep(1/x,x))
  return(vec2)
}
