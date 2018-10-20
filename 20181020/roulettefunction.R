LTMember <- c("Here presenters of LT as character.")

roulette <- function(vec){
  x <- length(vec)
  vec2 <- sample(vec,x,replace = FALSE, rep(1/x,x))
  return(vec2)
}

roulette(LTMember)