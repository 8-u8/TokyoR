Head <- function(DataFrame, n = 5){
  DataFrame[1:n,]
}
Tail <- function(DataFrame, n = 5){
  END   <- nrow(DataFrame)
  START <- END-n
  DataFrame[START:END,]
}

EXPm1 <- function(x){
  exp(x-1)	
}

"%+=%" <- function(x,y){x + y}
"%-=%" <- function(x,y){x - y}

"%>1%" <- function(x, Function){Function(x)}

HasDot <- function(expr){
  any(as.list(expr) == substitute(.))
}

InsertDot <- function(expr){
  as.call(c(expr[[1]], quote(.), as.list(expr[-1])))
}

"%>2%" <- function(LeftHands, RightHands){
  env <- parent.frame()
  expr <- substitute(RightHands)
  dotted <- if(HasDot(expr)) expr else InsertDot(expr)
  eval(dotted, list(. = LeftHands), env)
}

"%fillna%" <- function(x,y){
  x[is.na(x)] <- y
  return(x)
}

