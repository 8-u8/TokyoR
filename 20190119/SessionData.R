# 20190119 TokyoR#75 Advanced-Session
# coding by Takafumi Ito(@0_u0)
#####注意#####
#1. このスクリプトをベースに関数を組むことに
#   Takafumi Itoへの特別な許可は必要ありません．
#2. {base}に入っている関数名で実装するとたいへんなことになるので注意してください
#3. この関数をそのまま使って環境が壊れたなどの責任について，
#   Takafumi Itoは一切の責任を負いません．
#
##############

# 検証用
X <- c(1:99, NA)
Y <- matrix(X, 10,10)

# Head Tail Expm1
Head <- function(DataFrame, n = 5){
    if(class(DataFrame) == "data.frame" | class(DataFrame) == "matrix"){
      DataFrame[1:n,]
    }else{
      DataFrame[1:n]
    }
}
Tail <- function(DataFrame, n = 5){

  if(class(DataFrame) == "data.frame" | class(DataFrame) == "matrix"){
    END   <- nrow(DataFrame)
    START <- END-n
    DataFrame[START:END,]
  }else{
    END   <- length(DataFrame)
    START <- END-n
    DataFrame[START:END]
  }
}

Head(X)
Head(Y)
Tail(X)
Tail(Y)

EXPm1 <- function(x){
  exp(x-1)	
}

EXPm1(X)
Head(EXPm1(Y))

# 演算子
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


Y %>2% apply(., MARGIN = 1, sum)
Y %fillna% mean(X, na.rm = TRUE)
