# 20190119 TokyoR#75 Advanced-Session
# coding by Takafumi Ito(@0_u0)
#####����#####
#1. ���̃X�N���v�g���x�[�X�Ɋ֐���g�ނ��Ƃ�
#   Takafumi Ito�ւ̓��ʂȋ��͕K�v����܂���D
#2. {base}�ɓ����Ă���֐����Ŏ�������Ƃ����ւ�Ȃ��ƂɂȂ�̂Œ��ӂ��Ă�������
#3. ���̊֐������̂܂܎g���Ċ�����ꂽ�Ȃǂ̐ӔC�ɂ��āC
#   Takafumi Ito�͈�؂̐ӔC�𕉂��܂���D
#
##############

# ���ؗp
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

# ���Z�q
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