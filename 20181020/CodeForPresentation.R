library(tidyverse)
library(rgl)

# Psuedo data creating
X1 <- rnorm(1000,0,1)
X2 <- rnorm(1000,3,0.05)
X3 <- rnorm(1000,2,2)
X4 <- 3*X1 + 4*X2 + 875*X3 - 899

Y  <- rnorm(1000,0,var(X1+X2+X3+X4))

Data <- data.frame(y = Y, x1 = X1, x2 = X2, x3 = X3, x4 = X4)

Data %>% summary

model <- lm(y ~ . ,data = Data)

model %>% summary

plot3d(Data$x2,Data$x3,Data$x4)
cor(Data)

model2 <- lm(x4 ~ x1+x2+x3, data = Data)
summary(model2)
