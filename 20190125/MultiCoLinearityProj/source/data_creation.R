library(tidyverse)

# initial values
set.seed(1234)
n <- 1000
X1 <- rnorm(n, mean = 0, sd = 1)
X2 <- rpois(n, lambda = 2)
X3 <- rnorm(n, mean = 5, sd = 5)
noise <- rnorm(n, mean=0, sd = 0.01)
X4 <- X1 + noise
cor(X1, X4)

## fact model
y <- 1.2 * X1 + 0.6 * X2 + 0.3 * X3 + rnorm(n, 0, 1) #iid

## data.frame

UseData <- data.frame(target = y,
                      feature_1 = X1,
                      feature_2 = X2,
                      feature_3 = X3,
                      feature_4 = X4
#                      feature_5 = X5
                      )

lm_fit    <- lm(target ~ feature_1 + feature_2 + feature_3, data = UseData)
lm_model1 <- lm(target ~ ., data = UseData)
summary(lm_fit)
summary(lm_model1)
