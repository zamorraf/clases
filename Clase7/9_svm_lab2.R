library(e1071)
library(ISLR)
library(ROCR)
library(dplyr)

# Metricas de desempeño

set.seed(1)
x = matrix(rnorm(200 * 2), ncol = 2)
x[1:100, ] = x[1:100, ] + 2
x[101:150, ] = x[101:150, ] - 2
y = c(rep(1, 150), rep(2, 50))

dat = data.frame(x = x, y = as.factor(y))
plot(x, col = y)

train = sample(200, 100)
svmfit = svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 1, 
             cost = 1)
plot(svmfit, dat[train, ])
summary(svmfit)
svmfit = svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 1, 
             cost = 1e+05)

matriz_confsion <- table(true = dat[-train, "y"], 
                         pred = predict(tune.out$best.model, 
                        newx = dat[-train, ]))

Exactitud <- (matriz_confsion[1,1] + matriz_confsion[2,2]) / 
              sum(matriz_confsion)
Exactitud
tasa_error <- (matriz_confsion[1,2] + matriz_confsion[2,1]) / 
  sum(matriz_confsion)
tasa_error




