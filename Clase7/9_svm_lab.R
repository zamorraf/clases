
library(e1071)
library(ISLR)
library(ROCR)
library(dplyr)

# Support Vector Classifier

set.seed(1)
x = matrix(rnorm(20 * 2), ncol = 2)
y = c(rep(-1, 10), rep(1, 10))
x[y == 1, ] = x[y == 1, ] + 1

plot(x, col = (3 - y))
dat = data.frame(x = x, y = as.factor(y))

glimpse(dat)
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)

# Pregunta: Cuales otros kernels tenemos disponibles?

plot(svmfit, dat)

svmfit$index
summary(svmfit)

svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 0.1, scale = FALSE)

plot(svmfit, dat)
svmfit$index

set.seed(1)
tune.out = tune(svm, y ~ ., data = dat, kernel = "linear",
                ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)

# Pregunta: que caracteristica de estilo hay aqui que no recomendamos
xtest = matrix(rnorm(20 * 2), ncol = 2)
ytest = sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1, ] = xtest[ytest == 1, ] + 1
testdat = data.frame(x = xtest, y = as.factor(ytest))
ypred = predict(bestmod, testdat)

table(predict = ypred, truth = testdat$y)

svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 0.01, scale = FALSE)
ypred = predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)

x[y == 1, ] = x[y == 1, ] + 0.5
plot(x, col = (y + 5)/2, pch = 19)


dat = data.frame(x = x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 1e+05)

summary(svmfit)
plot(svmfit, dat)

svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 1)

summary(svmfit)
plot(svmfit, dat)


# Support Vector Machine

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

plot(svmfit, dat[train, ])

set.seed(1)
tune.out = tune(svm, y ~ ., data = dat[train, ],
                kernel = "radial",
                ranges = list(cost = c(0.1, 1, 10, 100, 1000), 
                              gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)

table(true = dat[-train, "y"], 
      pred = predict(tune.out$best.model, newx = dat[-train, ]))


# ROC Curves

rocplot = function(pred, truth, ...) {
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

svmfit.opt = svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 2, 
                 cost = 1, decision.values = T)
fitted = attributes(predict(svmfit.opt, dat[train, ], decision.values = TRUE))$decision.values

par(mfrow = c(1, 2))

rocplot(fitted, dat[train, "y"], main = "Training Data")

svmfit.flex = svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 50, 
                  cost = 1, decision.values = T)
fitted = attributes(predict(svmfit.flex, dat[train, ], decision.values = T))$decision.values

rocplot(fitted, dat[train, "y"], add = T, col = "red")

fitted = attributes(predict(svmfit.opt, dat[-train, ], decision.values = T))$decision.values

rocplot(fitted, dat[-train, "y"], main = "Test Data")

fitted = attributes(predict(svmfit.flex, dat[-train, ], decision.values = T))$decision.values

rocplot(fitted, dat[-train, "y"], add = T, col = "red")


#-----------------------
library(pROC)
## Type 'citation("pROC")' for a citation.
## 
## Attaching package: 'pROC'
## The following objects are masked from 'package:stats':
## 
##     cov, smooth, var
pROC_obj <- roc( dat[-train, "y"],#df$labels,
                fitted,##df$predictions,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type="shape", col="lightblue")
## Warning in plot.ci.se(sens.ci, type = "shape", col = "lightblue"): Low
## definition shape.
plot(sens.ci, type="bars")
#---------------------------


# SVM with Multiple Classes


set.seed(1)
x = rbind(x, matrix(rnorm(50 * 2), ncol = 2))
y = c(y, rep(0, 50))
x[y == 0, 2] = x[y == 0, 2] + 2
dat = data.frame(x = x, y = as.factor(y))
par(mfrow = c(1, 1))
plot(x, col = (y + 1))
svmfit = svm(y ~ ., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)

# Application to Gene Expression Data

names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)

length(Khan$ytrain)
length(Khan$ytest)

table(Khan$ytrain)
table(Khan$ytest)

dat = data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out = svm(y ~ ., data = dat, kernel = "linear", cost = 10)

summary(out)

table(out$fitted, dat$y)
dat.te = data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
pred.te = predict(out, newdata = dat.te)
table(pred.te, dat.te$y)


