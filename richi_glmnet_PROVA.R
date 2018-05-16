library(glmnet)
library(Matrix)
library(tictoc)


# richiamo dati
load("tfidf_train")


# prendo vettore y e matrice X
yTrain = richi_train[, "deal_probability"]
XTrain = richi_train[, !colnames(richi_train) %in% c("deal_probability", "act_march")]


# glmnet
tic()
ridgeFit = glmnet(x = XTrain,
                  y = YTrain,
                  alpha = 0,
                  standardize = F,
                  intercept = T)
toc()

tic()
lassoFit = glmnet(x = XTrain,
                  y = yTrain,
                  alpha = 1,
                  standardize = F,
                  intercept = T)
toc()

# cross validation glmnet
tic()
ridgeCross = cv.glmnet(x = XTrain,
                       y = yTrain,
                       nfolds = 5,
                       alpha = 0,
                       standardize = T,
                       intercept = T,
                       type.measure = "mse")
toc()

tic()
lassoCross = cv.glmnet(x = XTrain,
                       y = yTrain,
                       nfolds = 5,
                       alpha = 1,
                       standardize = F,
                       intercept = T,
                       type.measure = "mse")
toc()


# plots!
plot(ridgeCross)
plot(ridgeFit, xvar = "lambda")
abline(v = log(ridgeCross$lambda.min))

plot(lassoCross)
plot(lassoFit, xvar = "lambda")
abline(v = log(lassoCross$lambda.min))

