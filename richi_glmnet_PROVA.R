library(glmnet)
library(caret)
library(Matrix)
library(tictoc)



# richiamo dati
load("sparse_train")


# standardizzo con caret
stdData = preProcess(Xtrain, method = c("center", "scale"))
XTrainStd = predict(stdData, XTrain)
#XTestStd = predict(stdData, XTest)


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

