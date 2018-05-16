library(glmnet)
library(Matrix)
library(doMC)
library(tictoc)



# richiamo dati
load("sparse_train")



# glmnet
tic()
ridgeFit = glmnet(x = XTrain,
                  y = yTrain,
                  alpha = 0,
                  standardize = T,
                  intercept = T)
toc()

tic()
lassoFit = glmnet(x = XTrain,
                  y = yTrain,
                  alpha = 1,
                  standardize = T,
                  intercept = T)
toc()

# cross validation glmnet
# cores = detectCores() - 1
registerDoMC(cores = 3)

tic()
ridgeCross = cv.glmnet(x = XTrain,
                       y = yTrain,
                       nfolds = 5,
                       alpha = 0,
                       standardize = T,
                       intercept = T,
                       type.measure = "mse",
                       parallel = T)
toc()

tic()
lassoCross = cv.glmnet(x = XTrain,
                       y = yTrain,
                       nfolds = 5,
                       alpha = 1,
                       standardize = T,
                       intercept = T,
                       type.measure = "mse",
                       parallel = T)
toc()


# plots!
plot(ridgeCross)
plot(ridgeFit, xvar = "lambda")
abline(v = log(ridgeCross$lambda.min))

plot(lassoCross)
plot(lassoFit, xvar = "lambda")
abline(v = log(lassoCross$lambda.min))



########
# MODELS
########

ridgeLambda = 0.006459385
lassoLambda = 3.140941e-05

lassoBest = glmnet(x = XTrain,
                   y = yTrain,
                   alpha = 1,
                   lambda = lassoLambda,
                   standardize = T,
                   intercept = T)

sparseFitted = predict.glmnet(lassoBest, XTrain)
sparsePredict = predict.glmnet(lassoBest, XTest)



############
# SUBMISSION
############

sparsePredictC = sparsePredict
sparsePredictC[which(sparsePredict < 0)] = 0
sparsePredictC[which(sparsePredict > 1)] = 1

sparseSub = cbind(avito_test$item_id, sparsePredictC)
colnames(sparseSub) = c("item_id", "deal_probability")
readr::write_csv(as.data.frame(sparseSub), path = "/Users/riccardoparviero/Documents/R/Avito.competition/kaggle_avito_DUE/sub_PROVA_uno.csv")


