library(glmnet)
library(Matrix)
library(doMC)
library(tictoc)



# richiamo dati
load("sparse_train")

# parallel stuff!
registerDoMC(cores = 3) # cores = detectCores() - 1



# ridge

tic()
ridgeFit = glmnet(x = XTrain,
                  y = yTrain,
                  alpha = 0,
                  standardize = T,
                  intercept = F)
toc()

# CV
tic()
ridgeCross = cv.glmnet(x = XTrain,
                       y = yTrain,
                       nfolds = 10,
                       alpha = 0,
                       standardize = T,
                       intercept = F,
                       type.measure = "mse",
                       parallel = T)
toc()

# plot!
plot(ridgeCross)
plot(ridgeFit, xvar = "lambda")
abline(v = log(ridgeCross$lambda.min))


# LASSO

tic()
lassoFit = glmnet(x = XTrain,
                  y = yTrain,
                  alpha = 1,
                  standardize = T,
                  intercept = F)
toc()

# CV
tic()
lassoCross = cv.glmnet(x = XTrain,
                       y = yTrain,
                       nfolds = 10,
                       alpha = 1,
                       standardize = T,
                       intercept = F,
                       type.measure = "mse",
                       parallel = T)
toc()

# plot!
plot(lassoCross)
plot(lassoFit, xvar = "lambda")
abline(v = log(lassoCross$lambda.min))



########
# MODELS
########

# 5 fold
ridgeLambda = 0.006459385
lassoLambda = 3.140941e-05 # con intercetta!
lassoLambda = 3.339533e-05 # senza intercetta!

# 10 fold
ridgeLambda = 0
lassoLambda = 0

lassoBest = glmnet(x = XTrain,
                   y = yTrain,
                   alpha = 1,
                   lambda = lassoLambda,
                   standardize = T,
                   intercept = F)

sparseFitted = predict.glmnet(lassoBest, XTrain)

sparseFittedC = sparseFitted
sparseFittedC[which(sparseFitted < 0)] = 0
sparseFittedC[which(sparseFitted > 1)] = 1

load("sparse_test")
sparsePredict = predict.glmnet(lassoBest, XTest)



############
# SUBMISSION
############

sparsePredictC = sparsePredict
sparsePredictC[which(sparsePredict < 0)] = 0
sparsePredictC[which(sparsePredict > 1)] = 1

load("my_avito")
plot(density(yTrain[, 1]), lwd = 1.5)
lines(density(sparseFitted), lwd = 1.5, col = "red")
lines(density(sparseFittedC), lwd = 1.5, col = "orange")

sparseSub = cbind(avito_test$item_id, sparsePredictC)
colnames(sparseSub) = c("item_id", "deal_probability")
readr::write_csv(as.data.frame(sparseSub), path = "/Users/riccardoparviero/Documents/R/Avito.competition/kaggle_avito_DUE/sub_PROVA_due.csv")


