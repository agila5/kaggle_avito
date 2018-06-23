library(glmnet)
library(Matrix)
library(doMC)
library(tictoc)



# richiamo dati
load("tfidf_train")
yTrain = as.matrix(cbind(1 - yTrain, yTrain))

# parallel stuff!
registerDoMC(cores = 5) # cores = detectCores() - 1



# ridge

tic()
ridgeFit = glmnet(x = XTrain,
                  y = yTrain,
                  alpha = 0,
                  standardize = T,
                  intercept = F,
                  family = "binomial")
toc()

# CV
tic()
ridgeCross = cv.glmnet(x = XTrain,
                       y = yTrain,
                       nfolds = 5,
                       alpha = 0,
                       standardize = T,
                       intercept = F,
                       family = "binomial",
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
                  intercept = F,
                  family = "binomial")
toc()

# CV
tic()
lassoCross = cv.glmnet(x = XTrain,
                       y = yTrain,
                       nfolds = 5,
                       alpha = 1,
                       standardize = T,
                       intercept = F,
                       family = "binomial",
                       type.measure = "mse",
                       parallel = T)
toc()

# plot!
plot(lassoCross)
plot(lassoFit, xvar = "lambda")
abline(v = log(lassoCross$lambda.min))


# print!
print(ridgeCross$lambda.min)
print(lassoCross$lambda.min)



########
# MODELS
########

# normal reg
ridgeLambda = 0.006459385
lassoLambda = 3.140941e-05 # con intercetta!
lassoLambda = 3.339533e-05 # senza intercetta!

# logit reg
ridgeLambda = 0.08757387
lassoLambda = 0.0001087208

lassoBest = glmnet(x = XTrain,
                   y = yTrain,
                   alpha = 1,
                   lambda = lassoLambda,
                   standardize = T,
                   intercept = F,
                   family = "binomial")

sparseFitted = predict.glmnet(lassoBest, XTrain)
sparseFitted = exp(sparseFitted)/(1 + exp(sparseFitted))

load("sparse_test")
sparsePredict = predict.glmnet(lassoBest, XTest)
sparsePredict = exp(sparsePredict)/(1 + exp(sparsePredict))


##############
# SUBMISSION #
##############


plot(density(yTrain[, 2]), lwd = 1.5)
lines(density(sparseFitted), lwd = 1.5, col = "red")


load("my_avito")
sparseSub = cbind(avito_test$item_id, sparsePredict)
colnames(sparseSub) = c("item_id", "deal_probability")
readr::write_csv(as.data.frame(sparseSub), path = "/Users/richardparrow/Documents/R/Avito.competition/kaggle_avito/sub_PROVA_five.csv")


