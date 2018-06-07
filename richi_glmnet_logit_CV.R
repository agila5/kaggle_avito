library(glmnet)
library(Matrix)
library(doMC)
library(tictoc)



# richiamo dati
load("sparse_train")
yTrain = as.matrix(cbind(1 - yTrain, yTrain))

set.seed(39)
small = sample(1:nrow(XTrain), size = 10000)
XTrain = XTrain[small, ]
yTrain = yTrain[small, ]




# LASSO


# my CV functions!
# CV params

set.seed(123)
K = 5
folds = sample(1:K, size = nrow(XTrain), replace = T)

lambdaSeq = exp(seq(0, -8, length.out = 1000))
cvMSEs = matrix(0, nrow = K, ncol = length(lambdaSeq))

for(k in 1:K) {
  tic()

  lassoFold = glmnet(x = XTrain[!folds == k, ],
                     y = yTrain[!folds == k, ],
                     alpha = 1,
                     lambda = lambdaSeq,
                     standardize = T,
                     intercept = F,
                     family = "binomial")
  
  cvPreds = exp(predict.glmnet(lassoFold, XTrain[folds == k, ]))/(1 + exp(predict.glmnet(lassoFold, XTrain[folds == k, ])))
  cvMSEs[k, ] = apply(cvPreds, 2, function(preds) sqrt(mean((preds - yTrain[folds == k, 2])^2)))
  
  print(k)
  toc()
}

lambdaMSEs = apply(cvMSEs, 2, function(MSE) mean(MSE))

plot(log(lambdaSeq), lambdaMSEs, type = "l", lwd = 1.5)
abline(v = log(lambdaSeq[which.min(lambdaMSEs)]), col = "red", lty = 3, lwd = 1.5)

plot(density(yTrain[folds == K, 2], from = 0, to = 1), lwd = 1.5)
lines(density(cvPreds[, which.min(lambdaMSEs)], from = 0, to = 1), col = "red", lwd = 1.5)




# glmnet native

# parallel stuff!
registerDoMC(cores = 5) # cores = detectCores() - 1

set.seed(123)
tic()
lassoCross = cv.glmnet(x = XTrain,
                       y = yTrain,
                       nfolds = 5,
                       alpha = 1,
                       lambda = lambdaSeq,
                       standardize = T,
                       intercept = F,
                       family = "binomial",
                       type.measure = "mse",
                       parallel = T)
toc()

plot(lassoCross)



##########
# MODELS #
##########

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


############
# SUBMISSION
############


plot(density(yTrain[, 2]), lwd = 1.5)
lines(density(sparseFitted), lwd = 1.5, col = "red")


load("my_avito")
sparseSub = cbind(avito_test$item_id, sparsePredict)
colnames(sparseSub) = c("item_id", "deal_probability")
readr::write_csv(as.data.frame(sparseSub), path = "/Users/riccardoparviero/Documents/R/Avito.competition/kaggle_avito_DUE/sub_PROVA_tre.csv")


