library(xgboost)
library(tictoc)



# richiamo dati
load("tfidf_train")

# creo un subset
sub = as.vector(caret::createDataPartition(yTrain[, 1], p = 0.02, list = F))
XTrain = XTrain[sub, ]
yTrain = yTrain[sub, ]

# creo xgboost matrix
XgbTrain = xgb.DMatrix(data = XTrain, label = yTrain)



# xgboost!
xgbGrid = expand.grid(eta = c(0.04, 0.4),
                      max_depth = 18,
                      subsample = c(0.5, 0.8),
                      colsample_bytree = c(0.5, 0.7))

xgbPlot = list()

tic()
for(i in 1:nrow(xgbGrid)) {
  xgbTune = as.list(xgbGrid[i, ])
  
  xgbCross = xgb.cv(objective = "reg:logistic",
                    booster = "gbtree",
                    eval_metric = "rmse",
                    nthread = 5,
                    params = xgbTune, 
                    data = XgbTrain,
                    nrounds = 5000,
                    early_stopping_rounds = 50,
                    nfold = 5)
  
  xgbPlot$i = xgbCross$evaluation_log
}
toc()






