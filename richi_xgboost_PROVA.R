library(xgboost)
library(tictoc)



# richiamo dati
load("sparse_train")


# creo validation
ids = as.vector(caret::createDataPartition(yTrain[, 1], p = 0.9, list = F))
XgbTrain = xgb.DMatrix(data = XTrain[ids, ], label = yTrain[ids, ])
XgbValid = xgb.DMatrix(data = XTrain[-ids, ], label = yTrain[-ids, ])

# xgboost!
xgbTune = list(objective = "reg:logistic",
               booster = "gbtree",
               eval_metric = "rmse",
               nthread = 8,
               eta = 0.04,
               max_depth = 18,
               min_child_weight = 8,
               gamma = 0,
               subsample = 0.8,
               colsample_bytree = 0.7,
               alpha = 0,
               lambda = 0,
               nrounds = 5000)

tic()
m_xgb = xgb.train(xgbTune, 
                  XgbTrain, 
                  xgbTune$nrounds, 
                  list(val = XgbValid), 
                  print_every_n = 50, 
                  early_stopping_rounds = 50)
toc()

imp <- xgb.importance(colnames(XTrain), model = m_xgb)
xgb.plot.importance(imp, top_n = 15)
