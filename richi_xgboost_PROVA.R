library(xgboost)
library(tictoc)


# richiamo dati
load("tfidf_train")


# prendo vettore y e matrice X
yTrain = richi_train[, "deal_probability"]

# creo validation
ids = as.vector(caret::createDataPartition(yTrain, p = 0.9, list = F))
XTrain = xgb.DMatrix(data = richi_train[-ids, !colnames(richi_train) %in% "deal_probability"], label = yTrain[-ids])
XValid = xgb.DMatrix(data = richi_train[ids, !colnames(richi_train) %in% "deal_probability"], label = yTrain[ids])

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
                  XTrain, 
                  xgbTune$nrounds, 
                  list(val = XValid), 
                  print_every_n = 50, 
                  early_stopping_rounds = 50)
toc()

imp <- xgb.importance(colnames(XTrain), model = m_xgb)
xgb.plot.importance(imp, top_n = 15)
