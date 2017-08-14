library(lightgbm)


dtrain <- lgb.Dataset(data.matrix(transactions[train_indices$Resample1,-1]), label = y[train_indices$Resample1] ) 
dvalid <- lgb.Dataset(data.matrix(transactions[-train_indices$Resample1,-1]), label = y[-train_indices$Resample1] )


params <- list(objective = "regression", metric = "mae")
watchlist <- list(test = dvalid)

lgbmodel <- lgb.train(params, dtrain, 500, watchlist, min_data=1, learning_rate=1, early_stopping_rounds = 10)


params['sub_feature'] = 0.5
params['min_data'] = 500
params['min_hessian'] = 1

lgb.cv()