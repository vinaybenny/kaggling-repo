library(lightgbm)


dtrain <- lgb.Dataset(data.matrix(train_x), label = train_y ) 
dvalid <- lgb.Dataset(data.matrix(valid_x), label = valid_y )

lgb_grid <- expand.grid(
  learning_rate = c(0.002, 0.02, 0.2),
  
)

# Hyper-parameter tuning using grid search cross validation
maeErrorsHyperparameters <- apply(lgb_grid, 1, function(parameterList){
  
  # Extract parameters to use
  learning_rate_val = parameterList[["learning_rate"]]
  print(paste0(' Learning Rate: ', learning_rate_val))
  
  params <- list(max_bin = 9,
                 learning_rate = learning_rate_val,
                 boosting_type = 'gbdt',
                 objective = "regression",
                 metric = 'mae',
                 sub_feature = 0.5,
                 bagging_fraction = 0.85,
                 bagging_freq = 20,
                 num_leaves = 60,
                 min_data = 500,
                 min_hessian = 0.05)
  
  bstcv <- lgb.cv(params = params, data = dtrain, 
                  nrounds = 500, watchlist, early_stopping_rounds = 10,
                  nfold=10, 
                  showsd= TRUE, 
                  verbose=1)
  
  
  xvalidationscores <- as.data.frame(bstcv$evaluation_log)
  maeval <- min(xvalidationscores$test_mae_mean)
  best_iter <- bstcv$best_iteration
  
  return(c(max_depth_val, eta_val, best_iter, maeval))
})

# Obtain the optimum parameters
opt_max_depth_val <- maeErrorsHyperparameters[1 , which(maeErrorsHyperparameters[4, ] == min(maeErrorsHyperparameters[4, ]))]
opt_eta_val <- maeErrorsHyperparameters[2 , which(maeErrorsHyperparameters[4, ] == min(maeErrorsHyperparameters[4, ]))]
opt_num_rounds <- maeErrorsHyperparameters[3 , which(maeErrorsHyperparameters[4, ] == min(maeErrorsHyperparameters[4, ]))]






watchlist <- list(test = dvalid)

lgbmodel <- lgb.cv(params, dtrain, nrounds = 500, early_stopping_rounds = 10)


params['sub_feature'] = 0.5
params['min_data'] = 500
params['min_hessian'] = 1

lgb.cv()