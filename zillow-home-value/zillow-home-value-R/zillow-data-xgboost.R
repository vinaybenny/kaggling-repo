library(caret)
library(xgboost)

validation_size <- 0.2

# Train-Validation split
train_indices <- createDataPartition(y, times = 1, p = (1-validation_size), list = TRUE)


dtrain <- xgb.DMatrix(data.matrix(transactions[train_indices$Resample1,]), label = y[train_indices$Resample1] )
dvalid <- xgb.DMatrix(data.matrix(transactions[-train_indices$Resample1,]), label = y[-train_indices$Resample1] )



# Define Hyperparameter grid
xgb_grid <- expand.grid(
  max_depth = c(3, 5, 7),
  eta = c(0.1, 0.01)
);




rmseErrorsHyperparameters <- apply(xgb_grid, 1, function(parameterList){
  
  # Extract parameters to use
  max_depth_val = parameterList[["max_depth"]]
  eta_val = parameterList[["eta"]]
  print(paste0('Max Depth: ', max_depth_val, ' Eta: ', eta_val))
  
  bstcv <- xgb.cv(data = dtrain, nrounds = 1000, nfold=10, showsd= TRUE,
                  booster = "gbtree",
                  objective="reg:linear", 
                  metrics=list("mae"),
                  verbose=1,
                  nthread=4,
                  max_depth = max_depth_val,
                  eta = eta_val,
                  early_stopping_rounds=20,
                  maximize = FALSE,
                  seed=12345)
  
  
  xvalidationscores <- as.data.frame(bstcv$evaluation_log)
  maeval <- tail(xvalidationscores$test_mae_mean, 1)
  best_iter <- which.min(xvalidationscores[, "test_mae_mean"])
  
  return(c(max_depth_val, eta_val, best_iter, maeval))
})

opt_max_depth_val <- rmseErrorsHyperparameters[1 , which(rmseErrorsHyperparameters[4, ] == min(rmseErrorsHyperparameters[4, ]))]
opt_eta_val <- rmseErrorsHyperparameters[2 , which(rmseErrorsHyperparameters[4, ] == min(rmseErrorsHyperparameters[4, ]))]
opt_num_rounds <- rmseErrorsHyperparameters[3 , which(rmseErrorsHyperparameters[4, ] == min(rmseErrorsHyperparameters[4, ]))]

# Use the best model output from the hyperpaparmeter tuning using the minimum mae.
opt_param <- list(max_depth=opt_max_depth_val, 
                  eta=opt_eta_val, 
                  n_thread=4, 
                  silent=1, 
                  booster="gbtree"
              )
opt_model <- xgb.train(opt_param, 
                       data = dtrain, 
                       nrounds = opt_num_rounds,
                       objective="reg:linear", 
                       eval_metric="mae",
                       verbose=1,
                       maximize = FALSE
)

# Perform some analysis on the trained model

xgb.plot.importance(xgb.importance(colnames(modelset[train_indices$Resample1, !names(modelset) == "logerror"]), model = opt_model) , cex=0.9 )

importance_matrix_dtl <- xgb.importance(colnames(dtrain_temp), model = opt_model, data = dtrain_temp, label =y_train$actual_class);

# Perform prediction
j <- 2
for(i in c(10, 11, 12, 10, 11, 12) ){
  print(i)
  test$month <- rep(i, nrow(test))
  dtest <- xgb.DMatrix( data.matrix(test) )
  preds <- predict(opt_model, dtest)
  submission[, j] <- preds
  j <- j+1
}
names(submission)[1] <- "ParcelId"
write.csv(submission, "../data/full_submission.csv", row.names = FALSE)


