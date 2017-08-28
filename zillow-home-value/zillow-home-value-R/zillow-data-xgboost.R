library(caret)
library(xgboost)

validation_size <- 0.7

# Train-Validation split
train_indices <- createDataPartition(y, times = 1, p = (1-validation_size), list = TRUE)

# Create data matrices, excluding the id_parcel column
dtrain <- xgb.DMatrix(data.matrix(training[train_indices$Resample1,]), label = y[train_indices$Resample1] ) 
dvalid <- xgb.DMatrix(data.matrix(training[-train_indices$Resample1,]), label = y[-train_indices$Resample1] )
dtest <- xgb.DMatrix(data=data.matrix( test ))

# Define Hyperparameter grid
xgb_grid <- expand.grid(
  max_depth = c(2, 4, 6),
  eta = c(0.02, 0.01, 0.005)
);


maeErrorsHyperparameters <- apply(xgb_grid, 1, function(parameterList){
  
  # Extract parameters to use
  max_depth_val = parameterList[["max_depth"]]
  eta_val = parameterList[["eta"]]
  print(paste0('Max Depth: ', max_depth_val, ' Eta: ', eta_val))
  
  bstcv <- xgb.cv(data = dtrain, nrounds = 3000, nfold=10, showsd= TRUE,
                  booster = "gbtree",
                  objective="reg:linear", 
                  metrics=list("mae"),
                  verbose=TRUE,
                  nthread=4,
                  max_depth = max_depth_val,
                  eta = eta_val,
                  min_child_weight = 10,
                  subsample = 0.7,
                  colsample_bytree = 0.5,
                  early_stopping_rounds=50,
                  maximize = FALSE,
                  seed=12345)
  
  
  xvalidationscores <- as.data.frame(bstcv$evaluation_log)
  maeval <- min(xvalidationscores$test_mae_mean)
  best_iter <- bstcv$best_iteration
  
  return(c(max_depth_val, eta_val, best_iter, maeval))
})

opt_max_depth_val <- maeErrorsHyperparameters[1 , which(maeErrorsHyperparameters[4, ] == min(maeErrorsHyperparameters[4, ]))]
opt_eta_val <- maeErrorsHyperparameters[2 , which(maeErrorsHyperparameters[4, ] == min(maeErrorsHyperparameters[4, ]))]
opt_num_rounds <- maeErrorsHyperparameters[3 , which(maeErrorsHyperparameters[4, ] == min(maeErrorsHyperparameters[4, ]))]


# Use the best model output from the hyperpaparmeter tuning using the minimum mae.
opt_param <- list(max_depth=opt_max_depth_val, 
                  eta=opt_eta_val
              )
opt_model <- xgb.train(opt_param, 
                       data = dtrain, 
                       n_thread=4,
                       booster="gbtree",
                       subsample = 0.7,
                       colsample_bytree = 0.5,
                       min_child_weight = 10,
                       nrounds = opt_num_rounds,
                       objective="reg:linear", 
                       eval_metric="mae",
                       verbose=1,
                       maximize = FALSE,
                       seed=12345)


# Perform some analysis on the trained model

xgb.plot.importance(xgb.importance(colnames(training[train_indices$Resample1,]), model = opt_model) , cex=0.9 )



# Perform prediction
j <- 2
submission$id_parcel <- test$id_parcel
for(i in c(10, 11, 12, 10, 11, 12) ){
  print(i)
  test$month <- rep(i, nrow(test))
  dtest <- xgb.DMatrix( data.matrix(test[,!names(test) %in% c("id_parcel")]) )
  preds <- predict(opt_model, dtest)
  submission[, j] <- preds
  j <- j+1
}

preds <- predict(opt_model, dtest)
submission <- data.table(parcelid=properties$id_parcel, 
                      '201610'=preds, 
                      '201611'=preds, 
                      '201612'=preds, 
                      '201710'=preds,
                      '201711'=preds,
                      '201712'=preds
)

names(submission)[1] <- "ParcelId"
fwrite(submission, "../output/full_submission.csv", row.names = FALSE)



