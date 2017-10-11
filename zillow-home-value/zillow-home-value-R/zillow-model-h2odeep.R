# ================================================================================================ #
# Description: Train an H2O based neural network for Zillow logerror prediction on the cleaned
#   dataset.
# 
# Author: V Benny
#
# ================================================================================================ #

library(h2o)

# Start up an H2O server
h2o.init(nthreads = 2, max_mem_size = "7g")
h2o.removeAll()


predictors <- names(train_x)
train_x$resp <- train_y
valid_x$resp <- valid_y
response <- "resp"

localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
pathToData <- paste0(normalizePath("../data"), "/train.csv")
write.table(x = train_x, file = pathToData, row.names = F, col.names = T, sep=",")
train_x <- h2o.importFile(path = pathToData, destination_frame = "datt")

pathToData <- paste0(normalizePath("../data"), "/valid.csv")
write.table(x = valid_x, file = pathToData, row.names = F, col.names = T, sep=",")
valid_x <- h2o.importFile(path = pathToData, destination_frame = "datv")

test$month <- 10
pathToData <- paste0(normalizePath("../data"), "/test.csv")
write.table(x = test %>% select(-id_parcel) , file = pathToData, row.names = F, col.names = T, sep=",")
test <- h2o.importFile(path = pathToData, destination_frame = "dat")

hyper_params <- list(
  hidden=list(c(64,64,64), c(96,96)),
  input_dropout_ratio=c(0,0.05),
  rate=c(0.01,0.02),
  rate_annealing=c(1e-8,1e-7,1e-6)
)

grid <- h2o.grid(
  algorithm = "deeplearning",
  grid_id="dl_grid2",
  training_frame=train_x, 
  validation_frame= valid_x,   ## validation dataset: used for scoring and early stopping
  x=predictors,
  y=response,
  epochs=100,
  activation="Rectifier",  ## default
  stopping_rounds=10,
  stopping_metric="MAE",
  stopping_tolerance=0.01,
  l1=1e-5,                        ## add some L1/L2 regularization
  l2=1e-5,
  adaptive_rate=F,                ## manually tuned learning rate         
  momentum_start=0.2,             ## manually tuned momentum
  momentum_stable=0.4, 
  momentum_ramp=1e7,
  max_w2=10,                      ## helps stability for Rectifier
  hyper_params = hyper_params
)
grid <- h2o.getGrid("dl_grid",sort_by="MAE",decreasing=FALSE)
grid@summary_table[1,]
best_model <- h2o.getModel(grid@model_ids[[1]])


m1 <- h2o.deeplearning(
  model_id="dl_model_temp", 
  training_frame=train_x, 
  validation_frame= valid_x,   ## validation dataset: used for scoring and early stopping
  x=predictors,
  y=response,
  epochs=100,
  #activation="Rectifier",  ## default
  stopping_rounds=10,
  stopping_metric="MAE",
  stopping_tolerance=0.01,
  l1=1e-5,                        ## add some L1/L2 regularization
  l2=1e-5,
  adaptive_rate=F,                ## manually tuned learning rate         
  momentum_start=0.2,             ## manually tuned momentum
  momentum_stable=0.4, 
  momentum_ramp=1e7,
  max_w2=10,                      ## helps stability for Rectifier
  hidden=c(128,128),
  input_dropout_ratio = 0.05,
  rate = 0.01,
  rate_annealing = 1.0e-6
)
summary(m1)
plot(m1)

h2o.performance(m1, train=T)          ## sampled training data (from model building)
h2o.performance(m1, valid=T)          ## sampled validation data (from model building)
h2o.performance(m1, newdata=train_x[, !names(train_x) %in% c("resp") ])    ## full training data
h2o.performance(m1, newdata=valid_x[, !names(valid_x) %in% c("resp") ])    ## full validation data
h2o.performance(m3, newdata=test)     ## full test data

pred <- h2o.predict(m1, test)

pred <- read.csv("../output/h2ooutput")
pred <- round(pred, 10)
names(pred) <- c('201610')

submission <- data.table(parcelid=properties$id_parcel, 
                         '201610'=pred$predict, 
                         '201611'=pred$predict, 
                         '201612'=pred$predict, 
                         '201710'=pred$predict,
                         '201711'=pred$predict,
                         '201712'=pred$predict
)

# Save predictions to submission file
names(submission) <- c("ParcelId",'201610','201611','201612','201710','201711','201712')
fwrite(submission, "../output/full_submission.csv", row.names = FALSE)
h2o.shutdown(prompt=FALSE)
