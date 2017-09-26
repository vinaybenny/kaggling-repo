library(h2o)


# Start up an h2o server
h2o.init(nthreads = 4, max_mem_size = "4g")
h2o.removeAll()



train_x$resp <- train_y
valid_x$resp <- valid_y
predictors <- names(train_x)
response <- "resp"

localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)
pathToData <- paste0(normalizePath("../data"), "/train.csv")
write.table(x = train_x, file = pathToData, row.names = F, col.names = T)
train_x <- h2o.importFile(path = pathToData, destination_frame = "dat")
pathToData <- paste0(normalizePath("../data"), "/valid.csv")
write.table(x = valid_x, file = pathToData, row.names = F, col.names = T)
valid_x <- h2o.importFile(path = pathToData, destination_frame = "dat")

hyper_params <- list(
  hidden=list(c(32,32),c(64,64), c(128, 128)),
  input_dropout_ratio=c(0,0.05),
  rate=c(0.01,0.02),
  rate_annealing=c(1e-8,1e-7,1e-6)
)


m1 <- h2o.deeplearning(
  model_id="dl_model_first", 
  training_frame=train_x, 
  validation_frame= valid_x,   ## validation dataset: used for scoring and early stopping
  x=predictors,
  y=response,
  hidden=c(100, 100),                  ## 2 layers, 100 neurons each
  epochs=1000,
  #activation="Rectifier",  ## default
  stopping_rounds=10,
  stopping_metric="MAE",
  stopping_tolerance=0.01,
  variable_importances=T,    ## not enabled by default
  l1=1e-5,                        ## add some L1/L2 regularization
  l2=1e-5,
  adaptive_rate=F,                ## manually tuned learning rate
  rate=0.01, 
  rate_annealing=2e-6,            
  momentum_start=0.2,             ## manually tuned momentum
  momentum_stable=0.4, 
  momentum_ramp=1e7,
  max_w2=10,                      ## helps stability for Rectifier
  overwrite_with_best_model=T
)
summary(m1)
plot(m1)

h2o.performance(m3, train=T)          ## sampled training data (from model building)
h2o.performance(m3, valid=T)          ## sampled validation data (from model building)
h2o.performance(m3, newdata=train)    ## full training data
h2o.performance(m3, newdata=valid)    ## full validation data
h2o.performance(m3, newdata=test)     ## full test data

pred <- h2o.predict(m3, test)