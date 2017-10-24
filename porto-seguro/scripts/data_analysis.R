# ================================================================================================ #
# Description: Perform a few test models on the dataset.
# 
# Author: V Benny
#
# ================================================================================================ #

library(ROCR)
library(randomForest)

# Try a simple logit model first
logitmodel <- glm(formula = train_y~.
                  ,data = train_x
                  ,family = binomial(link = "logit") )
preds <- predict.glm(logitmodel
                     ,train_x
                     ,type = "response"
                     ) 

# Try a random forest for classification
rfmodel <- randomForest(x = train_x
                        ,y = factor(train_y)
                        ,ntree = 10
                        ,do.trace = 2
                        ,strata = factor(train_y)
                        ,sampling = c(12000, 12000)
                        ,nodesize = 10
                          )
preds <- (predict(rfmodel,data = train_x, type = "prob"))[,2]

plot(performance(prediction(preds, train_y), "tpr", "fpr"), lwd = 7)
confusionMatrix(preds, train_y)
(performance(prediction(preds, train_y), "auc"))@y.values

