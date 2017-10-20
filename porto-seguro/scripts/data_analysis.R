# ================================================================================================ #
# Description: Perform a few test models on the dataset.
# 
# Author: V Benny
#
# ================================================================================================ #

library(ROCR)
library(randomForest)

# Try a simple logit model first
logitmodel <- glm(formula = target~., data = train %>% select(-one_of(idcol, "ps_ind_09_bin", "ps_ind_14")), family = binomial(link = "logit") )
preds <- predict.glm(logitmodel, train %>% select(-one_of(idcol, "ps_ind_09_bin", "ps_ind_14")) ) 

# Try a random forest for classification
rfmodel <- randomForest(x = train %>% select(-one_of(idcol, "ps_ind_09_bin", "ps_ind_14", targetcol))
                        ,y = factor(train$target)
                        ,ntree = 80
                        ,do.trace = 2)
preds <- predict(rfmodel, data = train %>% select(-one_of(idcol, "ps_ind_09_bin", "ps_ind_14")) )

plot(performance(prediction(preds, train$target), "tpr", "rpp"), lwd = 7)

