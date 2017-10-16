# ================================================================================================ #
# Description: Perform a few test models on the dataset.
# 
# Author: V Benny
#
# ================================================================================================ #

# Try a simple logiit model first
 logitmodel <- glm(formula = target~., data = train, family = binomial(link = "logit") )
 