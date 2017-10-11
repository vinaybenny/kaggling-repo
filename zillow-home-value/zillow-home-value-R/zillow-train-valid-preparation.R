# ================================================================================================ #
# Description: Create a treatment/control split on transactions, and prepare test data.
# 
# Author: V Benny
#
# ================================================================================================ #

library(caret)

############################### Train/Valid/Test Split ################################################

train_x <- transactions %>% select(-one_of(idcol))

# Join transactions with properties to create full dataset for analysis
train_y <- train_x$logerror
train_x <- train_x %>% select(-one_of( c("logerror") ) )

validation_size <- 0.7

# Train-Validation split
train_indices <- createDataPartition(train_y, times = 1, p = validation_size, list = TRUE)

valid_x <- train_x[-train_indices$Resample1,]
valid_y <- train_y[-train_indices$Resample1]
train_x <- train_x[train_indices$Resample1,]
train_y <- train_y[train_indices$Resample1]

# Apply all the rules applied to training data to test data as well
test <- properties %>% mutate(month = 10)
