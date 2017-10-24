# ================================================================================================ #
# Description: Extract the train and test datasets and apply basic transformation
# 
# Author: V Benny
#
# ================================================================================================ #

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)

# Seed for replicability
set.seed(12345)

# Set working directory
setwd("C:/Users/vinay.benny/Documents/Kaggle/kaggling-repo/porto-seguro/scripts")


############################### Data Extraction ################################################

# Read files into memory
train <- fread('../data/train.csv', stringsAsFactors = FALSE) %>% as.data.frame()
test <- fread('../data/test.csv', stringsAsFactors = FALSE)  %>% as.data.frame()

# Train-Validation split
validation_size <- 0.7
train_indices <- createDataPartition(train$target, times = 1, p = validation_size, list = TRUE)

# Join transactions with properties to create full dataset for analysis
valid <- train[-train_indices$Resample1,]
train <- train[train_indices$Resample1,]


# Set column name variables
idcol <- "id"
targetcol <- "target"
catcols <- names(train)[grep("_cat", names(train))]
intcols <- names(train[, sapply(train, is.integer) & !( names(train) %in% c(idcol, catcols, targetcol))])
numcols <- names(train[, !names(train) %in% c(catcols, intcols, idcol, targetcol) ])


############################### Data Cleaning ################################################

# Convert all occurences of -1 into NA
train[train==-1] <- NA

# Convert categorical variables into factors
train[, catcols] <-  lapply( train[, catcols], function(x) as.factor(ifelse(x == -1, NA, x)) )

# Apply same transformations to test and validation dataset as well.
valid[valid==-1] <- NA
valid[, catcols] <-  lapply( valid[, catcols], function(x) as.factor(ifelse(x == -1, NA, x)) )
test[test==-1] <- NA
test[, catcols] <-  lapply( test[, catcols], function(x) as.factor(ifelse(x == -1, NA, x)) )

# Convert target into a factor variable
train$target <- factor(train$target)
valid$target <- factor(valid$target)



############################### Data Exploration ################################################

# Plot histograms of all variables after filtering NA
train %>% 
  select_if(is.numeric) %>% 
  select(-one_of(idcol)) %>% 
  melt() %>% 
  filter(!is.na(value)) %>%
  ggplot(aes(x = value)) + facet_wrap(~variable,scales = "free") + geom_histogram()
ggsave(file = "../output/plots/histograms_before_imputation.png", device = "png", width = 16, height = 8, units = "in")



