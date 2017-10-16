# ================================================================================================ #
# Description: Create a treatment/control split on transactions, and prepare test data.
# 
# Author: V Benny
#
# ================================================================================================ #

library(caret)

############################### Train/Valid/Test Split ################################################

train_x <- transactions %>% select(-one_of(idcol))
validation_size <- 0.7

# Train-Validation split
train_indices <- createDataPartition(train_x$logerror, times = 1, p = validation_size, list = TRUE)

# Join transactions with properties to create full dataset for analysis
valid_x <- train_x[-train_indices$Resample1,]
train_x <- train_x[train_indices$Resample1,]

############################### Create additional variables ###########################################

# Try a glm model on the dataset
res.lm  <-lm(formula = logerror ~ ., data = train_x )
cooksd <- cooks.distance(res.lm)

# Remove extreme outliers from train_x for better generalisation
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 10*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
outlier_ids <- data.frame(col = as.numeric(names(cooksd)), value = cooksd) %>% 
  filter(!is.na(value)) %>% 
  filter(value > quantile(value, prob = c(0.99))) %>% 
  select(col)
train_x <- train_x[-outlier_ids$col,]


# This will cause leakage of information from target column to covariates, but let us experiment!! Hopefully will not overfit too much.
regzip_submodel <- (summary(lm(formula = logerror~region_zip, data = train_x)))$coefficients %>% as.data.frame()
regzip_submodel_int <- regzip_submodel["(Intercept)", 1]
regzip_submodel$col <- rownames(regzip_submodel)
regzip_submodel <- regzip_submodel %>% 
  mutate(region_zip = (str_sub(col, 11, length(col)))
         ,lm_region_zip = Estimate + regzip_submodel_int) %>% 
  filter(`Pr(>|t|)` < 0.05, col != "(Intercept)") %>%
  select(region_zip, lm_region_zip)

train_x <- train_x %>% left_join(regzip_submodel, by = "region_zip") %>% select(-region_zip) %>% 
  mutate(lm_region_zip = ifelse(is.na(lm_region_zip), 0, lm_region_zip))

# Apply same transformation to validation and test sets.
valid_x <-  valid_x %>% left_join(regzip_submodel, by = "region_zip") %>% select(-region_zip) %>% 
  mutate(lm_region_zip = ifelse(is.na(lm_region_zip), 0, lm_region_zip))

# Apply all the rules applied to training data to test data as well
test <- properties %>% mutate(month = 10) %>% left_join(regzip_submodel, by = "region_zip") %>% select(-region_zip) %>% 
  mutate(lm_region_zip = ifelse(is.na(lm_region_zip), 0, lm_region_zip))



############################# Split train/valid covariates and target #################################
train_y <- train_x$logerror
valid_y <- valid_x$logerror
train_x <- train_x %>% select(-one_of( c("logerror") ) )
valid_x <- valid_x %>% select(-one_of( c("logerror") ) )




