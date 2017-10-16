# ================================================================================================ #
# Description: Perform extensive cleaning and application of business rules on training and test
#   datasets, and perform a few checks on distribution.
# 
# Author: V Benny
#
# ================================================================================================ #

library(VIM)
library(mice)
library(xlsx)
library(stringr)

# Missing values in training dataset
missing_values <- train %>% summarize_all(funs(sum(is.na(.))/n())) %>% gather(key="feature", value="missing_pct") %>% arrange(missing_pct)
ggplot(missing_values  , aes(x = reorder(feature,-missing_pct), y = missing_pct )) +
  geom_bar(stat="identity", fill ="red") +
  coord_flip()

# Obtain all combination of missingness patterns and their percentages
missing_pattern <- aggr(train[, !names(train) %in% missing_values[which(missing_values$missing_pct == 0), 1]]
                        ,col = mdc(1:2)
                        ,numbers = TRUE
                        ,labels = names(train[, !names(train) %in% missing_values[which(missing_values$missing_pct == 0), 1]])
                        ,cex.axis=.7
                        ,gap=3
                        ,ylab=c("Proportion of missingness","Missingness Pattern"))
missing_comb <- data.frame(missing_pattern$tabcomb)
names(missing_comb)  <- names(missing_pattern$x)
missing_comb$percent <- missing_pattern$percent
write.xlsx2(missing_comb, file = "../output/missing_values_combinations.xlsx", row.names = FALSE)


############################### Data Cleaning & Imputation ################################################

mn_ps_car_11 <- train %>% filter(!is.na(ps_car_11)) %>% summarise(mn = mean(ps_car_11) )
mn_ps_car_12 <- train %>% filter(!is.na(ps_car_12)) %>% summarise(mn = mean(ps_car_12) )
mn_ps_car_14 <- train %>% filter(!is.na(ps_car_14)) %>% summarise(mn = mean(ps_car_14) )
mn_ps_reg_03 <- train %>% filter(!is.na(ps_reg_03)) %>% summarise(mn = mean(ps_reg_03) )
train <- train %>%
  mutate(ps_car_01_cat = factor(ifelse(is.na(ps_car_01_cat), "UNK", as.character(ps_car_01_cat)))
         ,ps_car_02_cat = factor(ifelse(is.na(ps_car_02_cat), "UNK", as.character(ps_car_02_cat)))
         ,ps_car_03_cat = factor(ifelse(is.na(ps_car_03_cat), "UNK", as.character(ps_car_03_cat)))
         ,ps_car_05_cat = factor(ifelse(is.na(ps_car_05_cat), "UNK", as.character(ps_car_05_cat)))
         ,ps_car_07_cat = factor(ifelse(is.na(ps_car_07_cat), "UNK", as.character(ps_car_07_cat)))
         ,ps_car_09_cat = factor(ifelse(is.na(ps_car_09_cat), "UNK", as.character(ps_car_09_cat)))
         ,ps_ind_02_cat = factor(ifelse(is.na(ps_ind_02_cat), "UNK", as.character(ps_ind_02_cat)))
         ,ps_ind_04_cat = factor(ifelse(is.na(ps_ind_04_cat), "UNK", as.character(ps_ind_04_cat)))
         ,ps_ind_05_cat = factor(ifelse(is.na(ps_ind_05_cat), "UNK", as.character(ps_ind_05_cat)))
         ,ps_reg_03 = ifelse(is.na(ps_reg_03), mn_ps_reg_03[[1]], ps_reg_03)
         ,ps_car_11 = ifelse(is.na(ps_car_11), mn_ps_car_11[[1]], ps_car_11)
         ,ps_car_12 = ifelse(is.na(ps_car_12), mn_ps_car_12[[1]], ps_car_12)
         ,ps_car_14 = ifelse(is.na(ps_car_14), mn_ps_car_14[[1]], ps_car_14)
          )


# Missing values in training dataset after imputation
missing_values <- train %>% summarize_all(funs(sum(is.na(.))/n())) %>% gather(key="feature", value="missing_pct") %>% arrange(missing_pct)
ggplot(missing_values  , aes(x = reorder(feature,-missing_pct), y = missing_pct )) +
  geom_bar(stat="identity", fill ="red") +
  coord_flip()


# Apply the transformations to test and validaation dataset as well.
valid <- valid %>%
  mutate(ps_car_01_cat = factor(ifelse(is.na(ps_car_01_cat), "UNK", as.character(ps_car_01_cat)))
         ,ps_car_02_cat = factor(ifelse(is.na(ps_car_02_cat), "UNK", as.character(ps_car_02_cat)))
         ,ps_car_03_cat = factor(ifelse(is.na(ps_car_03_cat), "UNK", as.character(ps_car_03_cat)))
         ,ps_car_05_cat = factor(ifelse(is.na(ps_car_05_cat), "UNK", as.character(ps_car_05_cat)))
         ,ps_car_07_cat = factor(ifelse(is.na(ps_car_07_cat), "UNK", as.character(ps_car_07_cat)))
         ,ps_car_09_cat = factor(ifelse(is.na(ps_car_09_cat), "UNK", as.character(ps_car_09_cat)))
         ,ps_ind_02_cat = factor(ifelse(is.na(ps_ind_02_cat), "UNK", as.character(ps_ind_02_cat)))
         ,ps_ind_04_cat = factor(ifelse(is.na(ps_ind_04_cat), "UNK", as.character(ps_ind_04_cat)))
         ,ps_ind_05_cat = factor(ifelse(is.na(ps_ind_05_cat), "UNK", as.character(ps_ind_05_cat)))
         ,ps_reg_03 = ifelse(is.na(ps_reg_03), mn_ps_reg_03[[1]], ps_reg_03)
         ,ps_car_11 = ifelse(is.na(ps_car_11), mn_ps_car_11[[1]], ps_car_11)
         ,ps_car_12 = ifelse(is.na(ps_car_12), mn_ps_car_12[[1]], ps_car_12)
         ,ps_car_14 = ifelse(is.na(ps_car_14), mn_ps_car_14[[1]], ps_car_14)
  )
test <- test %>%
  mutate(ps_car_01_cat = factor(ifelse(is.na(ps_car_01_cat), "UNK", as.character(ps_car_01_cat)))
         ,ps_car_02_cat = factor(ifelse(is.na(ps_car_02_cat), "UNK", as.character(ps_car_02_cat)))
         ,ps_car_03_cat = factor(ifelse(is.na(ps_car_03_cat), "UNK", as.character(ps_car_03_cat)))
         ,ps_car_05_cat = factor(ifelse(is.na(ps_car_05_cat), "UNK", as.character(ps_car_05_cat)))
         ,ps_car_07_cat = factor(ifelse(is.na(ps_car_07_cat), "UNK", as.character(ps_car_07_cat)))
         ,ps_car_09_cat = factor(ifelse(is.na(ps_car_09_cat), "UNK", as.character(ps_car_09_cat)))
         ,ps_ind_02_cat = factor(ifelse(is.na(ps_ind_02_cat), "UNK", as.character(ps_ind_02_cat)))
         ,ps_ind_04_cat = factor(ifelse(is.na(ps_ind_04_cat), "UNK", as.character(ps_ind_04_cat)))
         ,ps_ind_05_cat = factor(ifelse(is.na(ps_ind_05_cat), "UNK", as.character(ps_ind_05_cat)))
         ,ps_reg_03 = ifelse(is.na(ps_reg_03), mn_ps_reg_03[[1]], ps_reg_03)
         ,ps_car_11 = ifelse(is.na(ps_car_11), mn_ps_car_11[[1]], ps_car_11)
         ,ps_car_12 = ifelse(is.na(ps_car_12), mn_ps_car_12[[1]], ps_car_12)
         ,ps_car_14 = ifelse(is.na(ps_car_14), mn_ps_car_14[[1]], ps_car_14)
  )


