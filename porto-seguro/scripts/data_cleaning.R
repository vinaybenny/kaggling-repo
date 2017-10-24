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
library(vtreat)
library(FactoMineR)
library(reshape2)


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

# Use vtreat package to perform data cleaning/imputation
cfeencoder <- vtreat::mkCrossFrameCExperiment(dframe = train, varlist = c(catcols, numcols, intcols)
                                              ,outcomename = targetcol, outcometarget = "1")
treatplan <- cfeencoder$treatments
train <- cfeencoder$crossFrame


# mn_ps_car_11 <- train %>% filter(!is.na(ps_car_11)) %>% summarise(mn = mean(ps_car_11) )
# mn_ps_car_12 <- train %>% filter(!is.na(ps_car_12)) %>% summarise(mn = mean(ps_car_12) )
# mn_ps_car_14 <- train %>% filter(!is.na(ps_car_14)) %>% summarise(mn = mean(ps_car_14) )
# mn_ps_reg_03 <- train %>% filter(!is.na(ps_reg_03)) %>% summarise(mn = mean(ps_reg_03) )
# ps_car_11_cat_lst <- train %>% select(ps_car_11_cat) %>% group_by(ps_car_11_cat) %>% 
#                          summarise(ct = n()) %>%  
#                          arrange(desc(ct)) %>%
#                          # head(10) %>% 
#                          mutate(ps_car_11_cat = as.character(ps_car_11_cat) ) %>%
#                          select(ps_car_11_cat) %>% 
#                          data.frame() 
# ps_car_06_cat_lst <- train %>% select(ps_car_06_cat) %>% 
#                          group_by(ps_car_06_cat) %>% 
#                          summarise(ct = n()) %>%  
#                          arrange(desc(ct)) %>%
#                          # head(10) %>% 
#                          mutate(ps_car_06_cat = as.character(ps_car_06_cat) ) %>%
#                          select(ps_car_06_cat) %>% data.frame()
# 
# train <- train %>%
#   mutate(ps_car_01_cat = factor(ifelse(is.na(ps_car_01_cat), "UNK", as.character(ps_car_01_cat)))
#          ,ps_car_02_cat = factor(ifelse(is.na(ps_car_02_cat), "UNK", as.character(ps_car_02_cat)))
#          ,ps_car_03_cat = factor(ifelse(is.na(ps_car_03_cat), "UNK", as.character(ps_car_03_cat)))
#          ,ps_car_05_cat = factor(ifelse(is.na(ps_car_05_cat), "UNK", as.character(ps_car_05_cat)))
#          ,ps_car_07_cat = factor(ifelse(is.na(ps_car_07_cat), "UNK", as.character(ps_car_07_cat)))
#          ,ps_car_09_cat = factor(ifelse(is.na(ps_car_09_cat), "UNK", as.character(ps_car_09_cat)))
#          ,ps_ind_02_cat = factor(ifelse(is.na(ps_ind_02_cat), "UNK", as.character(ps_ind_02_cat)))
#          ,ps_ind_04_cat = factor(ifelse(is.na(ps_ind_04_cat), "UNK", as.character(ps_ind_04_cat)))
#          ,ps_ind_05_cat = factor(ifelse(is.na(ps_ind_05_cat), "UNK", as.character(ps_ind_05_cat)))
#          ,ps_reg_03 = ifelse(is.na(ps_reg_03), mn_ps_reg_03[[1]], ps_reg_03)
#          ,ps_car_11 = ifelse(is.na(ps_car_11), mn_ps_car_11[[1]], ps_car_11)
#          ,ps_car_12 = ifelse(is.na(ps_car_12), mn_ps_car_12[[1]], ps_car_12)
#          ,ps_car_14 = ifelse(is.na(ps_car_14), mn_ps_car_14[[1]], ps_car_14)
#          ,ps_car_06_cat = as.factor( ifelse( !(as.character(ps_car_06_cat) %in% ps_car_06_cat_lst[,1] ),"OTHERS",
#                                              as.character(ps_car_06_cat)) )
#          ,ps_car_11_cat = as.factor( ifelse( !(as.character(ps_car_11_cat) %in% ps_car_11_cat_lst[,1] ),"OTHERS",
#                                            as.character(ps_car_11_cat)) )
#           )
# 
# 
# # Missing values in training dataset after imputation
# missing_values <- train %>% summarize_all(funs(sum(is.na(.))/n())) %>% gather(key="feature", value="missing_pct") %>% arrange(missing_pct)
# ggplot(missing_values  , aes(x = reorder(feature,-missing_pct), y = missing_pct )) +
#   geom_bar(stat="identity", fill ="red") +
#   coord_flip()
# 
# 
# # Apply the transformations to test and validaation dataset as well.
# valid <- valid %>%
#   mutate(ps_car_01_cat = factor(ifelse(is.na(ps_car_01_cat), "UNK", as.character(ps_car_01_cat)))
#          ,ps_car_02_cat = factor(ifelse(is.na(ps_car_02_cat), "UNK", as.character(ps_car_02_cat)))
#          ,ps_car_03_cat = factor(ifelse(is.na(ps_car_03_cat), "UNK", as.character(ps_car_03_cat)))
#          ,ps_car_05_cat = factor(ifelse(is.na(ps_car_05_cat), "UNK", as.character(ps_car_05_cat)))
#          ,ps_car_07_cat = factor(ifelse(is.na(ps_car_07_cat), "UNK", as.character(ps_car_07_cat)))
#          ,ps_car_09_cat = factor(ifelse(is.na(ps_car_09_cat), "UNK", as.character(ps_car_09_cat)))
#          ,ps_ind_02_cat = factor(ifelse(is.na(ps_ind_02_cat), "UNK", as.character(ps_ind_02_cat)))
#          ,ps_ind_04_cat = factor(ifelse(is.na(ps_ind_04_cat), "UNK", as.character(ps_ind_04_cat)))
#          ,ps_ind_05_cat = factor(ifelse(is.na(ps_ind_05_cat), "UNK", as.character(ps_ind_05_cat)))
#          ,ps_reg_03 = ifelse(is.na(ps_reg_03), mn_ps_reg_03[[1]], ps_reg_03)
#          ,ps_car_11 = ifelse(is.na(ps_car_11), mn_ps_car_11[[1]], ps_car_11)
#          ,ps_car_12 = ifelse(is.na(ps_car_12), mn_ps_car_12[[1]], ps_car_12)
#          ,ps_car_14 = ifelse(is.na(ps_car_14), mn_ps_car_14[[1]], ps_car_14)
#          ,ps_car_06_cat = as.factor( ifelse( !(as.character(ps_car_06_cat) %in% ps_car_06_cat_lst),"OTHERS",
#                                              as.character(ps_car_06_cat)) )
#          ,ps_car_11_cat = as.factor( ifelse( !(as.character(ps_car_11_cat) %in% ps_car_11_cat_lst),"OTHERS",
#                                              as.character(ps_car_11_cat)) )
#   )
# test <- test %>%
#   mutate(ps_car_01_cat = factor(ifelse(is.na(ps_car_01_cat), "UNK", as.character(ps_car_01_cat)))
#          ,ps_car_02_cat = factor(ifelse(is.na(ps_car_02_cat), "UNK", as.character(ps_car_02_cat)))
#          ,ps_car_03_cat = factor(ifelse(is.na(ps_car_03_cat), "UNK", as.character(ps_car_03_cat)))
#          ,ps_car_05_cat = factor(ifelse(is.na(ps_car_05_cat), "UNK", as.character(ps_car_05_cat)))
#          ,ps_car_07_cat = factor(ifelse(is.na(ps_car_07_cat), "UNK", as.character(ps_car_07_cat)))
#          ,ps_car_09_cat = factor(ifelse(is.na(ps_car_09_cat), "UNK", as.character(ps_car_09_cat)))
#          ,ps_ind_02_cat = factor(ifelse(is.na(ps_ind_02_cat), "UNK", as.character(ps_ind_02_cat)))
#          ,ps_ind_04_cat = factor(ifelse(is.na(ps_ind_04_cat), "UNK", as.character(ps_ind_04_cat)))
#          ,ps_ind_05_cat = factor(ifelse(is.na(ps_ind_05_cat), "UNK", as.character(ps_ind_05_cat)))
#          ,ps_reg_03 = ifelse(is.na(ps_reg_03), mn_ps_reg_03[[1]], ps_reg_03)
#          ,ps_car_11 = ifelse(is.na(ps_car_11), mn_ps_car_11[[1]], ps_car_11)
#          ,ps_car_12 = ifelse(is.na(ps_car_12), mn_ps_car_12[[1]], ps_car_12)
#          ,ps_car_14 = ifelse(is.na(ps_car_14), mn_ps_car_14[[1]], ps_car_14)
#          ,ps_car_06_cat = as.factor( ifelse( !(as.character(ps_car_06_cat) %in% ps_car_06_cat_lst),"OTHERS",
#                                              as.character(ps_car_06_cat)) )
#          ,ps_car_11_cat = as.factor( ifelse( !(as.character(ps_car_11_cat) %in% ps_car_11_cat_lst),"OTHERS",
#                                              as.character(ps_car_11_cat)) )
#   )


####################################### Apply variable transformations #################################################

# Create one-hot encoder
# onehot_encoder <- vtreat::designTreatmentsZ(train, c(catcols, numcols), minFraction = 0, verbose = TRUE)
# newvars <- onehot_encoder$scoreFrame$varName[onehot_encoder$scoreFrame$code %in% c("lev", "clean", "isBAD")]

# Apply on all datasets
train_y <- train[, names(train) %in% targetcol]
valid_y <- valid[, names(valid) %in% targetcol]
test_ids <- test[, names(test) %in% idcol]

train_x <- train %>% select(-one_of(idcol, targetcol))
valid_x <- valid %>% select(-one_of(idcol, targetcol))
test <- valid %>% select(-one_of(idcol))
valid_x <- vtreat::prepare(treatplan, valid_x, pruneSig = NULL, varRestriction = treatplan$scoreFrame$varName)
test <- vtreat::prepare(treatplan, test, pruneSig = NULL, varRestriction = treatplan$scoreFrame$varName)

# train_x <- vtreat::prepare(onehot_encoder, train, varRestriction = newvars)
# valid_x <- vtreat::prepare(onehot_encoder, valid, varRestriction = newvars)
# test <- vtreat::prepare(onehot_encoder, test, varRestriction = newvars)


# Apply PCA for dimension reduction
pcaencoder <- PCA(X = train_x, scale.unit = TRUE, ncp = 70, graph = FALSE)
# famdencoder <- FAMD(base = train_x, ncp = 57, graph = FALSE)

# Apply on all datasets
train_x <- data.frame((predict.PCA(pcaencoder, train_x))$coord)
valid_x <- data.frame((predict.PCA(pcaencoder, valid_x))$coord)
test <- data.frame((predict.PCA(pcaencoder, test))$coord)

# train_x <- data.frame((predict.FAMD(famdencoder, train_x))$coord)
# valid_x <- data.frame((predict.FAMD(famdencoder, valid_x))$coord)
# test <- data.frame((predict.FAMD(famdencoder, test))$coord)

############################### Post-transformation Data Exploration ################################################

corrmat <- cor(train %>% select(-one_of(targetcol)))
corrmat <- reorder_cormat(corrmat)
upper_tri <- get_upper_tri(corrmat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create list of columns that need to be excluded from any modelling, based on correlation being above a threshold value
exclude_columns <- (melted_cormat %>% filter(Var1 != Var2, value > 0.9))[,1] %>% as.character()



reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
















