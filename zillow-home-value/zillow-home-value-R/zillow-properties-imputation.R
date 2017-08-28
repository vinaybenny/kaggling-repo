library(missMDA)
library(FactoMineR)

# Apply factor analysis for imputation
training <- transactions
# training <- imputeFAMD(data.frame(transactions), ncp=5, verbose=TRUE)
# famdval <- PCA(trainingimpute,  ncp = 20)

# Join transactions with properties to create full dataset for analysis
y <- training$logerror
training <- training %>% select(-one_of( c("logerror","id_parcel") ) )


# Apply all the rules applied to training data to test data as well
test <- properties %>% select(one_of(names(transactions)))





