# Create a dataset for modelling purposes
modelset <- transactions[, names(transactions) %in% good_features$feature]

# Apply imputations to each column

