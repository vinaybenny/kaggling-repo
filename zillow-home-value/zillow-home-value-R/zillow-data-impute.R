# Create a dataset for modelling purposes
modelset <- transactions[, names(transactions) %in% good_features$feature]
modelset$month <- month(modelset$year_month)
modelset <- modelset[, !names(modelset) %in% c("id_parcel", "date", "year_month", "zoning_property", "zoning_landuse_county",
                                               "region_zip", "region_neighbor") ]


# Apply imputations to each column

# Apply all the rules applied to training data to test data as well
test <- submission %>% left_join(properties, by = "id_parcel")
test <- test[, names(test) %in% names(modelset)]

