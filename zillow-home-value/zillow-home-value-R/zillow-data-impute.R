library(missMDA)
library(FactoMineR)

# Drop columns with a lot of missing values
missing_values <- properties %>% summarize_all(funs(sum(is.na(.))/n())) %>% gather(key="feature", value="missing_pct") %>% arrange(missing_pct)
good_features <- filter(missing_values, missing_pct<=0.75)

# Create a dataset for modelling purposes with extra variables as required
# Drop the following columns
# zoning_property :  factor with 5000 levels, can't handle the dummy columns right now. will deal with it later
# zoning_landuse_county : factor with 241 levels. will include later
# region_zip :  factor with 405 levels, will handle later
# region_neighbor : factor with 528 levels, will handle later
# region_city : factor with 186 levels, will handle later
# tax_year :  assessment year has only 2015 and NA as values, with NA for very few records. Can either drop the column or consider NA as another 
#             factor level.
# tax_delinquency : is not required as information captured is redundant in tax_delinquency_year
# censustractandblock : not sure how this is different from rawcensusandblock , and unable to understand the data. Need to revisit later

excludecolumns <- c(
                    #"zoning_property", "zoning_landuse_county"
                    #,"region_zip", "region_neighbor", "region_city"
                    "tax_year", "tax_delinquency"
                    ,"censustractandblock"
                    )

training <- properties %>% 
  select(good_features$feature) %>%
  select(-one_of(excludecolumns))


# Other processing
# modelset <-  data.table(model.matrix(~., data = modelset))
# char_cols <- names(propsubset[, sapply(propsubset, is.factor) ])
# num_cols <- names(propsubset[, sapply(propsubset, is.numeric) ])

# Apply factor analysis for imputation
trainingimpute <- imputeFAMD(data.frame(training), ncp=5, verbose=TRUE)
famdval <- PCA(trainingimpute,  ncp = 20)

# Join transactions with properties to create full dataset for analysis
training <- training[transactions]
y <- training$logerror
training <- training %>% select(-one_of( c("date","logerror","id_parcel") ) )


# Apply all the rules applied to training data to test data as well
test <- properties %>% select(one_of(names(training)))





