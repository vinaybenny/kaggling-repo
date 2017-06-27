library(missMDA)
library(FactoMineR)

# Drop columns with a lot of missing values
missing_values <- transactions %>% summarize_all(funs(sum(is.na(.))/n())) %>% gather(key="feature", value="missing_pct") %>% arrange(missing_pct)
good_features <- filter(missing_values, missing_pct<=0.25)

# Create a dataset for modelling purposes with extra variables as required
# Drop the following columns
# id_parcel : dropped as this is just an ID column
# date : we already have a year-month variable used for modelling. Exact day may not be relevant, but can later be experimented with
# logerror : this is the dependent variable
# zoning_property :  factor with 5000 levels, can't handle the dummy columns right now. will deal with it later
# zoning_landuse_county : factor with 241 levels. will include later
# region_zip :  factor with 405 levels, will handle later
# region_neighbor : factor with 528 levels, will handle later
# region_city : factor with 186 levels, will handle later
# tax_year :  assessment year has only 2015 and NA as values, with NA for very few records. Can either drop the column or consider NA as another 
#             factor level.
# tax_delinquency : is not required as information captured is redundant in tax_delinquency_year
# censustractandblock : not surehow this is different from rawcensusandblock , and unable to understand the data. Need to revisit later
# build_year : Convert into derived variable build_age and then drop this variable

excludecolumns <- c("id_parcel", "date", "year_month", "logerror", "zoning_property", "zoning_landuse_county",
                    "region_zip", "region_neighbor", "region_city", "tax_year", "tax_delinquency", "censustractandblock", "build_year")

y <- transactions$logerror
modelset <- transactions %>% 
  select(good_features$feature) %>%
  mutate(month = month(transactions$year_month),
         build_age = year(transactions$year_month) - transactions$build_year) %>% 
  select(-one_of(excludecolumns))


# Other processing
#modelset <-  data.table(model.matrix(~., data = modelset))
char_cols <- names(modelset[, sapply(modelset, is.factor) ])
num_cols <- names(modelset[, sapply(modelset, is.numeric) ])



# Apply Principal Components to reduce dimensionality
#pcamodelset <- prcomp(modelset[, names(modelset) %in% num_cols ])
modelsetimpute <- imputeFAMD(modelset, ncp=5)
famdval <- FAMD(modelset, tab.comp = modelsetimpute$tab.disj, ncp = 20)


# Apply all the rules applied to training data to test data as well
test <- submission %>% 
  left_join(properties, by = "id_parcel")
test <- test %>%
  select( good_features$feature[!good_features$feature %in% c("logerror", "date", "year_month")] ) %>%
  select(-one_of(excludecolumns))


