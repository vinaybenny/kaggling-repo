library(missMDA)
library(FactoMineR)

# Drop columns with a lot of missing values
missing_values <- properties %>% summarize_all(funs(sum(is.na(.))/n())) %>% gather(key="feature", value="missing_pct") %>% arrange(missing_pct)
good_features <- filter(missing_values, missing_pct<=0.33)
intermediate_features <- filter(missing_values, missing_pct > 0.33, missing_pct <=0.75)

# Create a dataset for modelling purposes with extra variables as required
# Drop the following columns
# id_parcel : dropped as this is just an ID column
# zoning_property :  factor with 5000 levels, can't handle the dummy columns right now. will deal with it later
# zoning_landuse_county : factor with 241 levels. will include later
# region_zip :  factor with 405 levels, will handle later
# region_neighbor : factor with 528 levels, will handle later
# region_city : factor with 186 levels, will handle later
# tax_year :  assessment year has only 2015 and NA as values, with NA for very few records. Can either drop the column or consider NA as another 
#             factor level.
# tax_delinquency : is not required as information captured is redundant in tax_delinquency_year
# censustractandblock : not surehow this is different from rawcensusandblock , and unable to understand the data. Need to revisit later

excludecolumns <- c("id_parcel"
                    ,"zoning_property", "zoning_landuse_county","region_zip", "region_neighbor", "region_city"
                    ,"tax_year", "tax_delinquency"
                    ,"censustractandblock"
                    )

propsubset <- properties %>% 
  select(good_features$feature) %>%
  select(-one_of(excludecolumns))


# Other processing
#modelset <-  data.table(model.matrix(~., data = modelset))
char_cols <- names(propsubset[, sapply(propsubset, is.factor) ])
num_cols <- names(propsubset[, sapply(propsubset, is.numeric) ])



# Apply factor analysis for imputation
propsubsetimpute <- imputeFAMD(propsubset, ncp=5, verbose=TRUE)
famdval <- FAMD(propsubset, tab.comp = propsubsetimpute$tab.disj, ncp = 20)


propsubset <- as.data.frame(famdval$ind$coord)
propsubset$id_parcel <- properties$id_parcel

# Add in extra attributes for modelliing
propsubset <-  propsubset %>% 
  left_join( select(properties, id_parcel, excludecolumns[-1], intermediate_features$feature), by="id_parcel")


# Join transactions with properties to create full dataset for analysis
transactions <- transactions %>% left_join(propsubset, by = "id_parcel") %>% arrange(id_parcel, date) 
y <- transactions$logerror
transactions <- transactions %>% select(-one_of( c("date","logerror", "id_parcel") ) )


# Apply all the rules applied to training data to test data as well
test <- submission %>% 
  left_join(propsubset, by = "id_parcel")





