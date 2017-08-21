############################### Data transformation ################################################

# Join up transactions with properties
transactions <- transactions %>% inner_join(properties, by="id_parcel") %>% select(-date)

# Explore missing values in this dataset
missing_values <- transactions %>% summarize_all(funs(sum(is.na(.))/n())) %>% gather(key="feature", value="missing_pct") %>% arrange(missing_pct)
ggplot(missing_values, aes(x = reorder(feature,-missing_pct), y = missing_pct )) +
  geom_bar(stat="identity", fill ="red") +
  coord_flip()
good_features <- filter(missing_values, missing_pct <= 0.25)

############################### Data Cleaning ################################################
# Create a dataset for modelling purposes with extra variables as required
# Drop the following columns
# zoning_property :  factor with 5000 levels, can't handle the dummy columns right now. will deal with it later
# zoning_landuse_county : factor with 241 levels. will include later
# region_zip :  factor with 405 levels, will handle later
# region_neighbor : factor with 528 levels, will handle later
# region_city : factor with 186 levels, will handle later
# censustractandblock : not sure how this is different from rawcensusandblock , and unable to understand the data. Need to revisit later

excludecolumns <- c(
  "zoning_property", "zoning_landuse_county"
  ,"censustractandblock"
  ,"region_neighbour", "region_city", "region_zip"
)
# Remove columns that cannot be used
transactions <- transactions %>% select(good_features$feature) %>% select(-one_of(idcol, excludecolumns) )

