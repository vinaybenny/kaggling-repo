# ================================================================================================ #
# Description: Perform cleaning and application of business rules on transactions data
# 
# 
# Author: V Benny
#
# ================================================================================================ #

############################### Data transformation ################################################

# Join up transactions with properties
transactions <- transactions %>% inner_join(properties, by="id_parcel") %>% select(-date)

# Explore missing values in this dataset
missing_values <- transactions %>% summarize_all(funs(sum(is.na(.))/n())) %>% 
  gather(key="feature", value="missing_pct") %>% 
  arrange(missing_pct)
ggplot(missing_values, aes(x = reorder(feature,-missing_pct), y = missing_pct )) +
  geom_bar(stat="identity", fill ="red") +
  coord_flip()
good_features <- filter(missing_values, missing_pct <= 0.25)

############################### Data Cleaning ################################################
# Create a dataset for modelling purposes with extra variables as required
# Drop the following columns
# censustractandblock : not sure how this is different from rawcensusandblock , and unable to understand the data. Need to revisit later

excludecolumns <- c("none")
# Remove columns that cannot be used
transactions <- transactions %>% select(good_features$feature) %>% select(-one_of(excludecolumns) )




