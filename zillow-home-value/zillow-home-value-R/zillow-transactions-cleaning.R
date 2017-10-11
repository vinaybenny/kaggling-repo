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


# Try a glm model on the dataset
res.lm  <-lm(formula = logerror ~ ., data = transactions %>% select(-one_of(idcol)) )
cooksd <- cooks.distance(res.lm)

# Remove extreme outliers from transactions
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 10*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

outlier_ids <- data.frame(col = as.numeric(names(cooksd)), value = cooksd) %>% 
  filter(!is.na(value)) %>% 
  filter(value > quantile(value, prob = c(0.99))) %>% 
  select(col)
transactions <- transactions[-outlier_ids$col,]

