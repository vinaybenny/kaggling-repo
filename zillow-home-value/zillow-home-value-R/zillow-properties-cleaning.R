library(VIM)
library(mice)
library(xlsx)


############################### Missingness Analysis ################################################
# Missing values by count in properties dataset
missing_values <- properties %>% summarize_all(funs(sum(is.na(.))/n())) %>% gather(key="feature", value="missing_pct") %>% arrange(missing_pct)
ggplot(missing_values, aes(x = reorder(feature,-missing_pct), y = missing_pct )) +
  geom_bar(stat="identity", fill ="red") +
  coord_flip()

# Obtain all combination of missingness patterns and their percentages
missing_pattern <- aggr(properties[, !names(properties) %in% missing_values[which(missing_values$missing_pct == 0), 1]], 
                        col = mdc(1:2), numbers = TRUE, labels = names(properties), cex.axis=.7, gap=3,
                        ylab=c("Proportion of missingness","Missingness Pattern"))
missing_comb <- data.frame(miss_pattern$tabcomb)
names(missing_comb)  <- names(miss_pattern$x)
missing_comb$percent <- miss_pattern$percent
write.xlsx2(missing_comb, file = "../output/missing_values_combinations.xlsx", row.names = FALSE)

# Correlation matrix for missingness in data
miss_dummy <- as.data.frame(sapply(properties, function(x){ifelse(is.na(x), 1, 0)}))
corrmat <- cor(miss_dummy[, names(miss_dummy) %in% missing_values[missing_values$missing_pct > 0, 1] ])
write.xlsx2(corrmat, file = "../output/missingness_correlation.xlsx", row.names = TRUE)
ggplot(data = melt(corrmat), aes(x = Var1, y= Var2, fill = value)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed()


############################### Data Cleaning ################################################
# num_pool: If data in NA, let's assume 0 as count, or to represent unknown- may revisit this later
# area_pool: If num_pool is NA/0 , assume area_pool is also 0.
# flag_pool_with_spa: Mutually exclusive with flag_pool_without_hottub whenever num_pool is not null. Assume 0 when null.
# flag_pool_without_hottub: As above, assume 0 when null.
# flag_spa: There are some cases where this exists without num_pool values. Strange, but assume 0 when null.
# tax_delinquency: Assume 0 when null (no delinquency).
# tax_delinquency_year: has a large number of NA, but only when delinquency flag is missing. Assume no delinquency for NAs, so the delinquency year
#       is same as current year.
# flag_story: exists only when area_basement exists(except 4 records). Hence this can be considered as a binary flag.
# area_basement: is defaulted to 0 whenever it is missing.
# area_shed: is defaulted to 0 when missing, assuming that a storage shed does not exist
# area_patio: is defaulted to 0 when missing, assuming that a patio does not exist
# flag_deck: is defaulted to 0 when missing, assuming that a deck does not exist
# num_75_bath: If null, then can be safely defaulted to 0- analysis based on num_bath and num_bathroom_calculated
# flag_fireplace: If value is true, or if num_fireplace is not NA, then this should be 1, else 0.
# num_fireplace: If flag_fireplace = 0 then this is 0. If flag_fireplace is true, but num_fireplace value is NA, assume 1 fireplace for now.
# Area variables: Assume 0 if NA or now. This may be suboptimal, but no better choce for the time being
# heating: Assume 26 = "Unknown" when this is NA for the time being
# aircon: Assume 14 = "Unknown" when this is NA for the time being
# framing: Assume 0 = "Unknown" when this is NA for the time being
# material: Assume 0 = "Unknown" when this is NA for the time being
# architectural_style: Assume 0 = "Unknown" when this is NA for the time being
# quality: Use mode value 7 when NA for the time being
# num_unit: Assume 1 (mode value) when NA for the time being
# num_garage: When num_garage is NA, area garage is always NA. This may indicate that there is no garage for the place. Hence assume 0 as count.
#             However, it is to be noted that there are 57 records with 0 valued num_garage, but with non-zero area_garage values
# num_story: Assume 0 when NA.
properties <- properties %>% 
  mutate(
    # Pool/Spa
    num_pool = ifelse(is.na(num_pool), 0, num_pool),
    area_pool = ifelse(num_pool == 0, 0, area_pool),
    flag_pool_with_spa = ifelse(is.na(flag_pool_with_spa), 0, flag_pool_with_spa),
    flag_pool_without_hottub = ifelse(is.na(flag_pool_without_hottub), 0, 1),
    flag_spa = ifelse(is.na(flag_spa), 0, 1),
    # Tax
    tax_delinquency = ifelse(tax_delinquency == "Y",1,0),
    tax_delinquency_year = ifelse(is.na(tax_delinquency_year), 16, tax_delinquency_year ) ,
    tax_delinquency_year = ifelse(tax_delinquency_year > 20, 1900 + tax_delinquency_year, 2000 + tax_delinquency_year),
    # Fireplace
    flag_fireplace = ifelse(flag_fireplace == "true" | !is.na(num_fireplace) ,1,0),
    num_fireplace = ifelse(flag_fireplace == 0, 0, num_fireplace),
    num_fireplace = ifelse(flag_fireplace == 1  & is.na(num_fireplace), 1, num_fireplace),
    
    flag_tub = ifelse(flag_tub == "true",1,0),
    
    # Stories
    flag_story = ifelse(!is.na(area_basement), 1, 0), 
    area_basement = ifelse(!is.na(area_basement), area_basement, 0),
    num_story = ifelse(is.na(num_story), 0, num_story),
    
    # Bathrooms
    num_75_bath =  (num_bathroom - num_bath)*2,
    num_75_bath = ifelse(is.na(num_75_bath), 2*(num_bathroom - floor(num_bathroom)), num_75_bath),
    num_bath = num_bathroom - (num_75_bath/2),
    
    # Area
    area_shed = ifelse(is.na(area_shed), 0, area_shed),
    area_patio = ifelse(is.na(area_patio), 0, area_patio),
    area_lot = ifelse(is.na(area_lot), 0, area_lot),
    area_garage = ifelse(is.na(area_garage), 0, area_garage),
    area_base = ifelse(is.na(area_base), 0, area_base),
    area_unknown = ifelse(is.na(area_unknown), 0, area_unknown),
    area_total_finished = ifelse(is.na(area_total_finished), 0, area_total_finished),
    area_liveperi_finished = ifelse(is.na(area_liveperi_finished), 0, area_liveperi_finished),
    area_live_finished = ifelse(is.na(area_live_finished), 0, area_live_finished),
    area_total_calc = ifelse(is.na(area_total_calc), 0, area_total_calc),
    area_firstfloor_finished = ifelse(is.na(area_firstfloor_finished), 0, area_firstfloor_finished),
    
    # Heating
    heating = relevel(factor(ifelse(is.na(as.character(heating)), "26", as.character(heating) )), ref = "26"), #26 = Unknown
    # Aircon
    aircon = relevel(factor(ifelse(is.na(as.character(aircon)), "14", as.character(aircon) )), ref = "14"), #14 = Unknown
    # Framing
    framing = relevel(factor(ifelse(is.na(as.character(framing)), "0", as.character(framing) )), ref = "0"), #0 = Unknown
    # Material
    material = relevel(factor(ifelse(is.na(as.character(material)), "0", as.character(material) )), ref = "0"), #0 = Unknown
    # Architecture
    architectural_style = relevel(factor(ifelse(is.na(as.character(architectural_style)), "0", as.character(architectural_style) )), ref = "0"), #0 = Unknown
    # Quality
    quality = ifelse(is.na(quality), 7, quality), # Use mode value
    # Units
    num_unit = ifelse(is.na(num_unit), 1, num_unit), # Use mode value
    # Garage
    num_garage = ifelse(is.na(num_garage), 0, num_garage),
    # Deck
    flag_deck = ifelse(is.na(flag_deck), 0, 1)
  ) %>%
  # When num pool is available, either flag_pool_with_spa or flag_pool_without_hottub is always present, but no cases where both are missing. 
  # These variables are mutally exclusive when num_pool is available. Since these two are complementary, we only need 1 of these variables.
  # num_bathroom_calc: can be dropped safely, since this seems to be exactly the same information but with more NAs as the num_bathroom column.
  # num_bathroom: This information is redundant in num_bath + num_75_bath
  # flag_fireplace: Information redundant in num_fireplace, except when num_fireplace is NA and flag_fireplace is true. For now, in this situation 
  #   num_fireplace is assumed to be 1. Another option is to leave num_fireplace as NA and use imputation- here we need to include flag_fireplace.
  select(-flag_pool_without_hottub
         ,-flag_story # redundant information in area_basement.
         ,-story # Copied to num_stories
         ,-tax_delinquency # Information redundant in tax_delinquency_year
         ,-num_bathroom_calc
         ,-num_bathroom
         ,-flag_fireplace 
         )

