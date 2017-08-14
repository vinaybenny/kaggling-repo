# Data Cleaning
# num_pool: If data in NA, assume 0 as count, or to represent unknown
# area_pool: If num_pool is NA, assume area_pool is also 0.
# num_garage: 
# tax_delinquency_year has a large number of zeros, but only when delinquency flag is 1. This calls for some transformation
# story variable exists only when area_basement exists(except 4 records). Hence this can be considered as a binary flag
# area_basement is defaulted to 0 whenever it is missing.
# area_shed is defaulted to 0 when missing, assuming that a storage shed does not exist
# area_patio is defaulted to 0 when missing, assuming that a storage shed does not exist
properties_clean <- properties %>% 
  mutate(
    num_pool = ifelse(is.na(num_pool), 0, num_pool),
    area_pool = ifelse(is.na(num_pool), 0, area_pool),
    flag_pool_with_spa = ifelse(is.na(flag_pool_with_spa), 0, flag_pool_with_spa),
    
    
    
    tax_delinquency = ifelse(tax_delinquency == "Y",1,0),
         flag_fireplace = ifelse(flag_fireplace == "true",1,0),
         flag_tub = ifelse(flag_tub == "true",1,0),
         tax_delinquency_year = ifelse(is.na(tax_delinquency_year), 16, tax_delinquency_year ) ,
         tax_delinquency_year = ifelse(tax_delinquency_year > 20, 1900 + tax_delinquency_year, 2000 + tax_delinquency_year),
         zoning_landuse_county = as.numeric(as.factor(zoning_landuse_county)),
         zoning_property = as.numeric(as.factor(zoning_property)),
         
         area_shed = ifelse(is.na(area_shed), 0, area_shed),
         area_patio = ifelse(is.na(area_patio), 0, area_patio),
         flag_deck = ifelse(flag_deck == 66, 1, 0),
         flag_story = ifelse(!is.na(area_basement), 1, 0), 
         area_basement = ifelse(!is.na(area_basement), area_basement, 0),
         flag_spa = ifelse(flag_spa == 1, 1, 0),
         flag_pool_with_spa = ifelse(flag_pool_with_spa == 1, 1, 0),
         flag_pool_without_hottub = ifelse(flag_pool_without_hottub == 1, 1, 0),
  ) %>%
  # When num pool is available, either flag_pool_with_spa or flag_pool_without_hottub is always present, but no cases where both are missing. 
  # These variables are mutally exclusive when num_pool is available. Since these two are complementary, we only need 1 of these variables
  select(-flag_pool_without_hottub)