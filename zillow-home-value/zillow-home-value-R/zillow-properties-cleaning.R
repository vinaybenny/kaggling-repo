############################### Data Cleaning ################################################
# num_pool: If data in NA, let's assume 0 as count, or to represent unknown- may revisit this later
# area_pool: If num_pool is NA, assume area_pool is also 0.
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
properties <- properties %>% 
  mutate(
    # Pool/Spa
    num_pool = ifelse(is.na(num_pool), 0, num_pool),
    area_pool = ifelse(is.na(num_pool), 0, area_pool),
    flag_pool_with_spa = ifelse(is.na(flag_pool_with_spa), 0, flag_pool_with_spa),
    flag_pool_without_hottub = ifelse(is.na(flag_pool_without_hottub), 0, 1),
    flag_spa = ifelse(is.na(flag_spa), 0, 1),
    # Tax
    tax_delinquency = ifelse(tax_delinquency == "Y",1,0),
    tax_delinquency_year = ifelse(is.na(tax_delinquency_year), 16, tax_delinquency_year ) ,
    tax_delinquency_year = ifelse(tax_delinquency_year > 20, 1900 + tax_delinquency_year, 2000 + tax_delinquency_year),
    
    flag_fireplace = ifelse(flag_fireplace == "true",1,0),
    flag_tub = ifelse(flag_tub == "true",1,0),
    
    # Stories
    flag_story = ifelse(!is.na(area_basement), 1, 0), 
    area_basement = ifelse(!is.na(area_basement), area_basement, 0),
         
    area_shed = ifelse(is.na(area_shed), 0, area_shed),
    area_patio = ifelse(is.na(area_patio), 0, area_patio),
    flag_deck = ifelse(is.na(flag_deck), 0, 1)
  ) %>%
  # When num pool is available, either flag_pool_with_spa or flag_pool_without_hottub is always present, but no cases where both are missing. 
  # These variables are mutally exclusive when num_pool is available. Since these two are complementary, we only need 1 of these variables.
  select(-flag_pool_without_hottub
         ,-flag_story # redundant information in area_basement.
         ,-story # Copied to num_stories
         ,-tax_delinquency # Information redundant in tax_delinquency_year
         )

