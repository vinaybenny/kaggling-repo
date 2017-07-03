  library(tidyr)
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  
  # Read files into memory
  properties <- fread('../data/properties_2016.csv', verbose=TRUE)
  transactions <- fread('../data/train_2016.csv')
  submission <- fread('../data/sample_submission.csv', header = TRUE)
  
  # Rename columns to more meaningful values
  properties <- properties %>% rename(
    id_parcel = parcelid,
    build_year = yearbuilt,
    area_basement = basementsqft,
    area_patio = yardbuildingsqft17,
    area_shed = yardbuildingsqft26, 
    area_pool = poolsizesum,  
    area_lot = lotsizesquarefeet, 
    area_garage = garagetotalsqft,
    area_firstfloor_finished = finishedfloor1squarefeet,
    area_total_calc = calculatedfinishedsquarefeet,
    area_base = finishedsquarefeet6,
    area_live_finished = finishedsquarefeet12,
    area_liveperi_finished = finishedsquarefeet13,
    area_total_finished = finishedsquarefeet15,  
    area_unknown = finishedsquarefeet50,
    num_unit = unitcnt, 
    num_story = numberofstories,  
    num_room = roomcnt,
    num_bathroom = bathroomcnt,
    num_bedroom = bedroomcnt,
    num_bathroom_calc = calculatedbathnbr,
    num_bath = fullbathcnt,  
    num_75_bath = threequarterbathnbr, 
    num_fireplace = fireplacecnt,
    num_pool = poolcnt,  
    num_garage = garagecarcnt,  
    region_county = regionidcounty,
    region_city = regionidcity,
    region_zip = regionidzip,
    region_neighbor = regionidneighborhood,  
    tax_total = taxvaluedollarcnt,
    tax_building = structuretaxvaluedollarcnt,
    tax_land = landtaxvaluedollarcnt,
    tax_property = taxamount,
    tax_year = assessmentyear,
    tax_delinquency = taxdelinquencyflag,
    tax_delinquency_year = taxdelinquencyyear,
    zoning_property = propertyzoningdesc,
    zoning_landuse = propertylandusetypeid,
    zoning_landuse_county = propertycountylandusecode,
    flag_fireplace = fireplaceflag, 
    flag_tub = hashottuborspa,
    quality = buildingqualitytypeid,
    framing = buildingclasstypeid,
    material = typeconstructiontypeid,
    deck = decktypeid,
    story = storytypeid,
    heating = heatingorsystemtypeid,
    aircon = airconditioningtypeid,
    architectural_style= architecturalstyletypeid,
    spa = pooltypeid10,
    pool_with_spa = pooltypeid2,
    pool_without_hottub = pooltypeid7
  )
  
  # Apply datatypes to variables
  properties <- properties %>%
      mutate(
          region_county = as.factor(region_county),
          region_city = as.factor(region_city),
          region_zip = as.factor(region_zip),
          region_neighbor = as.factor(region_neighbor),
          tax_delinquency = as.factor(tax_delinquency),
          zoning_property = as.factor(zoning_property),
          zoning_landuse = as.factor(zoning_landuse),
          zoning_landuse_county = as.factor(zoning_landuse_county),
          flag_fireplace = as.factor(flag_fireplace),
          flag_tub = as.factor(flag_tub),
          quality = as.factor(quality),
          framing = as.factor(framing),
          material = as.factor(material),
          deck = as.factor(deck),
          story = as.factor(story),
          heating = as.factor(heating),
          aircon = as.factor(aircon),
          architectural_style = as.factor(architectural_style),
          spa = as.factor(spa),
          pool_with_spa = as.factor(pool_with_spa),
          pool_without_hottub = as.factor(pool_without_hottub),
          fips = as.factor(fips)
          )
  
  
  transactions <- transactions %>% rename(
    id_parcel = parcelid,
    date = transactiondate
  )
  
  
  # Data Cleaning
  # tax_delinquency_year has a large number of zeros, but only when delinquency flag is 1. This calls for some transformation,
  properties <- properties %>% 
    mutate(tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
           flag_fireplace = ifelse(flag_fireplace=="true",1,0),
           flag_tub = ifelse(flag_tub=="true",1,0),
           tax_delinquency_year = ifelse(is.na(tax_delinquency_year), -1, tax_delinquency_year ) )
  
  submission <- submission %>% rename(
    id_parcel = ParcelId
  )
  
  # Format date variable
  transactions <- transactions %>% 
    mutate(  month = month(date) )
  

  
  
  
