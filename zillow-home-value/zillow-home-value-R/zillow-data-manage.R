  library(tidyr)
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  
  set.seed(12345)
  
  # Set working directory
  setwd("C:/Users/vinay.benny/Documents/Kaggle/kaggling-repo/zillow-home-value/zillow-home-value-R")
  
  
  ############################### Data Extraction ################################################
  
  # Read files into memory
  properties <- fread('../data/properties_2016.csv', stringsAsFactors = FALSE)
  transactions <- fread('../data/train_2016.csv', stringsAsFactors = FALSE)
  
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
    flag_deck = decktypeid,
    story = storytypeid,
    heating = heatingorsystemtypeid,
    aircon = airconditioningtypeid,
    architectural_style= architecturalstyletypeid,
    flag_spa = pooltypeid10,
    flag_pool_with_spa = pooltypeid2,
    flag_pool_without_hottub = pooltypeid7
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
          flag_fireplace = as.factor(flag_fireplace), # Binary
          flag_tub = as.factor(flag_tub), # Binary
          quality = as.integer(quality), # Ordinal
          framing = as.factor(framing),
          material = as.factor(material),
          flag_deck = as.integer(flag_deck),# Binary
          flag_story = as.integer(story), # Could be binary, representing presence of basement or not
          heating = as.factor(heating),
          aircon = as.factor(aircon),
          architectural_style = as.factor(architectural_style),
          flag_spa = as.integer(flag_spa), # Binary
          flag_pool_with_spa = as.factor(flag_pool_with_spa), # Binary
          flag_pool_without_hottub = as.factor(flag_pool_without_hottub), # Binary
          fips = as.factor(fips)
          )  
  
  # Add a month variable and rename columns
  transactions <- transactions %>% 
    rename(
      id_parcel = parcelid,
      date = transactiondate) %>% 
    mutate(  month = month(date),
             day = day(date)
    ) 
  
  
  #  Convert the datasets into data.table and let id_parcel as key
  # properties <- data.table(properties)
  # transactions  <- data.table(transactions)
  # setkey(properties, id_parcel)
  # setkey(transactions, id_parcel)
  
  
  # Set column nam variables
  idcol <- "id_parcel"
  intcols <- names(properties[, sapply(properties, is.integer) & !(names(properties) == idcol)])
  catcols <- names(properties[, sapply(properties, is.factor) & !(names(properties) == idcol)])
  numcols <- names(properties[, !names(properties) %in% c(catcols, intcols, idcol) ])
  
  
