import numpy as np
import pandas as pd
import os

os.chdir('C:\\Users\\vinay.benny\\Documents\\Kaggle\\kaggling-repo\\zillow-home-value\\zillow-home-value')

# Read the datasets
properties = pd.read_csv("../data/properties_2016.csv")
transactions = pd.read_csv("../data/train_2016.csv", parse_dates = ["transactiondate"] )
submission = pd.read_csv("../data/sample_submission.csv")

print(transactions.shape)
print(properties.shape)

# Rename properties and group columns to be more meaningful
properties.rename(columns = {
    'parcelid':'id_parcel',
    'yearbuilt':'build_year',
    'basementsqft':'area_basement',
    'yardbuildingsqft17':'area_patio',
    'yardbuildingsqft26':'area_shed',
    'poolsizesum':'area_pool',
    'lotsizesquarefeet':'area_lot',
    'garagetotalsqft':'area_garage',
    'finishedfloor1squarefeet':'area_firstfloor_finished',
    'calculatedfinishedsquarefeet':'area_total_calc',
    'finishedsquarefeet6':'area_base',
    'finishedsquarefeet12':'area_live_finished',
    'finishedsquarefeet13':'area_liveperi_finished',
    'finishedsquarefeet15':'area_total_finished',
    'finishedsquarefeet50':'area_unknown',
    'unitcnt':'num_unit',
    'numberofstories':'num_story',
    'roomcnt':'num_room',
    'bathroomcnt':'num_bathroom',
    'bedroomcnt':'num_bedroom',
    'calculatedbathnbr':'num_bathroom_calc',
    'fullbathcnt':'num_bath',
    'threequarterbathnbr':'num_75_bath',
    'fireplacecnt':'num_fireplace',
    'poolcnt':'num_pool',
    'garagecarcnt':'num_garage',
    'regionidcounty':'region_county',
    'regionidcity':'region_city',
    'regionidzip':'region_zip',
    'regionidneighborhood':'region_neighbor',
    'taxvaluedollarcnt':'tax_total',
    'structuretaxvaluedollarcnt':'tax_building',
    'landtaxvaluedollarcnt':'tax_land',
    'taxamount':'tax_property',
    'assessmentyear':'tax_year',
    'taxdelinquencyflag':'tax_delinquency',
    'taxdelinquencyyear':'tax_delinquency_year',
    'propertyzoningdesc':'zoning_property',
    'propertylandusetypeid':'zoning_landuse',
    'propertycountylandusecode':'zoning_landuse_county',
    'fireplaceflag':'flag_fireplace',
    'hashottuborspa':'flag_tub',
    'buildingqualitytypeid':'quality',
    'buildingclasstypeid':'framing',
    'typeconstructiontypeid':'material',
    'decktypeid':'deck',
    'storytypeid':'story',
    'heatingorsystemtypeid':'heating',
    'airconditioningtypeid':'aircon',
    'architectural_style':'architecturalstyletypeid',
    'pooltypeid10':'spa',
    'pooltypeid2':'pool_with_spa',
    'pooltypeid7':'pool_without_hottub'
    }, inplace = True)


transactions.rename(columns = {'parcelid' : 'id_parcel', 'transactiondate' : 'date'}, inplace = True)
submission.rename(columns = {'ParcelId' : 'id_parcel'}, inplace = True)

# Transform variables
properties['tax_delinquency'] = np.where( properties['tax_delinquency'] == "Y", 1, 0 )
properties['flag_fireplace'] = np.where( properties['flag_fireplace'] == "true", 1, 0 )
properties['flag_tub'] = np.where( properties['flag_tub'] == "true", 1, 0 )
properties['tax_delinquency_year'] = np.where( properties['tax_delinquency_year'] > 20, 
          1900 + properties['tax_delinquency_year'],  2000 + properties['tax_delinquency_year'] )
properties['tax_delinquency_year'] = np.where( pd.isnull(properties['tax_delinquency_year']), 2016, properties['tax_delinquency_year'])

transactions['month'] = transactions['date'].dt.month
transactions.drop('date', inplace = True, axis = 1)


# Recode some column datatypes
properties[['region_city', 'region_county', 'region_neighbor', 'region_zip', 'zoning_landuse_county', 
'zoning_landuse', 'zoning_property', 'quality', 'framing', 'material', 'deck', 'story', 'heating','aircon', 
'architecturalstyletypeid','spa', 'pool_with_spa', 'pool_without_hottub', 'fips']] = properties[['region_city', 
'region_county', 'region_neighbor', 'region_zip', 'zoning_landuse_county', 'zoning_landuse', 'zoning_property', 
'quality', 'framing', 'material', 'deck', 'story', 'heating','aircon', 'architecturalstyletypeid','spa', 
'pool_with_spa', 'pool_without_hottub', 'fips']].apply(pd.Categorical)



# Join the datasets together
transactions = pd.merge(transactions, properties, on = "id_parcel", how="inner")

print(transactions.shape)
transactions.head(2).transpose()
transactions.dtypes
