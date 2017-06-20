import numpy as np
import pandas as pd

# Read the datasets
properties = pd.read_csv("../data/properties_2016.csv")
transactions = pd.read_csv("../data/train_2016.csv", parse_dates = ["transactiondate"] )
heatingorsystemtype = pd.read_excel("../data/zillow_data_dictionary.xlsx", sheetname = "HeatingOrSystemTypeID")
propertylandusetype = pd.read_excel("../data/zillow_data_dictionary.xlsx", sheetname = "PropertyLandUseTypeID")
storytype = pd.read_excel("../data/zillow_data_dictionary.xlsx", sheetname = "StoryTypeID")
airconditioningtype = pd.read_excel("../data/zillow_data_dictionary.xlsx", sheetname = "AirConditioningTypeID")
architecturalstyletype = pd.read_excel("../data/zillow_data_dictionary.xlsx", sheetname = "ArchitecturalStyleTypeID")
typeconstructiontype = pd.read_excel("../data/zillow_data_dictionary.xlsx", sheetname = "TypeConstructionTypeID")
buildingclasstype = pd.read_excel("../data/zillow_data_dictionary.xlsx", sheetname = "BuildingClassTypeID")

print(transactions.shape)
print(properties.shape)

# Rename properties and group columns to be more meaningful
properties.rename({
    'id_parcel':'parcelid',
    'build_year':'yearbuilt',
    'area_basement':'basementsqft',
    'area_patio':'yardbuildingsqft17',
    'area_shed':'yardbuildingsqft26',
    'area_pool':'poolsizesum',
    'area_lot':'lotsizesquarefeet',
    'area_garage':'garagetotalsqft',
    'area_firstfloor_finished':'finishedfloor1squarefeet',
    'area_total_calc':'calculatedfinishedsquarefeet',
    'area_base':'finishedsquarefeet6',
    'area_live_finished':'finishedsquarefeet12',
    'area_liveperi_finished':'finishedsquarefeet13',
    'area_total_finished':'finishedsquarefeet15',
    'area_live_firstfloor':'finishedsquarefeet50',
    'num_unit':'unitcnt',
    'num_story':'numberofstories',
    'num_room':'roomcnt',
    'num_bathroom':'bathroomcnt',
    'num_bedroom':'bedroomcnt',
    'num_bathroom_calc':'calculatedbathnbr',
    'num_bath':'fullbathcnt',
    'num_75_bath':'threequarterbathnbr',
    'num_fireplace':'fireplacecnt',
    'num_pool':'poolcnt',
    'num_garage':'garagecarcnt',
    'region_county':'regionidcounty',
    'region_city':'regionidcity',
    'region_zip':'regionidzip',
    'region_neighbor':'regionidneighborhood',
    'tax_total':'taxvaluedollarcnt',
    'tax_building':'structuretaxvaluedollarcnt',
    'tax_land':'landtaxvaluedollarcnt',
    'tax_property':'taxamount',
    'tax_year':'assessmentyear',
    'tax_delinquency':'taxdelinquencyflag',
    'tax_delinquency_year':'taxdelinquencyyear',
    'zoning_property':'propertyzoningdesc',
    'zoning_landuse':'propertylandusetypeid',
    'zoning_landuse_county':'propertycountylandusecode',
    'flag_fireplace':'fireplaceflag',
    'flag_tub':'hashottuborspa',
    'quality':'buildingqualitytypeid',
    'framing':'buildingclasstypeid',
    'material':'typeconstructiontypeid',
    'deck':'decktypeid',
    'story':'storytypeid',
    'heating':'heatingorsystemtypeid',
    'aircon':'airconditioningtypeid',
    'architectural_style':'architecturalstyletypeid'
    }, inplace = True)

transactions.rename({
  'id_parcel' : 'parcelid',
  'date' : 'transactiondate'
    }, inplace = True)


# Join the datasets together
merged = pd.merge(transactions, properties, on = "parcelid", how="left")
print(merged.shape)
merged.dtypes



merged = pd.merge(merged, heatingorsystemtype, left_on = "heatingorsystemtypeid", right_on = "HeatingOrSystemTypeID", how="left")
merged = pd.merge(merged, propertylandusetype, left_on = "propertylandusetypeid", right_on = "PropertyLandUseTypeID", how="left")
merged = pd.merge(merged, storytype, left_on = "storytypeid", right_on = "StoryTypeID", how="left")
merged = pd.merge(merged, airconditioningtype, left_on = "airconditioningtypeid", right_on = "AirConditioningTypeID", how="left")
merged = pd.merge(merged, architecturalstyletype, left_on = "architecturalstyletypeid", right_on = "ArchitecturalStyleTypeID", how="left")
merged = pd.merge(merged, typeconstructiontype, left_on = "typeconstructiontypeid", right_on = "TypeConstructionTypeID", how="left")
#merged = pd.merge(merged, buildingclasstype, left_on = "buildingclasstypeid", right_on = "BuildingClassTypeID", how="left")
merged.drop("HeatingOrSystemTypeID", 1, inplace=True)
merged.drop("heatingorsystemtypeid", 1, inplace=True)
merged.drop("PropertyLandUseTypeID", 1, inplace=True)
merged.drop("propertylandusetypeid", 1, inplace=True)
merged.drop("StoryTypeID", 1, inplace=True)
merged.drop("storytypeid", 1, inplace=True)
merged.drop("AirConditioningTypeID", 1, inplace=True)
merged.drop("airconditioningtypeid", 1, inplace=True)
merged.drop("ArchitecturalStyleTypeID", 1, inplace=True)
merged.drop("architecturalstyletypeid", 1, inplace=True)
merged.drop("TypeConstructionTypeID", 1, inplace=True)
merged.drop("typeconstructiontypeid", 1, inplace=True)
#merged.drop("BuildingClassTypeID", 1, inplace=True)


print(merged.shape)
merged.head(2).transpose()
merged.dtypes
