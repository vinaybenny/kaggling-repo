import numpy as np
import pandas as pd
import os
import pypyodbc
import missingno as msno
import matplotlib
import matplotlib.pyplot as plt
from sklearn import model_selection, preprocessing
import xgboost as xgb
matplotlib.style.use('ggplot')



# Read the datasets
train = pd.read_csv("../data/train_2016.csv", parse_dates = ["transactiondate"] )
properties = pd.read_csv("../data/properties_2016.csv")
heatingorsystemtype = pd.read_excel("../data/zillow_data_dictionary.xlsx", sheetname = "HeatingOrSystemTypeID")
propertylandusetype = pd.read_excel("../data/zillow_data_dictionary.xlsx", sheetname = "PropertyLandUseTypeID")
storytype = pd.read_excel("../data/zillow_data_dictionary.xlsx", sheetname = "StoryTypeID")
airconditioningtype = pd.read_excel("../data/zillow_data_dictionary.xlsx", sheetname = "AirConditioningTypeID")
architecturalstyletype = pd.read_excel("../data/zillow_data_dictionary.xlsx", sheetname = "ArchitecturalStyleTypeID")
typeconstructiontype = pd.read_excel("../data/zillow_data_dictionary.xlsx", sheetname = "TypeConstructionTypeID")
buildingclasstype = pd.read_excel("../data/zillow_data_dictionary.xlsx", sheetname = "BuildingClassTypeID")

print(train.shape)
print(properties.shape)


# Join the datasets together
merged = pd.merge(train, properties, on = "parcelid", how="left")
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

# Missing value analysis
missingvalue_columns = merged.columns[merged.isnull().any()].tolist()
msno.bar(merged[missingvalue_columns], figsize=(20,8), fontsize=12, labels=True,)
msno.heatmap(merged[missingvalue_columns],figsize=(20,20), fontsize=8)

# Feature Selection with xgboost
mergedFilterd = merged.fillna(-999)
for f in mergedFilterd.columns:
    if mergedFilterd[f].dtype=='object':
        lbl = preprocessing.LabelEncoder()
        lbl.fit(list(mergedFilterd[f].values)) 
        mergedFilterd[f] = lbl.transform(list(mergedFilterd[f].values))
        
train_y = mergedFilterd.logerror.values
train_X = mergedFilterd.drop(["parcelid", "transactiondate", "logerror"], axis=1)

xgb_params = {
    'eta': 0.05,
    'max_depth': 8,
    'subsample': 0.7,
    'colsample_bytree': 0.7,
    'objective': 'reg:linear',
    'eval_metric': 'rmse',
    'silent': 1
}
dtrain = xgb.DMatrix(train_X, train_y, feature_names=train_X.columns.values)
model = xgb.train(dict(xgb_params, silent=0), dtrain, num_boost_round=100)

featureImportance = model.get_fscore()
features = pd.DataFrame()
features['features'] = featureImportance.keys()
features['importance'] = featureImportance.values()
features.sort_values(by=['importance'],ascending=False,inplace=True)
fig,ax= plt.subplots()
fig.set_size_inches(20,10)
plt.xticks(rotation=90)
plt.show()