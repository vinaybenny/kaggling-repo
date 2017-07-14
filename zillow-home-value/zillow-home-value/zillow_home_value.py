import missingno as msno
import matplotlib
import matplotlib.pyplot as plt
from sklearn import model_selection, preprocessing
import xgboost as xgb
matplotlib.style.use('ggplot')



# Missing value analysis
missingvalue_columns = transactions.columns[transactions.isnull().any()].tolist()
msno.bar(transactions[missingvalue_columns], figsize=(20,8), fontsize=12, labels=True,)
msno.heatmap(transactions[missingvalue_columns],figsize=(20,20), fontsize=8)

# Feature Selection with xgboost
mergedFilterd = transactions .fillna(-999)
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