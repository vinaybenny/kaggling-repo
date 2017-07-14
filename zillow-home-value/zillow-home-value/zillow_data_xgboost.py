from sklearn import model_selection, preprocessing
import xgboost as xgb


for c in x_train.dtypes[x_train.dtypes == object].index.values:
    x_train[c] = (x_train[c] == True)


X_train, X_valid, y_train, y_valid = model_selection.train_test_split(transactions.drop(['id_parcel', 'logerror'], axis = 1), 
                                    transactions.logerror.values, test_size = 0.25, random_state = 12345)
X_test = submission.merge(properties, on = 'id_parcel', how = 'left')
test_ids = X_test['id_parcel']
X_test.drop({'id_parcel', '201610', '201611', '201612', '201710', '201711', '201712'}, axis = 1, inplace=True)
X_test['month'] = 10

      
      
X_train.dtypes[X_train.dtypes == object].index.values

params = {}
params['eta'] = 0.02
params['objective'] = 'reg:linear'
params['eval_metric'] = 'mae'
params['max_depth'] = 4
params['silent'] = 1
      
d_train = xgb.DMatrix(X_train, label=y_train)
d_valid = xgb.DMatrix(X_valid, label=y_valid)
d_test = xgb.DMatrix(X_test)

watchlist = [(d_train, 'train'), (d_valid, 'valid')]
clf = xgb.train(params, d_train, 10000, watchlist, early_stopping_rounds=100, verbose_eval=10)
