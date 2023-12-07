
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# import libraries

import os
import numpy as np
import pandas as pd
import pmdarima as pm

from datetime import datetime
from itertools import product

import platform
if 'win' in platform.platform():
    os.environ['R_HOME'] = 'C:\Program Files\R\R-4.3.2'

import rpy2
import rpy2.robjects.packages as rpackages
import rpy2.robjects as robjects

import rpy2.robjects.numpy2ri
rpy2.robjects.numpy2ri.activate()

rugarch = rpackages.importr('rugarch')

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# import data and do train test split

today = datetime.today().strftime("%Y-%m-%d")
fname = f"2000-01-01_{today}_dax_returns.csv"

# Load data
ret_df = pd.read_csv(f"./data/{fname}", index_col=0)
ret_df.index = pd.to_datetime(ret_df.index)

ignore_first = 5500
ret_df = ret_df.iloc[ignore_first:]

# train test split
split_pt = 0.9
train_data = ret_df[:int(len(ret_df) * split_pt)]
test_data = ret_df[int(len(ret_df) * split_pt):]

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# params for model fitting

max_p_q = 5

# Define the parameter space
p_values = range(1,max_p_q)  # AR order
q_values = range(1,max_p_q)  # MA order
r_values = [1, 2]  # GARCH order
s_values = [1, 2]  # ARCH order

arma_garch_search_params = product(p_values, q_values, r_values, s_values)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# actual model fitting

# - - - - - - - - - - - - - -
arma_models = {}

print('# '*15)
print(f"start auto arima")
print('# '*15)

for i, ret_horizon in enumerate(train_data.columns):
    
    print('= '*15)
    print(f"start auto arima {ret_horizon}")
    print('= '*15)

    # fit ARIMA models using pmdarima to find best order
    arma_models[ret_horizon] = pm.auto_arima(train_data[ret_horizon], start_p=1, start_q=1,
                                             max_p=max_p_q, max_q=max_p_q, error_action='ignore')

# - - - - - - - - - - - - - -
arma_garch_models = {}

# fit ARIMA + GARCH models using rpy2

# for i, ret_horizon in enumerate(train_data.columns):

#     for i, params in enumerate(arma_garch_search_params):

#         mean_model = robjects.ListVector(
#             {'armaOrder': robjects.IntVector([p, q])})

#         variance_model = robjects.ListVector(
#             {'model': "sGARCH",
#             'garchOrder': robjects.IntVector([arch_p, arch_q])})

#         if dist == 'std':
#             model = rugarch.ugarchspec(variance_model=variance_model, mean_model=mean_model, distribution_model=dist, fixed_pars=r_std_params)

#         elif dist == "norm":
#             model = rugarch.ugarchspec(variance_model=variance_model, mean_model=mean_model, distribution_model=dist)
        
#         model_name = f"{ret_horizon}_({p},{q})_({arch_p}, {arch_q})"
#         arima_garch_model[model_name] = rugarch.ugarchfit(spec=model, data=train_data, solver='hybrid', tol=1e-3) 

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# evaluate models on remaining test set using rolling window fcast

# - - - - - - - - - - - - - -
# arma test set rolling window fcast

arma_models_eval = arma_models.copy()
test_data_arma_preds = test_data.copy()

print('# '*15)
print(f"start rolling forecast in test_data for arma models")
print('# '*15)

for ret_horizon, model in arma_models_eval.items():

    # for each row in test data predict one step ahead return
    # and update model with new data
    preds = np.zeros(len(test_data))
    conf_int = np.zeros((len(test_data), 2))

    for idx, (timestamp, test_data_row) in enumerate(test_data.iterrows()):

        print(f"{idx+1}/{len(test_data)}", end="\r")
        preds[idx], conf_int[idx] = model.predict(n_periods=1, return_conf_int=True)

        model.update(test_data_row, error_action='ignore')
        # Internally, this calls fit again 
        # using the OLD model parameters as the starting parameters for the new model’s MLE computation.


    test_data_arma_preds[f"{ret_horizon}_arma_pred"] = preds
    test_data_arma_preds[f"{ret_horizon}_lower_CI"] = conf_int[:, 0]
    test_data_arma_preds[f"{ret_horizon}_upper_CI"] = conf_int[:, 1]

    print('- '*15)
    print(f"{ret_horizon} done")
    print('- '*15)

# - - - - - - - - - - - - - -
# arma evaluation variables

arma_results_df = pd.DataFrame()

for ret_horizon in test_data.columns:

    preds = test_data_arma_preds[f"{ret_horizon}_arma_pred"]
    test_actual = test_data[ret_horizon].values
    error = test_actual - preds

    MSE = np.mean(error**2)
    MAE = np.mean(np.abs(error))
    MAPE = np.mean(np.abs(error) / np.abs(test_actual))
    SMAPE = np.mean(2 * np.abs(error) / (np.abs(test_actual) + np.abs(preds)))

    arma_model_dict = arma_models_eval[ret_horizon].to_dict()
    p, d, q = arma_model_dict['order']
    aic = arma_model_dict['aic']
    bic = arma_model_dict['bic']
    loglike = float(arma_models_eval[ret_horizon].summary().as_text().split("Log Likelihood")[1].split("\n")[0].replace(" ", ""))

    # save all results in a dataframe
    arma_results = pd.DataFrame({'ret_horizon': ret_horizon,
                                 'p': p,
                                 'd': d,
                                 'q': q,
                                 'aic': aic,
                                 'bic': bic,
                                 'loglike': loglike,
                                 'MSE': MSE,
                                 'MAE': MAE,
                                 'MAPE': MAPE,
                                 'SMAPE': SMAPE}, index=[0])
    
    # append to results_df
    arma_results_df = pd.concat([arma_results_df, arma_results], ignore_index=True)

print('# '*15)
print(f"results of arma models")
print('# '*15)

print(arma_results_df)

# - - - - - - - - - - - - - -
# do the same evaluation for the arma_garch models
# for model_name, model in arima_garch_model.items():
