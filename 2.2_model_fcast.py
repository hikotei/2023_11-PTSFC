
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

ignore_first = 2000
ret_df = ret_df.iloc[ignore_first:]

# train test split
split_pt = 0.8
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

for i, ret_horizon in enumerate(train_data.columns):

    # fit ARIMA models using pmdarima to find best order
    arma_models[horizon] = pm.auto_arima(train_data[horizon], start_p=1, start_q=1,
                                         max_p=max_p_q, max_q=max_p_q, trace=True,
                                         error_action='ignore',
                                         suppress_warnings=True,
                                         stepwise=True)

# - - - - - - - - - - - - - -
arma_garch_models = {}

# fit ARIMA + GARCH models using rpy2

for i, ret_horizon in enumerate(train_data.columns):

    for i, params in enumerate(arma_garch_search_params):

        mean_model = robjects.ListVector(
            {'armaOrder': robjects.IntVector([p, q])})

        variance_model = robjects.ListVector(
            {'model': "sGARCH",
            'garchOrder': robjects.IntVector([arch_p, arch_q])})

        if dist == 'std':
            model = rugarch.ugarchspec(variance_model=variance_model, mean_model=mean_model, distribution_model=dist, fixed_pars=r_std_params)

        elif dist == "norm":
            model = rugarch.ugarchspec(variance_model=variance_model, mean_model=mean_model, distribution_model=dist)
        
        model_name = f"{ret_horizon}_({p},{q})_({arch_p}, {arch_q})"
        arima_garch_model[model_name] = rugarch.ugarchfit(spec=model, data=train_data, solver='hybrid', tol=1e-3) 

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# evaluate models on remaining test set using rolling window fcast

# create results df with:

    #     model params ie p, q, arch_p, arch_q
    #     model information criteria
    #     model log likelihood

    #     in sample errors: MSE, MAE, MAPE, SMAPE
    #     out of sample fcast errors: MSE, MAE, MAPE, SMAPE

results_df = pd.DataFrame(results)

for model_name, model in arma_models.items():

    fitted_values = model.fittedvalues()
    error = model.resid()

    # rolling window forecast

    for i in range(len(test_data)):

        # fit model on training data
        model = ARIMA(train_data[:i], order=model['order'])
        model_fit = model.fit()

        # forecast one step ahead
        y_hat = model_fit.forecast()[0]

        # store forecast and ob
        forecasts.append(yhat)
        obs.append(test_data[i])

    results.append({
        'model': model_name,
        'order': model['order'],
        'in_sample_fcast': model['in_sample_fcast'],
        'errors': errors
    })

for model_name, model in arima_garch_model.items():
