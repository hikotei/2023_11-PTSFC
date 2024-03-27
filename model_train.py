# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

import os
import time
import pickle

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# import pmdarima as pm
from sklearn.linear_model import QuantileRegressor, LinearRegression
from sklearn.ensemble import GradientBoostingRegressor, HistGradientBoostingRegressor

from datetime import datetime
from itertools import product

import platform
if 'win' in platform.platform():
    os.environ['R_HOME'] = 'C:\Program Files\R\R-4.3.2'

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Quant Reg Model

def fit_quant_reg(X_train, y_train, quantiles, verbose=True):

    if verbose: 
        print('- '*15)  
        print(f'> start training quantile regression models ...')

        # start counting time
        start_time = time.time()
        
    all_models_quant_reg = {}

    for alpha in quantiles:

        if verbose: print(f'>> alpha = {alpha:.3f} ...')
        quantile_regressor = QuantileRegressor(quantile=alpha, alpha=0, solver='highs')
        all_models_quant_reg[f"q {alpha:.3f}"] = quantile_regressor.fit(X_train, y_train)

        """ print coefficients of quant reg model to check for feasibility """
        # print(quantile_regressor.intercept_)
        # df_coef_w_names = pd.DataFrame(quantile_regressor.coef_, index=quantile_regressor.feature_names_in_)
        # print(df_coef_w_names.to_string())

    if verbose: 
        # print time taken
        print('- '*15) 
        print(f"> time taken: {time.time() - start_time:.2f} seconds")
        print('- '*15)

    return all_models_quant_reg

def fit_grad_boost(X_train, y_train, quantiles, verbose=True):

    if verbose:
        print('- '*15)  
        print(f'> start training gradient boosting models ...')

        # start counting time
        start_time = time.time()

    quantile_params = {0.025: {'learning_rate': 0.4, 'max_depth': 10, 'min_samples_leaf': 7, 'n_estimators': 400, 'subsample': 0.9}, 
                       0.250: {'learning_rate': 0.3, 'max_depth': 10, 'min_samples_leaf': 6, 'n_estimators': 250, 'subsample': 0.7},
                       0.500: {'learning_rate': 0.3, 'max_depth': 5, 'min_samples_leaf': 6, 'n_estimators': 400, 'subsample': 0.8},
                       0.750: {'learning_rate': 0.2, 'max_depth': 5, 'min_samples_leaf': 7, 'n_estimators': 250, 'subsample': 0.7},
                       0.975: {'learning_rate': 0.4, 'max_depth': 10, 'min_samples_leaf': 6, 'n_estimators': 400, 'subsample': 0.7}}
    
    all_models_grad_boost = {}

    for alpha in quantiles:

        if verbose: print(f'>> alpha = {alpha:.3f} ...')
        params = quantile_params[alpha]

        # Use the QuantileRegressor for faster training on small to medium datasets n < 10_000
        gbr = GradientBoostingRegressor(loss="quantile", alpha=alpha, **params)

        # Use the HistGradientBoostingRegressor for faster training on large datasets n > 10_000
        # gbr = HistGradientBoostingRegressor(loss='quantile', quantile=alpha, **params)

        all_models_grad_boost[f"q {alpha:.3f}"] = gbr.fit(X_train, y_train)

    if verbose:
        # print time taken
        print('- '*15) 
        print(f"> time taken: {time.time() - start_time:.2f} seconds")
        print('- '*15)

    return all_models_grad_boost

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# PLOT
    
def plot_quantile_fcast(df_in, fcast_idx, y_true=None, title=''):

    plt.figure(figsize=(15, 5))
    plt.title(f'{title} - quantile plot')

    cols = ['temp', 'lightblue', 'mediumblue', 'black', 'mediumblue', 'lightblue']

    for idx, quantile in enumerate(df_in.columns):
        if 'timestamp' in quantile:
            continue # skip timestamp column
        y_pred = df_in[quantile]
        plt.plot(df_in['timestamp_CET'], y_pred, label=f'{quantile}', lw=1, alpha=1, color=cols[idx])

    plt.fill_between(df_in['timestamp_CET'], df_in['q 0.025'], df_in['q 0.975'], color='lightblue', alpha=0.1)
    plt.fill_between(df_in['timestamp_CET'], df_in['q 0.250'], df_in['q 0.750'], color='mediumblue', alpha=0.1)

    plt.axvspan(fcast_idx[0], fcast_idx[2], alpha=0.2, color='grey')
    plt.axvspan(fcast_idx[3], fcast_idx[5], alpha=0.2, color='grey')

    # for timestamp in submission idx, make vertical line
    for timestamp in fcast_idx:
        plt.axvline(timestamp, color='grey', linestyle='--', lw=1)

    # plot true values if available
    if y_true is not None:
        plt.plot(y_true['timestamp_CET'], y_true['gesamt'], lw=1, color='red', label='actual')

    plt.ylim(20_000, 85_000)
    plt.legend()
    plt.show()