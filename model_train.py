# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# import libraries

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

def fit_quant_reg(X_train, y_train, quantiles):

    print('- '*15)  
    print(f'> start training quantile regression models ...')

    # start counting time
    start_time = time.time()
    all_models_quant_reg = {}

    for alpha in quantiles:

        print(f'>> alpha = {alpha:.3f} ...')
        quantile_regressor = QuantileRegressor(quantile=alpha, alpha=0, solver='highs')
        all_models_quant_reg[f"q {alpha:.3f}"] = quantile_regressor.fit(X_train, y_train)

        """ print coefficients of quant reg model to check for feasibility """
        # print(quantile_regressor.intercept_)
        # df_coef_w_names = pd.DataFrame(quantile_regressor.coef_, index=quantile_regressor.feature_names_in_)
        # print(df_coef_w_names.to_string())

    # print time taken
    print('- '*15) 
    print(f"> time taken: {time.time() - start_time:.2f} seconds")
    print('- '*15)

    return all_models_quant_reg

def fit_grad_boost(X_train, y_train, quantiles):

    print('- '*15)  
    print(f'> start training gradient boosting models ...')

    quantile_params = {0.025: {'learning_rate': 0.4, 'max_depth': 10, 'min_samples_leaf': 7, 'n_estimators': 400, 'subsample': 0.9}, 
                       0.250: {'learning_rate': 0.3, 'max_depth': 10, 'min_samples_leaf': 6, 'n_estimators': 250, 'subsample': 0.7},
                       0.500: {'learning_rate': 0.3, 'max_depth': 5, 'min_samples_leaf': 6, 'n_estimators': 400, 'subsample': 0.8},
                       0.750: {'learning_rate': 0.2, 'max_depth': 5, 'min_samples_leaf': 7, 'n_estimators': 250, 'subsample': 0.7},
                       0.975: {'learning_rate': 0.4, 'max_depth': 10, 'min_samples_leaf': 6, 'n_estimators': 400, 'subsample': 0.7}}
    
    # start counting time
    start_time = time.time()
    all_models_grad_boost = {}

    for alpha in quantiles:

        print(f'>> alpha = {alpha:.3f} ...')
        params = quantile_params[alpha]

        # Use the QuantileRegressor for faster training on small to medium datasets n < 10_000
        gbr = GradientBoostingRegressor(loss="quantile", alpha=alpha, **params)

        # Use the HistGradientBoostingRegressor for faster training on large datasets n > 10_000
        # gbr = HistGradientBoostingRegressor(loss='quantile', quantile=alpha, **params)

        all_models_grad_boost[f"q {alpha:.3f}"] = gbr.fit(X_train, y_train)

    # print time taken
    print('- '*15) 
    print(f"> time taken: {time.time() - start_time:.2f} seconds")
    print('- '*15)

    return all_models_grad_boost

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# PLOT
    
def plot_quantile_fcast(df_in, fcast_idx, title=''):

    plt.figure(figsize=(15, 5))
    plt.title(f'{title} - quantile plot')

    for quantile in df_in.columns:

        if 'timestamp' in quantile:
            continue

        y_pred = df_in[quantile]
        quantile_float = float(quantile[2:])

        # Save predicted values based on quantile
        if '0.025' in quantile:
            y_pred_025 = y_pred
            col = 'lightblue'
        elif '0.25' in quantile:
            y_pred_25 = y_pred
            col = 'mediumblue'
        elif '0.5' in quantile:
            y_pred_50 = y_pred
            col = 'black'
        elif '0.75' in quantile:
            y_pred_75 = y_pred
            col = 'mediumblue'
        elif '0.975' in quantile:
            y_pred_975 = y_pred
            col = 'lightblue'
        
        # Plot the prediction line
        plt.plot(df_in['timestamp_CET'], y_pred, label=f'{quantile}', lw=1, alpha=1, color=col)

    plt.fill_between(df_in['timestamp_CET'], y_pred_025, y_pred_975, color='lightblue', alpha=0.1)
    plt.fill_between(df_in['timestamp_CET'], y_pred_25, y_pred_75, color='mediumblue', alpha=0.1)

    # submission period 1 = fcast_idx[:3]
    # submission period 2 = fcast_idx[3:]
    plt.axvspan(fcast_idx[0], fcast_idx[2], alpha=0.2, color='grey')
    plt.axvspan(fcast_idx[3], fcast_idx[5], alpha=0.2, color='grey')

    # for timestamp in submission idx, make vertical line
    for timestamp in fcast_idx:
        plt.axvline(timestamp, color='grey', linestyle='--', lw=1)

    plt.ylim(20_000, 80_000)
    plt.legend()
    plt.show()