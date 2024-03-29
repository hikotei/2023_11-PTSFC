import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import (
    mean_absolute_error,
    mean_squared_error,
    mean_absolute_percentage_error,
    mean_pinball_loss,
)

def eval_fcast_w_plot(df_fcast, df_energy_new, subm_timestamps, quantiles, ylims=[20_000, 80_000]):

    """ 
    imported data up to (and including) sunday of the fcast week
    first subset the required data from df_energy_new ...ie t_wednesday to t_sunday
    then plot the iterative fcast from thursday to sunday
    against the actual values in the same plot for comparison

    """
    # plot all actual values inbetween fcast timeframe
    mask = (df_energy_new['timestamp_CET'] >= df_fcast['timestamp_CET'].min()) & (df_energy_new['timestamp_CET'] <= df_fcast['timestamp_CET'].max())
    actual_df = df_energy_new.loc[mask]

    # plot actual values only for the fcast timestamps
    # actual_df = df_energy_new.loc[df_energy_new['timestamp_CET'].isin(df_fcast['timestamp_CET'])]

    # = = = = = = = = = = = = = 
    # plot

    plt.figure(figsize=(15, 5))
    plt.title(f'quantile fcast evaluation')

    cols = ['temp', 'lightblue', 'mediumblue', 'black', 'mediumblue', 'lightblue']
    for idx, quantile in enumerate(df_fcast.columns):
        if 'timestamp' in quantile:
            continue
        y_pred = df_fcast[quantile]
        # Plot the prediction line
        plt.plot(df_fcast['timestamp_CET'], y_pred, label=f'{quantile}', lw=1, alpha=1, color=cols[idx])

    plt.fill_between(df_fcast['timestamp_CET'], df_fcast['q 0.025'], df_fcast['q 0.975'], color='lightblue', alpha=0.1)
    plt.fill_between(df_fcast['timestamp_CET'], df_fcast['q 0.250'], df_fcast['q 0.750'], color='mediumblue', alpha=0.1)

    # highlight submission timeframes
    plt.axvspan(subm_timestamps[0], subm_timestamps[2], alpha=0.2, color='grey')
    plt.axvspan(subm_timestamps[3], subm_timestamps[5], alpha=0.2, color='grey')

    # for timestamp in submission idx, make vertical line
    for timestamp in subm_timestamps:
        plt.axvline(timestamp, color='grey', linestyle='--', lw=1)

    # plot actual values
    plt.plot(actual_df['timestamp_CET'], actual_df['gesamt'], lw=1, label='actual', color='red')

    plt.ylim(ylims)
    plt.legend()
    plt.show()

    """ 
    to calculate the error, we can only subset the actual values for the fcast timestamps
    because otherwise the actual_df will have more values than the fcast_df
    
    now that everything lines up ... we can compute the quantile scores 
    and other error metrics such as MAE, MAPE, MSE, etc
    do it for the entire iterative fcast and also for the 6 submission fcast timestamps

    """

    actual_df = df_energy_new.loc[df_energy_new['timestamp_CET'].isin(df_fcast['timestamp_CET'])]

    # for entire iterative fcast
    # ==========================
    print('- '*15)
    print(f"entire iterative fcast")
    print('- '*15)

    mae = mean_absolute_error(actual_df['gesamt'], df_fcast['q 0.500'])
    print(f"MAE = {mae}")
    mse = mean_squared_error(actual_df['gesamt'], df_fcast['q 0.500'])
    print(f"MSE = {mse}")
    mape = mean_absolute_percentage_error(actual_df['gesamt'], df_fcast['q 0.500'])
    print(f"MAPE = {mape}")

    quantile_scores = []
    for q in quantiles:
        quantile_scores.append(mean_pinball_loss(actual_df['gesamt'], df_fcast[f'q {q:.3f}']))
    print(quantile_scores)

    # for submission fcast timestamps
    # ===============================
    print('- '*15)
    print(f"submission fcast timestamps")
    print('- '*15)

    mae = mean_absolute_error(actual_df.loc[actual_df['timestamp_CET'].isin(subm_timestamps), 'gesamt'].values, 
                              df_fcast.loc[df_fcast['timestamp_CET'].isin(subm_timestamps), 'q 0.500'].values)
    print(f"MAE q 0.5 = {mae}")

    df_error_metrics = pd.DataFrame(index=subm_timestamps)
    for timestamp in subm_timestamps:
        mae = mean_absolute_error(actual_df.loc[actual_df['timestamp_CET'] == timestamp, 'gesamt'].values,
                                df_fcast.loc[df_fcast['timestamp_CET'] == timestamp, 'q 0.500'].values)
        df_error_metrics.loc[timestamp, 'abs error q 0.5'] = mae

        for q in quantiles:
            quantile_score = mean_pinball_loss(actual_df.loc[actual_df['timestamp_CET'] == timestamp, 'gesamt'].values,
                                               df_fcast.loc[df_fcast['timestamp_CET'] == timestamp, f'q {q:.3f}'].values,
                                               alpha=q) / 1000 * 2
            # save in df
            df_error_metrics.loc[timestamp, f'q-score {q:.3f}'] = quantile_score

    # add last row to df_error_metrics which is the avg of the cols
    df_error_metrics.loc['avg', :] = df_error_metrics.mean(axis=0)
    
    return df_error_metrics

def eval_fcast(df_fcast, df_energy_new, subm_timestamps, quantiles):
    """
    to calculate the error, we can only subset the actual values for the fcast timestamps
    because otherwise the actual_df will have more values than the fcast_df

    after everything lines up ... we can compute the quantile scores
    and other error metrics such as MAE, MAPE, MSE, etc
    do it for the entire iterative fcast and also for the 6 submission fcast timestamps

    """

    actual_df = df_energy_new.loc[
        df_energy_new["timestamp_CET"].isin(df_fcast["timestamp_CET"])
    ]

    # for submission fcast timestamps
    # ===============================
    # print("- " * 15)
    # print("submission fcast timestamps")
    # print("- " * 15)

    mae = mean_absolute_error(
        actual_df.loc[
            actual_df["timestamp_CET"].isin(subm_timestamps), "gesamt"
        ].values,
        df_fcast.loc[
            df_fcast["timestamp_CET"].isin(subm_timestamps), "q 0.500"
        ].values,
    )
    # print(f"MAE q 0.5 = {mae}")

    df_error_metrics = pd.DataFrame(index=subm_timestamps)
    for timestamp in subm_timestamps:
        try:
            mae = mean_absolute_error(
                actual_df.loc[actual_df["timestamp_CET"] == timestamp, "gesamt"].values,
                df_fcast.loc[df_fcast["timestamp_CET"] == timestamp, "q 0.500"].values,
            )
            df_error_metrics.loc[timestamp, "abs error q 0.5"] = mae

            for q in quantiles:
                quantile_score = (
                    mean_pinball_loss(
                        actual_df.loc[
                            actual_df["timestamp_CET"] == timestamp, "gesamt"
                        ].values,
                        df_fcast.loc[
                            df_fcast["timestamp_CET"] == timestamp, f"q {q:.3f}"
                        ].values,
                        alpha=q,
                    )
                    / 1000
                    * 2
                )
                # save in df
                df_error_metrics.loc[timestamp, f"q-score {q:.3f}"] = quantile_score
        except:
            print(f"error computing error metrics for {timestamp}")

    # add last row to df_error_metrics which is the avg of the cols
    # df_error_metrics.loc["avg", :] = df_error_metrics.mean(axis=0)

    return df_error_metrics

def eval_fcast_qscore(df_fcast, df_energy_new, subm_timestamps, quantiles):

    actual_df = df_energy_new.loc[
        df_energy_new["timestamp_CET"].isin(df_fcast["timestamp_CET"])
    ]


    df_error_metrics = pd.DataFrame(index=subm_timestamps)
    for timestamp in subm_timestamps:
        try:
            for q in quantiles:
                quantile_score = (
                    mean_pinball_loss(
                        actual_df.loc[
                            actual_df["timestamp_CET"] == timestamp, "gesamt"
                        ].values,
                        df_fcast.loc[
                            df_fcast["timestamp_CET"] == timestamp, f"q {q:.3f}"
                        ].values,
                        alpha=q,
                    )
                    / 1000
                    * 2
                )
                # save in df
                df_error_metrics.loc[timestamp, f"q-score {q:.3f}"] = quantile_score
        except:
            print(f"error computing error metrics for {timestamp}")

    # add last row to df_error_metrics which is the avg of the cols
    # df_error_metrics.loc["avg", :] = df_error_metrics.mean(axis=0)

    return df_error_metrics

