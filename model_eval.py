import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics import (
    mean_absolute_error,
    mean_squared_error,
    mean_absolute_percentage_error,
    mean_pinball_loss,
)


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
    df_error_metrics.loc["avg", :] = df_error_metrics.mean(axis=0)

    return df_error_metrics
