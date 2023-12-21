# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =	
# Import libraries
import os
import numpy as np
import pandas as pd
import yfinance as yf
from datetime import datetime

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =	
# Get DAX Data

"""
data from 2015-01-01 to 2023-11-30 is already saved in .csv file
only need to update data from last available date to today

"""

def get_dax_data(start="2000-01-01", end="2023-11-01"):
    
    # parse datetime from start and end
    start = datetime.strptime(start, "%Y-%m-%d")
    end = datetime.strptime(end, "%Y-%m-%d")

    # get data from yahoo finance
    stock = yf.Ticker("^GDAXI") 
    hist = stock.history(start=start, end=end)
    hist = hist.set_index(hist.index.date)

    # save data to csv file
    fname = f"{start.strftime('%Y-%m-%d')}_{end.strftime('%Y-%m-%d')}_dax_prices"
    hist.to_csv(f"./data/{fname}.csv")

    return None

def compute_return(y, r_type="log", h=1):
    
    """
    r_type = return type, by default percentage return, otherwise log return
    """

    # exclude first h observations
    y2 = y[h:]
    # exclude last h observations
    y1 = y[:-h]
    
    if r_type == "log":
        ret = np.concatenate(([np.nan]*h, 100 * (np.log(y2) - np.log(y1))))
    else:
        ret = np.concatenate(([np.nan]*h, 100 * (y2-y1)/y1))
        
    return ret

def get_dax_returns(historic_fname, recent_fname):

    # get historic returns
    hist = pd.read_csv(f"./data/{historic_fname}.csv", index_col=0, parse_dates=True)
    # get recent returns
    recent = pd.read_csv(f"./data/{recent_fname}.csv", index_col=0, parse_dates=True)

    # row bind historic and recent returns
    dax = pd.concat([hist, recent], axis=0)

    # compute returns
    for i in range(5):
        dax["ret"+str(i+1)] = compute_return(dax["Close"].values, h=i+1)

    # only keep return columns
    dax = dax[["ret1", "ret2", "ret3", "ret4", "ret5"]]

    # remove NA rows
    dax = dax.dropna()

    return dax

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =	
# Get Energy Data

""" 
data from 2015-01-01 to 2023-10-31 is already saved in .csv file
only need to update data from last available date to today

add weekday information
add weekend and holiday indicator variables (maybe separate important holidays eg newyear & christmas)
add temperature and weather data (population weighted?)

"""

def get_energy_data_today(to_date=None) :

    os.chdir("C:/Users/ytl_c/OneDrive/Desktop/23_24 WS (Master)/VL - PTSFC/2023_11-PTSFC")
    print(os.getcwd())

    # if input param to_date is not given, use today's date
    if to_date is None :
        today = datetime.today().strftime("%Y%m%d")
    else :
        today = to_date

    hist_fname = f"Realisierter_Stromverbrauch_201501010000_202310312359_Viertelstunde.csv"
    recent_fname = f"Realisierter_Stromverbrauch_202311010000_{today}2359_Viertelstunde.csv"

    # Check if file exists first before loading data
    if not os.path.isfile(f"./data/{recent_fname}"):
        print(f"{recent_fname} does not exist, first download data from SMARD !")
        return None

    # Load data
    hist_df = pd.read_csv(f"./data/{hist_fname}", sep=";", decimal=",")
    recent_df = pd.read_csv(f"./data/{recent_fname}", sep=";", decimal=",")

    # merge 2 dataframes
    df = pd.concat([hist_df, recent_df])

    # Rename columns for convenience
    df.columns = ["datum", "anfang", "ende", "gesamt", "residual", "pump"]

    # replace '-' with NA
    df.replace('-', pd.NA, inplace=True)

    # Merge date and time column and set that as the index
    df["timestamp"] = pd.to_datetime(df['datum'] + ' ' + df['anfang'], format='%d.%m.%Y %H:%M')

    # need to choose anfang here !!!
    # otherwise in DST switching end of march we will have 3am twice

    # change datetime to utc time
    df['timestamp_CET'] = df['timestamp'].dt.tz_localize('CET', ambiguous='infer')
    df['timestamp_UTC'] = df['timestamp_CET'].dt.tz_convert('UTC')
    df.set_index("timestamp_UTC", inplace=True)
    
    # make index datetime object
    df.index = pd.to_datetime(df.index)

    df_utc = df[['gesamt', 'timestamp_CET']].copy()

    # Replace "." with "" and then replace "," with "."
    df_utc[['gesamt']] = df_utc[['gesamt']].apply(lambda x: x.str.replace('.', '', regex=False))
    df_utc[['gesamt']] = df_utc[['gesamt']].apply(lambda x: x.str.replace(',', '.', regex=False))
    df_utc[['gesamt']] = df_utc[['gesamt']].apply(pd.to_numeric)

    # if there are consecutive trailing NaNs, drop them ... find last valid index
    last_valid_index = df_utc['gesamt'].last_valid_index()

    if last_valid_index is not None:
        df_utc = df_utc.loc[:last_valid_index]

    print(f"{df_utc['gesamt'].isna().sum()} NA in df")

    # Interpolate missing values in between
    df_utc_interp = df_utc.copy()
    df_utc_interp['gesamt'].interpolate(method='time', inplace=True)

    df_hourly = df_utc_interp.resample("1h", label="left").agg({'gesamt':'sum','timestamp_CET':'first'})

    # Add weekday column
    # df_hourly["weekday"] = df_hourly['timestamp_CET'].dt.weekday # Monday=0, Sunday=6

    # reorder columns
    df_hourly = df_hourly[["timestamp_CET", "gesamt"]]

    to_date_str = datetime.strptime(to_date, "%Y%m%d").strftime("%Y-%m-%d")
    fname = f"2015-01-01_{to_date_str}_energy"
    df_hourly.to_csv(f"./data/{fname}.csv")

    return df_hourly

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =	

# if __name__ == "__main__":
