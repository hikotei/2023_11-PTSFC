# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =	
# Import libraries
import numpy as np
import pandas as pd

from tqdm import tqdm
from datetime import datetime, timedelta

import os
import requests
import holidays
import yfinance as yf

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

def get_energy_data_json():

    # get all available time stamps
    stampsurl = "https://www.smard.de/app/chart_data/410/DE/index_quarterhour.json"
    response = requests.get(stampsurl)

    # ignore first 8 years (historic data is in already saved csv)
    timestamps = list(response.json()["timestamps"])[8*52 + 45:]

    col_names = ['timestamp_CET', 'gesamt']
    energydata = pd.DataFrame(columns=col_names)
    
    # loop over all available timestamps
    for stamp in tqdm(timestamps):

        dataurl = "https://www.smard.de/app/chart_data/410/DE/410_DE_quarterhour_" + str(stamp) + ".json"
        response = requests.get(dataurl)
        rawdata = response.json()["series"]

        for i in range(len(rawdata)):
            rawdata[i][0] = datetime.fromtimestamp(int(str(rawdata[i][0])[:10])).strftime("%Y-%m-%d %H:%M:%S")

        if energydata.empty:
            energydata = pd.DataFrame(rawdata, columns=col_names)
        else: 
            energydata = pd.concat([energydata, pd.DataFrame(rawdata, columns=col_names)])

    energydata = energydata.dropna()
    energydata["timestamp_CET"] = pd.to_datetime(energydata.timestamp_CET).dt.tz_localize('CET', ambiguous='infer')
    energydata['timestamp_UTC'] = energydata['timestamp_CET'].dt.tz_convert('UTC')
    
    # set UTC date_time as index
    energydata.set_index("timestamp_UTC", inplace=True)

    return energydata

def get_energy_data_today(to_date=None, recycle=False) :

    # os.chdir("C:/2023_11-PTSFC")
    # os.chdir("../2023_11-PTSFC")
    print(f"> cwd = {os.getcwd()}")

    # if input param to_date is not given, use today's date
    if to_date is None :
        to_date = datetime.today().strftime("%Y%m%d")

    # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    # check if file already exists
        
    # if file DNE then see if a later date exists 
    # which we can also take but just need to cut off the irrelevant tail

    # check all files that start with "2015-01-01_" and end with "_energy.csv"
    # and take the first one with a date later than to_date
    # if no such file exists then proceed download data from SMARD
        
    if recycle :

        to_date_fname = datetime.strptime(to_date, "%Y%m%d").strftime("%Y-%m-%d")
        fname = f"2015-01-01_{to_date_fname}_energy.csv"
        print(f"> checking if {fname} or later already exists ...")

        if os.path.isfile(f"./data/{fname}"):
            print(f"> {fname} already exists, reading from csv ...")
            df = pd.read_csv(f"./data/{fname}")
            return df

        else :
            files = os.listdir("./data")
            files = [f for f in files if f.startswith("2015-01-01_")]
            files = [f for f in files if f.endswith("_energy.csv")]
            files = [f for f in files if f > fname]

            # take last file in list
            if len(files) > 0 :             
                recent_fname = files[-1]
                print(f"using {recent_fname} instead of {fname} !")

                df = pd.read_csv(f"./data/{recent_fname}")
                cutoff = (datetime.strptime(to_date, "%Y%m%d") + timedelta(days=1)).strftime("%Y-%m-%d")
                df = df[df['timestamp_CET'] < cutoff]
                return df

            else :
                print(f"{fname} does not exist, downloading data from SMARD !")
    
    # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    
    # if to_date is earlier than 2023_11_01, use historic data only
    hist_fname = "Realisierter_Stromverbrauch_201501010000_202310312359_Viertelstunde.csv"
    hist_df = pd.read_csv(f"./data/{hist_fname}", sep=";", decimal=",")
    # Rename columns for convenience and remove cols = ['residual', 'pump']
    hist_df.columns = ["datum", "anfang", "ende", "gesamt", "residual", "pump"]
    hist_df.drop(columns=['residual', 'pump'], inplace=True)
    # replace '-' with NA
    hist_df.replace('-', pd.NA, inplace=True)

    # Merge date and time column and set that as the index
    hist_df["timestamp"] = pd.to_datetime(hist_df['datum'] + ' ' + hist_df['anfang'], format='%d.%m.%Y %H:%M')
    # change datetime to utc time
    hist_df['timestamp_CET'] = hist_df['timestamp'].dt.tz_localize('CET', ambiguous='infer')
    hist_df['timestamp_UTC'] = hist_df['timestamp_CET'].dt.tz_convert('UTC')
    hist_df.set_index("timestamp_UTC", inplace=True)
    # make index datetime object
    hist_df.index = pd.to_datetime(hist_df.index)
    # drop redundant columns
    hist_df = hist_df[['timestamp_CET', 'gesamt']].copy()

    # Replace "." with "" and then replace "," with "."
    hist_df[['gesamt']] = hist_df[['gesamt']].apply(lambda x: x.str.replace('.', '', regex=False))
    hist_df[['gesamt']] = hist_df[['gesamt']].apply(lambda x: x.str.replace(',', '.', regex=False))
    hist_df[['gesamt']] = hist_df[['gesamt']].apply(pd.to_numeric)

    if to_date > "20231101" :
        print(f"> to_date is later than 2023-11-01, using recent data as well !")
        recent_df = get_energy_data_json()
        # get last index of hist_df
        last_index = hist_df.index[-1]
        # drop rows of recent_df that are already in hist_df
        recent_df = recent_df.loc[last_index + timedelta(minutes=15):]
        # merge 2 dataframes
        df_utc = pd.concat([hist_df, recent_df])

    else :
        print(f"> to_date is earlier than 2023-11-01, using historic data only !")
        df_utc = hist_df.copy()

    # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    # cutoff date
    cutoff = (datetime.strptime(to_date, "%Y%m%d") + timedelta(days=1)).strftime("%Y-%m-%d")
    df_utc = df_utc[df_utc['timestamp_CET'] < cutoff]

    # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    # if there are consecutive trailing NaNs, drop them ... find last valid index
    last_valid_index = df_utc['gesamt'].last_valid_index()

    if last_valid_index is not None:
        df_utc = df_utc.loc[:last_valid_index]

    print(f"> {df_utc['gesamt'].isna().sum()} NA in df")
    print(f"> last valid index = {last_valid_index}")

    # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    # Interpolate missing values in between
    df_utc_interp = df_utc.copy()
    df_utc_interp['gesamt'].interpolate(method='time', inplace=True)

    # resample at hourly level
    df_hourly = df_utc_interp.resample("1h", label="left").agg({'gesamt':'sum','timestamp_CET':'first'})

    # reorder columns
    df_hourly = df_hourly[["timestamp_CET", "gesamt"]]

    # save to csv
    to_date_str = last_valid_index.strftime("%Y-%m-%d")
    fname = f"2015-01-01_{to_date_str}_energy"
    df_hourly.to_csv(f"./data/{fname}.csv")
    print(f"> done and saved to {fname}.csv")

    return df_hourly

def create_features_df(df, holiday_method='simple', lags=None):

    df_out = df.copy()

    # - - - - - - - - - - - - - - - - - - - - - -
    # basic features 
    # - - - - - - - - - - - - - - - - - - - - - -

    # add hour
    df_out["hour"] = df_out['timestamp_CET'].dt.hour
    # add weekday
    df_out["weekday"] = df_out['timestamp_CET'].dt.weekday
    # add month
    df_out["month"] = df_out['timestamp_CET'].dt.month
    # add weeknum
    # df_out["weeknum"] = df_out['timestamp_CET'].dt.isocalendar().week

    # - - - - - - - - - - - - - - - - - - - - - -
    # holidays
    # - - - - - - - - - - - - - - - - - - - - - -

    # get all years in dataframe
    uniq_yrs = df_out['timestamp_CET'].dt.year.unique()
    # print(f"unique years in df: {uniq_yrs}")
    
    # get holidays for germany for all states and combine them into one single dict
    states = ['BB', 'BE', 'BW', 'BY', 'BYP', 'HB', 'HE', 'HH', 'MV', 
              'NI', 'NW', 'RP', 'SH', 'SL', 'SN', 'ST', 'TH']
    
    holidays_de = holidays.CountryHoliday('DE', years=uniq_yrs)
    for state in states:
        holidays_de.update(holidays.CountryHoliday('DE', state=state, years=uniq_yrs))

    # sort holidays
    holidays_de = dict(sorted(holidays_de.items()))
    holidays_de_dates = list(holidays_de.keys())

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    # only one holiday dummy for all holidays
    if holiday_method == 'simple':

        df_out['is_holiday'] = df_out['timestamp_CET'].dt.date.isin(holidays_de_dates).astype(int)

    # create separate dummies for each holiday ...
    if holiday_method == 'separate' :

        # newyears + silvester ist kein feiertag aber die meisten nehmen trotzdem frei
        # create dummy variable for all rows where timestamp_CET is 12.31 or 01.01
        df_out['is_holiday_newyear_d31'] = ((df_out['timestamp_CET'].dt.month == 12) & (df_out['timestamp_CET'].dt.day == 31))
        df_out['is_holiday_newyear_d01'] = ((df_out['timestamp_CET'].dt.month == 1) & (df_out['timestamp_CET'].dt.day == 1))

        # Heilige Drei Könige (01.06)
        threekings_dates = [k for k, v in holidays_de.items() if v == 'Heilige Drei Könige']
        df_out['is_holiday_threekings'] = df_out['timestamp_CET'].dt.date.isin(threekings_dates)

        # Karfreitag (easter - 2d)
        karfreitag_dates = [k for k, v in holidays_de.items() if v == 'Karfreitag']
        df_out['is_holiday_karfreitag'] = df_out['timestamp_CET'].dt.date.isin(karfreitag_dates)

        # Eastermonday (easter + 1d)
        easter_dates = [k for k, v in holidays_de.items() if v == 'Ostermontag']
        df_out['is_holiday_easter'] = df_out['timestamp_CET'].dt.date.isin(easter_dates)

        # Erster Mai / Tag der Arbeit (05.01)
        erstermai_dates = [k for k, v in holidays_de.items() if v == 'Erster Mai']
        df_out['is_holiday_erstermai'] = df_out['timestamp_CET'].dt.date.isin(erstermai_dates)

        # Christi Himmelfahrt (easter + 39d)
        himmelfahrt_dates = [k for k, v in holidays_de.items() if v == 'Christi Himmelfahrt']
        df_out['is_holiday_himmelfahrt'] = df_out['timestamp_CET'].dt.date.isin(himmelfahrt_dates)

        # Pfingstmontag (easter + 50d)
        pfingstmontag_dates = [k for k, v in holidays_de.items() if v == 'Pfingstmontag']
        df_out['is_holiday_pfingstmontag'] = df_out['timestamp_CET'].dt.date.isin(pfingstmontag_dates)

        # Fronleichnam (easter + 60d)
        fronleichnam_dates = [k for k, v in holidays_de.items() if v == 'Fronleichnam']
        df_out['is_holiday_fronleichnam'] = df_out['timestamp_CET'].dt.date.isin(fronleichnam_dates)

        # Maria Himmelfahrt (08.15)
        mariahimmelfahrt_dates = [k for k, v in holidays_de.items() if v == 'Mariä Himmelfahrt']
        df_out['is_holiday_mariahimmelfahrt'] = df_out['timestamp_CET'].dt.date.isin(mariahimmelfahrt_dates)

        # Tag der Deutschen Einheit (10.03)
        einheit_dates = [k for k, v in holidays_de.items() if v == 'Tag der Deutschen Einheit']
        df_out['is_holiday_einheit'] = df_out['timestamp_CET'].dt.date.isin(einheit_dates)

        # Reformationstag (10.31)
        reformationstag_dates = [k for k, v in holidays_de.items() if v == 'Reformationstag']
        df_out['is_holiday_reformationstag'] = df_out['timestamp_CET'].dt.date.isin(reformationstag_dates)

        # Allerheiligen (11.01)
        allerheiligen_dates = [k for k, v in holidays_de.items() if v == 'Allerheiligen']
        df_out['is_holiday_allerheiligen'] = df_out['timestamp_CET'].dt.date.isin(allerheiligen_dates)

        # christmas = list of datetimes from 12.24 to 12.26 
        df_out['is_holiday_xmas_d23'] = ((df_out['timestamp_CET'].dt.month == 12) & (df_out['timestamp_CET'].dt.day == 23))
        df_out['is_holiday_xmas_d24'] = ((df_out['timestamp_CET'].dt.month == 12) & (df_out['timestamp_CET'].dt.day == 24))
        df_out['is_holiday_xmas_d25'] = ((df_out['timestamp_CET'].dt.month == 12) & (df_out['timestamp_CET'].dt.day == 25))
        df_out['is_holiday_xmas_d26'] = ((df_out['timestamp_CET'].dt.month == 12) & (df_out['timestamp_CET'].dt.day == 26))

        # brückentage zwischen weihnachten und neujahr
        # 12.27, 12.28, 12.29, 12.30
        df_out['is_holiday_xmas2newyear'] = ((df_out['timestamp_CET'].dt.month == 12) & (df_out['timestamp_CET'].dt.day.isin([27,28,29,30])))

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        # reformat all columns that begin with "is_holiday" to int
        for col in df_out.columns:
            if col.startswith('is_holiday'):
                df_out[col] = df_out[col].astype(int)

    # - - - - - - - - - - - - - - - - - - - - - -
    # lags
    # - - - - - - - - - - - - - - - - - - - - - -

    # only if column 'gesamt' exists
    if 'gesamt' in df_out.columns:
        # add lagged versions of column 'gesamt' based on input lags list of lagged values
        if lags is not None:
            for lag in lags:
                df_out[f"lag_{lag}"] = df_out["gesamt"].shift(lag)

            # take biggest value in lags and remove first rows in df_out to get rid of NaNs
            max_lag = max(lags)
            df_out = df_out[max_lag:]

    # - - - - - - - - - - - - - - - - - - - - - -
    # df_out.drop(columns=["timestamp_CET"], inplace=True)

    return df_out

def create_dummy_df(df, month_method='simple', weekday_method='simple', hour_method='simple', holiday_method='simple'):

    df_out = df.copy()

    if month_method == 'simple':
        
        # https://stackoverflow.com/a/37426982/15816035
        # - - - - - - - - - - - - - - - - - - - - - - -
        # cats = ['a', 'b', 'c']
        # df = pd.DataFrame({'cat': ['a', 'b', 'a']})

        # dummies = pd.get_dummies(df, prefix='', prefix_sep='')
        # dummies = dummies.T.reindex(cats).T.fillna(0)

        cats = ['month_1', 'month_2', 'month_3', 'month_4', 'month_5', 'month_6', 'month_7', 'month_8', 'month_9', 'month_10', 'month_11', 'month_12']

        # binary dummy var for each month
        dummy_month = pd.get_dummies(df_out['timestamp_CET'].dt.month, prefix='month').astype(int)
        dummy_month = dummy_month.T.reindex(cats).T.fillna(0)
        # leave out first month to avoid multicollinearity
        dummy_month = dummy_month.iloc[:, 1:]
        # add values dummy_month to df_temp
        df_out = pd.concat([df_out, dummy_month], axis=1)

    if weekday_method == 'simple':

        cats = ['weekday_0', 'weekday_1', 'weekday_2', 'weekday_3', 'weekday_4', 'weekday_5', 'weekday_6']

        # binary dummy var for each weekday
        dummy_weekday = pd.get_dummies(df_out['timestamp_CET'].dt.weekday, prefix='weekday').astype(int)
        dummy_weekday = dummy_weekday.T.reindex(cats).T.fillna(0)
        # leave out first weekday to avoid multicollinearity
        dummy_weekday = dummy_weekday.iloc[:, 1:]

        df_out = pd.concat([df_out, dummy_weekday], axis=1)

    # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    # create hour dummies

    if hour_method == 'simple':

        # binary dummy var for each hour
        dummy_hour = pd.get_dummies(df_out['timestamp_CET'].dt.hour, prefix='hour').astype(int)
        # leave out first hour to avoid multicollinearity
        dummy_hour = dummy_hour.iloc[:, 1:]
        df_out = pd.concat([df_out, dummy_hour], axis=1)

    if hour_method == 'seasonal':

        # separate hourly dummy vars for summer and winter months
        summer_months = [4, 5, 6, 7, 8, 9]
        df_out['is_summer'] = df_out['timestamp_CET'].dt.month.isin(summer_months).astype(int)

        # create dummy variables for each hour in summer months
        for hr in range(1, 24):
            # skip hour 0 in summer to avoid multicollinearity
            df_out[f'hour_{hr}_summer'] = ((df_out['is_summer'] == 1) & (df_out['timestamp_CET'].dt.hour == hr)).astype(int)

        for hr in range(1, 24):
            # skip hour 0 in winter to avoid multicollinearity
            df_out[f'hour_{hr}_winter'] = ((df_out['is_summer'] == 0) & (df_out['timestamp_CET'].dt.hour == hr)).astype(int)

        # drop is_summer
        df_out.drop(columns=['is_summer'], inplace=True)

    if hour_method == 'monthly':

        # separate hourly dummy vars for EVERY month
        month_dict = {1: 'January', 2: 'February', 3: 'March', 4: 'April', 5: 'May',
                    6: 'June', 7: 'July', 8: 'August', 9: 'September', 10: 'October',
                    11: 'November', 12: 'December'}

        for m_idx in range(1, 13):
            
            m_name = month_dict[m_idx][:3].lower() # short version of month name

            for hr in range(1, 24):
                # skip hour 0 to avoid multicollinearity
                df_out[f'hour_{hr}_{m_name}'] = ((df_out['timestamp_CET'].dt.month == m_idx) & 
                                                 (df_out['timestamp_CET'].dt.hour == hr)).astype(int)

    # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    # = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    # create holiday dummies

    # get all years in dataframe
    uniq_yrs = df_out['timestamp_CET'].dt.year.unique()

    # get holidays for germany for all states and combine them into one single dict
    states = ['BB', 'BE', 'BW', 'BY', 'BYP', 'HB', 'HE', 'HH', 'MV', 
              'NI', 'NW', 'RP', 'SH', 'SL', 'SN', 'ST', 'TH']
    
    holidays_de = holidays.CountryHoliday('DE', years=uniq_yrs)
    for state in states:
        holidays_de.update(holidays.CountryHoliday('DE', state=state, years=uniq_yrs))

    # sort holidays
    holidays_de = dict(sorted(holidays_de.items()))
    holidays_de_dates = list(holidays_de.keys())

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    # only one holiday dummy for all holidays
    if holiday_method == 'simple':

        df_out['is_holiday'] = df_out['timestamp_CET'].dt.date.isin(holidays_de_dates).astype(int)

    # create separate dummies for each holiday ...
    if holiday_method == 'separate' :

        # newyears + silvester ist kein feiertag aber die meisten nehmen trotzdem frei
        # create dummy variable for all rows where timestamp_CET is 12.31 or 01.01
        df_out['is_holiday_newyear_d31'] = (((df_out['timestamp_CET'].dt.month == 12) & (df_out['timestamp_CET'].dt.day == 31)) | ((df_out['timestamp_CET'].dt.month == 1) & (df_out['timestamp_CET'].dt.day == 1) & (df_out['timestamp_CET'].dt.hour <= 6)))
        df_out['is_holiday_newyear_d01'] = ((df_out['timestamp_CET'].dt.month == 1) & (df_out['timestamp_CET'].dt.day == 1) & (df_out['timestamp_CET'].dt.hour > 6))

        # # List of holiday names
        # holiday_names = [
        #     'Heilige Drei Könige', 'Karfreitag', 'Ostersonntag', 'Ostermontag',
        #     'Erster Mai', 'Christi Himmelfahrt', 'Pfingstmontag', 'Fronleichnam',
        #     'Mariä Himmelfahrt', 'Tag der Deutschen Einheit', 'Reformationstag',
        #     'Allerheiligen'
        # ]

        # # Iterate over holiday names
        # for holiday_name in holiday_names:
        #     # Get corresponding dates for the holiday
        #     holiday_dates = [k for k, v in holidays_de.items() if v == holiday_name]
            
        #     # Create a column for each holiday
        #     column_name = 'is_holiday_' + holiday_name.lower().replace(' ', '')
        #     df_out[column_name] = df_out['timestamp_CET'].dt.date.isin(holiday_dates)

        # Heilige Drei Könige (01.06)
        threekings_dates = [k for k, v in holidays_de.items() if v == 'Heilige Drei Könige']
        df_out['is_holiday_threekings'] = (df_out['timestamp_CET'].dt.date.isin(threekings_dates) & (df_out['timestamp_CET'].dt.hour > 3))

        # Karfreitag (easter - 2d)
        karfreitag_dates = [k for k, v in holidays_de.items() if v == 'Karfreitag']
        df_out['is_holiday_karfreitag'] = (df_out['timestamp_CET'].dt.date.isin(karfreitag_dates) & (df_out['timestamp_CET'].dt.hour > 6))
        # also add the hours till 5am of the following day

        # Eastersunday (easter)
        easter_sun_dates = [k for k, v in holidays_de.items() if v == 'Ostersonntag']
        df_out['is_holiday_easter_sunday'] = df_out['timestamp_CET'].dt.date.isin(easter_sun_dates)

        # Eastermonday (easter + 1d)
        easter_mon_dates = [k for k, v in holidays_de.items() if v == 'Ostermontag']
        df_out['is_holiday_easter_monday'] = (df_out['timestamp_CET'].dt.date.isin(easter_mon_dates) & (df_out['timestamp_CET'].dt.hour > 6))

        # Erster Mai / Tag der Arbeit (05.01)
        erstermai_dates = [k for k, v in holidays_de.items() if v == 'Erster Mai']
        df_out['is_holiday_erstermai'] = (df_out['timestamp_CET'].dt.date.isin(erstermai_dates) & (df_out['timestamp_CET'].dt.hour > 6))

        # Christi Himmelfahrt (easter + 39d)
        himmelfahrt_dates = [k for k, v in holidays_de.items() if v == 'Christi Himmelfahrt']
        df_out['is_holiday_himmelfahrt'] = (df_out['timestamp_CET'].dt.date.isin(himmelfahrt_dates) & (df_out['timestamp_CET'].dt.hour > 6))

        # Pfingstmontag (easter + 50d)
        pfingstmontag_dates = [k for k, v in holidays_de.items() if v == 'Pfingstmontag']
        df_out['is_holiday_pfingstmontag'] = (df_out['timestamp_CET'].dt.date.isin(pfingstmontag_dates) & (df_out['timestamp_CET'].dt.hour > 3))

        # Fronleichnam (easter + 60d)
        fronleichnam_dates = [k for k, v in holidays_de.items() if v == 'Fronleichnam']
        df_out['is_holiday_fronleichnam'] = (df_out['timestamp_CET'].dt.date.isin(fronleichnam_dates) & (df_out['timestamp_CET'].dt.hour > 3))

        # Maria Himmelfahrt (08.15)
        mariahimmelfahrt_dates = [k for k, v in holidays_de.items() if v == 'Mariä Himmelfahrt']
        df_out['is_holiday_mariahimmelfahrt'] = (df_out['timestamp_CET'].dt.date.isin(mariahimmelfahrt_dates) & (df_out['timestamp_CET'].dt.hour > 2))

        # Tag der Deutschen Einheit (10.03)
        einheit_dates = [k for k, v in holidays_de.items() if v == 'Tag der Deutschen Einheit']
        df_out['is_holiday_einheit'] = (df_out['timestamp_CET'].dt.date.isin(einheit_dates) & (df_out['timestamp_CET'].dt.hour > 3))

        # Reformationstag (10.31)
        reformationstag_dates = [k for k, v in holidays_de.items() if v == 'Reformationstag']
        df_out['is_holiday_reformationstag'] = (df_out['timestamp_CET'].dt.date.isin(reformationstag_dates)  & (df_out['timestamp_CET'].dt.hour > 2))

        # Allerheiligen (11.01)
        allerheiligen_dates = [k for k, v in holidays_de.items() if v == 'Allerheiligen']
        df_out['is_holiday_allerheiligen'] = df_out['timestamp_CET'].dt.date.isin(allerheiligen_dates)

        # christmas = list of datetimes from 12.24 to 12.26 
        df_out['is_holiday_xmas_d23'] = ((df_out['timestamp_CET'].dt.month == 12) & (df_out['timestamp_CET'].dt.day == 23) & (df_out['timestamp_CET'].dt.hour > 3))
        df_out['is_holiday_xmas_d24'] = ((df_out['timestamp_CET'].dt.month == 12) & (df_out['timestamp_CET'].dt.day == 24))
        df_out['is_holiday_xmas_d25'] = (((df_out['timestamp_CET'].dt.month == 12) & (df_out['timestamp_CET'].dt.day == 25)) | ((df_out['timestamp_CET'].dt.month == 12) & (df_out['timestamp_CET'].dt.day == 26) & (df_out['timestamp_CET'].dt.hour < 6)))
        df_out['is_holiday_xmas_d26'] = ((df_out['timestamp_CET'].dt.month == 12) & (df_out['timestamp_CET'].dt.day == 26) & (df_out['timestamp_CET'].dt.hour >= 6))

        # brückentage zwischen weihnachten und neujahr
        # 12.27, 12.28, 12.29, 12.30
        df_out['is_holiday_xmas2newyear'] = ((df_out['timestamp_CET'].dt.month == 12) & (df_out['timestamp_CET'].dt.day.isin([27,28,29,30])))

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        # reformat all columns that begin with "is_holiday" to int
        for col in df_out.columns:
            if col.startswith('is_holiday'):
                df_out[col] = df_out[col].astype(int)

    return df_out

def fix_quantile_crossing(df):

    df_out = df.copy()
    for index, row in df.iterrows():

        # check if quantiles are in ascending order
        if not all(row.diff().dropna() > 0):
            # print(f'> ERROR: Quantiles are not in ascending order for {index}')
            # print(row)
            # sort columns 
            df_out.loc[index] = row.sort_values().values
            # print(df_ensemble_pred.loc[index])

    return df_out

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =	

# if __name__ == "__main__":
