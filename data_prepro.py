# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =	
# Import libraries
import os
import numpy as np
import pandas as pd
import yfinance as yf
import holidays
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

        # binary dummy var for each month
        dummy_month = pd.get_dummies(df_out['timestamp_CET'].dt.month, prefix='month').astype(int)
        # leave out first month to avoid multicollinearity
        dummy_month = dummy_month.iloc[:, 1:]

        df_out = pd.concat([df_out, dummy_month], axis=1)

    if weekday_method == 'simple':

        # binary dummy var for each weekday
        dummy_weekday = pd.get_dummies(df_out['timestamp_CET'].dt.weekday, prefix='weekday').astype(int)
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
        df_out['is_holiday_newyear_d31'] = ((df_out['timestamp_CET'].dt.month == 12) & (df_out['timestamp_CET'].dt.day == 31))
        df_out['is_holiday_newyear_d31'] = ((df_out['timestamp_CET'].dt.month == 1) & (df_out['timestamp_CET'].dt.day == 1) & (df_out['timestamp_CET'].dt.hour < 6))
        df_out['is_holiday_newyear_d01'] = ((df_out['timestamp_CET'].dt.month == 1) & (df_out['timestamp_CET'].dt.day == 1) & (df_out['timestamp_CET'].dt.hour >= 6))

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
        df_out['is_holiday_threekings'] = df_out['timestamp_CET'].dt.date.isin(threekings_dates)

        # Karfreitag (easter - 2d)
        karfreitag_dates = [k for k, v in holidays_de.items() if v == 'Karfreitag']
        df_out['is_holiday_karfreitag'] = df_out['timestamp_CET'].dt.date.isin(karfreitag_dates)

        # Eastersunday (easter)
        easter_sun_dates = [k for k, v in holidays_de.items() if v == 'Ostersonntag']
        df_out['is_holiday_easter_sunday'] = df_out['timestamp_CET'].dt.date.isin(easter_sun_dates)

        # Eastermonday (easter + 1d)
        easter_mon_dates = [k for k, v in holidays_de.items() if v == 'Ostermontag']
        df_out['is_holiday_easter_monday'] = df_out['timestamp_CET'].dt.date.isin(easter_mon_dates)

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

    return df_out

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =	

# if __name__ == "__main__":
