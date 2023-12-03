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
data from 2015-01-01 to 2023-11-30 is already saved in .csv file
only need to update data from last available date to today

add weekday information
add weekend and holiday indicator variables (maybe separate important holidays eg newyear & christmas)
add temperature and weather data (population weighted?)

"""

if __name__ == "__main__":

    print(f"cwd = {os.getcwd()}")

    if (not os.path.isfile("./data/2000-01-01_2023-11-01_dax_prices.csv")) :
        print("Downloading historic DAX data ... from 2000-01-01 to 2023-11-01")
        get_dax_data()

    start = "2023-11-01"
    today = datetime.today().strftime("%Y-%m-%d")
    get_dax_data(start, today)

    # get returns from combined historirc and recent data
    returns_df = get_dax_returns("2000-01-01_2023-11-01_dax_prices", f"{start}_{today}_dax_prices")

    # save returns to csv file
    fname = f"2000-01-01_{today}_dax_returns"
    returns_df.to_csv(f"./data/{fname}.csv")