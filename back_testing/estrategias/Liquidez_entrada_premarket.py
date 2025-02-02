# -*- coding: utf-8 -*-
"""
Created on Thu Jan 30 19:31:28 2025

@author: bravi
"""
import pandas as pd
import numpy as np

threshold = 0.002
trail = 0.01
time_buy = 3
price_trail = 0

def Liquidez_entrada_premarket(stock):
    stock = pd.DataFrame(stock)
    stock['date'] = pd.to_datetime(stock['date'])
    stock['date_ymd'] = stock['date'].dt.strftime('%Y-%m-%d')
    stock['date_ymd'] = pd.to_datetime(stock['date_ymd'])

    stock['change_day'] = (
        stock['date_ymd'] != stock['date_ymd'].shift()
        ).astype(int)

    stock["change"] = stock["close"].pct_change()
    stock.loc[np.isnan(stock["change"]), "change"] = 0
    
    stock["buy"] = stock.apply(lambda row: when_buy(row, stock), axis=1)
    stock["buy"] = stock["buy"].shift(time_buy)
    stock.loc[np.isnan(stock["buy"]), "buy"] = 0
        
    return stock


def when_buy(row, stock):
    if(row["change_day"] == 1 and row["change"] >= threshold):
        future_index = row.name + time_buy
        if future_index < len(stock):
            return stock.loc[future_index, "close"]
    return 0
        
        
        