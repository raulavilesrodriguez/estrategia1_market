# -*- coding: utf-8 -*-
"""
Created on Thu Jan 30 11:18:38 2025

@author: bravi
"""

import os
from datetime import datetime
import matplotlib.pyplot as plt
import time

from helpers.Get_stock_data import Get_stock_data
from helpers.Save_data import Save_data
from estrategias.Liquidez_entrada_premarket import Liquidez_entrada_premarket

stock = Get_stock_data("SPY", "5m", "2024-01-01","2024-01-31")

print(stock.head())


data = Liquidez_entrada_premarket(stock)
Save_data(data, "SPY")





