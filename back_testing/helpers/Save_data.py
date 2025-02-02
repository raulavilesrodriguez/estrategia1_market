# -*- coding: utf-8 -*-
"""
Created on Thu Jan 30 13:15:09 2025

@author: bravi
"""

import os
from datetime import datetime

def Save_data(data, symbol, start_date=None, end_date=None):
    output_dir = "datos"
    
    # Get the date and actual hour
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    
    file_path = os.path.join(output_dir, f"{symbol}_{timestamp}.csv")
    data.to_csv(file_path, index=False)
    print(f"Data saved in {file_path}")