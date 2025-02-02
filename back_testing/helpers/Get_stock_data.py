
from openbb import obb


obb.user.preferences.output_type = "dataframe"

def Get_stock_data(symbol, interval="5m", start_date=None, end_date=None):
    data = obb.equity.price.historical(
        symbol,
        start_date=start_date,
        end_date=end_date,
        interval=interval,
        provider="yfinance",
    )
    data.reset_index(inplace=True)
    return data


