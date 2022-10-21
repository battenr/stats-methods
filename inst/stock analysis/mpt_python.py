from datetime import date
from datetime import timedelta
import numpy as np
import yfinance as yf
import matplotlib as plot


tickers = ['BRZE','AAPL','SBUX','AAL','WMT','AMZN',
           'TMDX','XOM','NFLX','COIN','VTNR','SIGA']
end_date = date.today() - timedelta(days=1)
start_date = end_date - timedelta(days=120)

stock_data = yf.download(tickers, start=start_date, end = end_date)

ret_data = np.log(stock_data/stock_data.shift(1))

ret_data.dropna(inplace=True)
print
ret_data.plot(figsize=(12,10))
plt.show()
show.show

ret_data.head()
