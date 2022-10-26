from datetime import date
from datetime import timedelta
import pandas as pd
import numpy as np
import yfinance as yf
import matplotlib as plt
import torch 


tickers = ['BRZE','AAPL','SBUX','AAL','WMT','AMZN',
           'TMDX','XOM','NFLX','COIN','VTNR','SIGA']
end_date = date.today() - timedelta(days=1)
start_date = end_date - timedelta(days=120)

stock_data = yf.download(tickers, start=start_date, end = end_date)

ret_data = np.log(stock_data/stock_data.shift(1))


rng = np.random.default_rng(1337)
X = np.array(ret_data.T)
w = rng.random(ret_data.shape[1])
w = torch.nn.Softmax(w)

plt.plot(w
plot(w).show
plt.legend
plt.show()

w = DataFrame(w)



import jax
import jax.numpy as jnp
?jax
from jax import grad, jit, vmap
from jax import random
N = stock_data.shape[0]

jax.nn.softmax(ret_data)

def exp_r_and_cov(ret_m):
    r = jnp.mean(ret_m, axis=1)
    cov_m = cov_m = jnp.cov(ret_m)
    return r, cov_m

def ret_and_vol(w, r, cov_m):
    #ensure our weights sum to 1
    w = jax.nn.softmax(w)
    ex_ret = jnp.dot(w.T,r) * N
    ex_vol = jnp.sqrt(jnp.dot(w.T, jnp.dot(cov_m * N,w)))
    return (ex_ret,ex_vol)

def sharpe(ex_ret, ex_vol):
    sr = ex_ret/ex_vol
    return sr

def exp_r_and_cov(ret_m):
    r = jnp.mean(ret_m, axis=1)
    cov_m = cov_m = jnp.cov(ret_m)
    return r, cov_m



ret_data.dropna(inplace=True)
print
ret_data.plot(figsize=(12,10))
plt.show()
show.show

ret_data.head()
