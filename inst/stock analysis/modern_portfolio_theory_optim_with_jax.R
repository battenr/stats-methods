# Title: Analyzing Stocks using Modern Portfolio
# link: https://www.countbayesie.com/blog/2022/8/7/modern-portfolio-theory-and-optimization-with-jax

# Description: 

# Setup ----

#... Libraries ----

library(quantmod)
library(tidyverse)

#... Dependencies ----

# Data ----

# Specify ticker of stocks wanted

tickers = c('BRZE','AAPL','SBUX','AAL','WMT','AMZN',
           'TMDX','XOM','NFLX','COIN','VTNR','SIGA')

sdate <- as.Date("2022-04-07") # start date
edate <- as.Date("2022-08-09") # end date

quantmod::getSymbols(tickers, from = sdate, to = edate) # getting data

#... Formatting to R ----

dat = data.frame(
   BRZE , AAPL , SBUX , AAL , WMT , AMZN ,
   TMDX , XOM , NFLX , COIN , VTNR , SIGA 
) %>% 
  dplyr::select(
    ends_with(".Close")
  ) %>% 
  rename_at(
    .vars = vars(ends_with(".Close")), 
    .funs = funs(sub("[.]Close$", "", .))
  ) %>% 
  tibble::rownames_to_column("date") %>% 
  dplyr::mutate(
    date = as.Date(date)
  ) %>% 
  tidyr::pivot_longer(
    cols = !c(date),
    values_to = "price" 
  )

# Plotting Data ----

dat.appl <- dat %>% dplyr::filter(name == "AAPL")

ggplot(dat, mapping = aes(x = date, y = price, color = name)) +
  geom_line()

# Log-Transform Data ----

# Transform data to work with better

# Using log return helps ensure the returns are symmetric in magnitude

df.log = dat %>% 
  dplyr::group_by(name) %>% 
  dplyr::arrange(
    date
  ) %>% 
  dplyr::mutate(
    return = price/lag(price),
    log_return = log(return)
  ) %>% 
  dplyr::ungroup()

ggplot(df.log, 
       mapping = aes(x = date, y = log_return, color = name)) +
  geom_line()

# Modelling one stock (AAL) ----

#... Histogram ----

df.aal = df.log %>% dplyr::filter(name == "AAL")

ggplot(data = df.aal, mapping = aes(x = log_return)) + 
  geom_histogram(fill = "blue") + 
  ylim(0, 10) +
  geom_density(color = "black")

#... Mean (SD) ----

mu.aal = mean(df.aal$log_return, na.rm = TRUE)
sigma.aal = sd(df.aal$log_return, na.rm = TRUE)

#... Sample from normal distribution ----

df.aal.sim = data.frame(
  price = rnorm(84, mean = mu.aal, sigma.aal),
  date = df.aal$date
)

ggplot(df.aal.sim, mapping = aes(x = date, y = price)) + 
  geom_line()

#... Simulating Actual Stock Price using the Normal Log Return Model ----

df.aal.cumsum = df.aal %>% 
  dplyr::filter(date != sdate) %>% 
  dplyr::mutate(
    logcumsum = cumsum(log_return),
    alt_aal_change = exp(logcumsum),
    alt_aal = 16.95*alt_aal_change # 16.95 is original price for AAL stock
  )

ggplot(df.aal.cumsum, mapping = aes(x = date, y = logcumsum)) + 
  geom_line()

ggplot(df.aal.cumsum, mapping = aes(x = date, y = alt_aal_change)) + 
  geom_line()

ggplot(df.aal.cumsum, mapping = aes(x = date, y = alt_aal)) + 
  geom_line()


# Since SD vs mean is large we'd expect there to be a lot of variety

mu.aal/sigma.aal

# In Finance: mean of prices is expected return, SD is the volatility
# Ratio of expected return to volatility is the Sharpe Ratio

# Making Function for Calculation Log of Return ----

log_return <- function(){}

log_to_price <- function(logeturn)

# Modelling Multiple Stocks ----

library(GGally)

#... Correlation Plot ----

dat.corr <- data.frame(
  BRZE , AAPL , SBUX , AAL , WMT , AMZN ,
  TMDX , XOM , NFLX , COIN , VTNR , SIGA 
) %>% 
  dplyr::select(
    ends_with(".Close")
  ) %>% 
  rename_at(
    .vars = vars(ends_with(".Close")), 
    .funs = funs(sub("[.]Close$", "", .))
  ) 

ggpairs(dat.corr)

#... Plotting AMZN vs AAPL ----

ggplot(dat %>% dplyr::filter(name == "AAPL" | name == "AMZN"), mapping = aes(x = date, y = price, color = name)) +
  geom_line()

# our simulation won't work because we need to account for fact they are correlated

# instead: 

#... Multivariate Normal ----

df.appl.amzn = data.frame(
  BRZE , AAPL , SBUX , AAL , WMT , AMZN ,
  TMDX , XOM , NFLX , COIN , VTNR , SIGA 
) %>% 
  dplyr::select(
    ends_with(".Close")
  ) %>% 
  rename_at(
    .vars = vars(ends_with(".Close")), 
    .funs = funs(sub("[.]Close$", "", .))
  ) %>% 
  dplyr::select(AAPL, AMZN) %>% 
  dplyr::mutate(
    aapl_return = price/lag(price),
    aappl_log_return = log(return),
    log_aapl = log(AAPL), 
    log_amzn = log(AMZN),
    aapl_log_cumsum = cumsum(log_aapl),
    amzn_log_cumsum = cumsum(log_amzn)
  ) %>% 
  tibble::rownames_to_column("date")
  

df.for.mean <- dat %>% dplyr::filter(name == "AAPL" | name == "AMZN") 

mu.aapl.amzn = mean(df.for.mean$price)
sigma.appl.amzn = sqrt(diag(cov(df.appl.amzn %>% dplyr::select(log_aapl, log_amzn))))
sigma.appl.amzn

mean(df.appl.amzn$log_aapl)
mean(df.appl.amzn$log_amzn)

df.predict.mn <- data.frame(
  appl_price = rmultinom(),
  amzn_price = rmulitnom(),
  date = df.aal$date
)

diag(sqrt(cov(df.appl.amzn)))
cov(df.appl.amzn)
sd(df.for.mean$price)

mean(df.appl.amzn)
cov(df.appl.amzn)

#... Plotting ----

ggplot(df.appl.amzn, mapping = aes(x = as.Date(date))) + 
  geom_line(aes(y = log_aapl))

ggplot(df.aal.cumsum, mapping = aes(x = date, y = logcumsum)) + 
  geom_line()
