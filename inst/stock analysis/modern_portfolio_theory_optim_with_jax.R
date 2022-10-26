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

log_return <- function(var){
  return = var/lag(var)
  log_of_return = log(return)
  return(log_of_return)
}

log_to_price <- function(var, original_price = 1.0){
  
  # variable is the log of the return
  
  price_change = exp(cumsum(replace_na(var, 0)))
  price = original_price*price_change
  
  return(price)
  
}

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

library(MASS)

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
    aapl_log_return = log_return(AAPL),
    amzn_log_return = log_return(AMZN)
    # aapl_new_price = log_to_price(AAPL, original_price = 172.14),
    # amzn_new_price = log_to_price(AMZN, original_price = 157.7845)
  ) %>% 
  tibble::rownames_to_column("date")

df.for.mean = df.log %>% 
  dplyr::filter(
    name == "AMZN" | name == "AAPL"
  ) 

df.for.mean$log_return

mu.aapl =  mean(df.appl.amzn$aapl_log_return, na.rm = TRUE)
mu.amzn = mean(df.appl.amzn$amzn_log_return, na.rm = TRUE)
mu.both = mean(df.for.mean$log_return, na.rm = TRUE)
sigma.appl.amzn = cov(df.appl.amzn %>% dplyr::select(aapl_log_return, amzn_log_return) %>% drop_na())
sigma.appl.amzn

sigma = matrix(c(0.0005810042, 0.0006949783,
                 0.0006949783, 0.0013516147),
               2,
               2)

mu = matrix(c(mu.aapl, mu.amzn))

df.predict.mn <- data.frame(
  log_return = MASS::mvrnorm(n = 84, mu = mu, Sigma = sigma),
  date = df.aal$date
) %>% 
  dplyr::mutate(
    aapl_price = log_to_price(log_return.1, original_price = 172.14),
    amzn_price = log_to_price(log_return.2, original_price = 157.7845)
  )

ggplot(data = df.predict.mn, mapping = aes(x = date)) +
  geom_line(aes(y = aapl_price), color = "orange", group = "AAPL") + 
  geom_line(aes(y = amzn_price), color = "blue")


library(MASS)

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

# Modern Portfolio Theory ----

# Following this link https://www.codingfinance.com/post/2018-05-31-portfolio-opt-in-r/#:~:text=Portfolio%20optimization%20is%20an%20important,need%20to%20optimize%20the%20portfolios.

library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tidyverse)

#... Data ----

tick <- c('AMZN', 'AAPL', 'NFLX', 'XOM', 'T')

price_data <- tq_get(tick,
                     from = '2014-01-01',
                     to = '2018-05-31',
                     get = 'stock.prices')

#... Daily Returns ----

log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

#... Making Wide Format and XTS format ----

log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()

head(log_ret_xts)

#... Mean Return ----

mean_ret <- colMeans(log_ret_xts)
print(round(mean_ret, 5))

#... Covariance ----

cov_mat <- cov(log_ret_xts) * 252

print(round(cov_mat,4))

?optim

fr <- function(x) {   ## Rosenbrock Banana function
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}

optim(price_data$close, fr)

#... Portfolio Returns and Risk ----

# Calculate the random weights (dividing by sum ensures they add up to 1)
wts <- runif(n = length(tick))
wts <- wts/sum(wts)

# Calculate the portfolio returns
port_returns <- (sum(wts * mean_ret) + 1)^252 - 1

# Calculate the portfolio risk
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))

# Calculate the Sharpe Ratio
sharpe_ratio <- port_returns/port_risk

print(wts)

#... Simulating 5000 random portfolios ----

num_port <- 5000

# Creating a matrix to store the weights

all_wts <- matrix(nrow = num_port,
                  ncol = length(tick))

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port)

for (i in seq_along(port_returns)) {
  
  wts <- runif(length(tick))
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  
  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
}

# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)


# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(log_ret_xts)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

head(portfolio_values)

min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]

#... Plotting with Lowest Variance ----

p <- min_var %>%
  gather(AAPL:XOM, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

#... Plotting Portfolio with Highest Sharpe Ratio ----

p <- max_sr %>%
  gather(AAPL:XOM, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red') +
  annotate('text', x = 0.20, y = 0.42, label = "Tangency Portfolio") +
  annotate('text', x = 0.18, y = 0.01, label = "Minimum variance portfolio") +
  annotate(geom = 'segment', x = 0.14, xend = 0.135,  y = 0.01, 
           yend = 0.06, color = 'red', arrow = arrow(type = "open")) +
  annotate(geom = 'segment', x = 0.22, xend = 0.2275,  y = 0.405, 
           yend = 0.365, color = 'red', arrow = arrow(type = "open"))


ggplotly(p)