
library(rethinking)

data(chimpanzees)

d <- chimpanzees

?chimpanzees

d$treatment <- 1 + d$prosoc_left + 2*d$condition

xtabs( ~ treatment + prosoc_left + condition , d )

m11.1 <- quap(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a ,
    a ~ dnorm( 0 , 10 )
  ) , data=d )

prior <- extract.prior(m11.1)

plot(prior)

# Hamilton Monte Carlo ----

dat_list <- list(
  pulled_left = d$pulled_left,
  actor = d$actor,
  treatment = as.integer(d$treatment) )

library

library(brms)

m11.4 <- ulam(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + b[treatment] ,
    a[actor] ~ dnorm( 0 , 1.5 ),
    b[treatment] ~ dnorm( 0 , 0.5 )
  ) , data=dat_list , chains=4 , log_lik=TRUE )

precis( m11.4 , depth=2 )

# Modelling a variable using both Poisson and Multinomial ----

data(UCBadmit)
d <- UCBadmit

m_binom <- quap(
  alist(
    admit ~ dbinom(applications,p),
    logit(p) <- a,
    a ~ dnorm( 0 , 1.5 )
  ), data=d )
# Poisson model of overall admission rate and rejection rate
# 'reject' is a reserved word in Stan, cannot use as variable name
dat <- list( admit=d$admit , rej=d$reject )
m_pois <- ulam(
  alist(
    admit ~ dpois(lambda1),
    rej ~ dpois(lambda2),
    log(lambda1) <- a1,
    log(lambda2) <- a2,
    c(a1,a2) ~ dnorm(0,1.5)
  ), data=dat , chains=3 , cores=3 )

library(brms)

# Specify the model
library(brms)

# Specify the model
m_pois_brms <- brm(
  formula = 
    bf(admit ~ 1, family = poisson()) + 
    bf(rej ~ 1, family = poisson()),
  data = dat,
  prior = c(
    prior(normal(0, 1.5), class = "Intercept")  # Applying prior for both intercepts
  ),
  chains = 3,
  cores = 3
)

exp(4.99)/ (exp(4.99) + exp(5.44))

inv_logit(coef(m_binom))

k <- coef(m_pois)
a1 <- k['a1']
a2 <- k['a2']
exp(a1)/(exp(a1)+exp(a2))

# Inspect the model output
summary(m_pois_brms)


# Inspect the model output
summary(m_pois_brms)


# Categorical Outcomes ----

library(tidyverse)

ss = 250 # sample size 

test = data.frame(
  y = sample(x = seq(from = 1, to = 7, by = 1), 
       size = ss,
       replace = TRUE), 
  x = rbinom(n = ss, 
             size = 1, 
             prob = 0.5)
)

pr_k <- table(test$y)/nrow(test)

cum_pr_k <- cumsum(pr_k) # cumulative odds 

# plot
plot( 1:7 , cum_pr_k , type="b" , xlab="response" ,
      ylab="cumulative proportion" , ylim=c(0,1) )


logit <- function(x) log(x/(1-x)) # convenience function 
round( lco <- logit( cum_pr_k ) , 2 ) # log cumulative odds 

m12.4q <- quap(
  alist(
    y ~ dordlogit( 0 , c(a1,a2,a3,a4,a5,a6) ),
    c(a1,a2,a3,a4,a5,a6) ~ dnorm( 0 , 1.5 )) , 
  data=test , 
  start=list(a1=-2,a2=-1,a3=0,a4=1,a5=2,a6=2.5) 
)

plot(precis( m12.4q , depth=2 ))

plot(precis(round( inv_logit(coef(m12.4q)) , 3 ))) # these are the cumulative probabilites

plot(precis(inv_logit(coef(m12.4q))))

?brms::brmsfamily()

brms::brm(
  formula = y ~ x, 
  data = test, 
  family = cumulative(link = "logit")
)



