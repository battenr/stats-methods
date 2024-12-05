# Statistical Rethinking - Chapter 5 

library(tidyverse)
library(rethinking)

d <- read.csv("inst/stat-rethinking/WaffleDivorce.csv", sep = ";")

precis(d)

d$A <- scale(d$MedianAgeMarriage)
d$D <- scale(d$Divorce)
d$M <- scale( d$Marriage )

plot(d$A, d$D)
plot(d$MedianAgeMarriage, d$Divorce)


# load data and copy
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
# standardize variables
d$A <- scale( d$MedianAgeMarriage )
d$D <- scale( d$Divorce )

m5.1 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bA * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

dnorm(x = 34, mean = 34, sd = ) # likelihood 

L(distribution | data)
probability(data | distribution)

ls()





m5.1

set.seed(10)
prior <- extract.prior( m5.1 )
mu <- link( m5.1 , post=prior , data=list( A=c(-2,2) ) )
plot( NULL , xlim=c(-2,2) , ylim=c(-2,2) )
for ( i in 1:50 ) lines( c(-2,2) , mu[i,] , col=col.alpha("black",0.4) )


precis(d)


# Code 5.3 ----

m5.3 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) , # probability of the data. We don't know what mu and sigma are 
    mu <- a + bM*M + bA*A , # the linear model we have 
    a ~ dnorm( 0 , 0.2 ) , # prior for the intercept
    bM ~ dnorm( 0 , 0.5 ) , # prior for the M
    bA ~ dnorm( 0 , 0.5 ) , # prior for the A
    sigma ~ dexp( 1 ) # prior for the SD
  ) , data = d )

# Remember from earlier it's

# prior * likelihood = posterior

# so basically

# P(data | distribution)

# P(distribution | data) is the likelihood

m5.4 <- quap(
  alist(
    M ~ dnorm( mu , sigma ) ,
    mu <- a + bAM * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bAM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )


precis( m5.3 )

# 5.2 Masked relationship ----

d <- read.csv("inst/stat-rethinking/milk.csv", sep = ";")

str(d)

d$K <- as.numeric(scale(d$kcal.per.g)) # we standarize these to get a reliable approximation of the poserior
# and to build reasonable priors 

d$N <- scale(d$neocortex.perc)
d$M <- scale(log(d$mass))

m5.5_draft <- quap(
  alist(
    K ~ dnorm(mu, sigma), 
    mu <- a + bN*N, 
    a ~ dnorm(0, 1), 
    bN ~ dnorm(0, 1), 
    sigma ~ dexp(1)
  ),
  data = d
)

dcc = d %>% 
  select(
    K, N, M
  ) %>% na.omit()

str(dcc)


m5.5_draft <- quap(
  alist(
    K ~ dnorm(mu, sigma), 
    mu <- a + bN*N, 
    a ~ dnorm(0, 1), 
    bN ~ dnorm(0, 1), 
    sigma ~ dexp(1)
  ),
  data = dcc
)

# We need to check to see if our priors make sense 

prior <- extract.prior( m5.5_draft )
xseq <- c(-2,2)
mu <- link( m5.5_draft , post=prior , data=list(N=xseq) )
plot( NULL , xlim=xseq , ylim=xseq )
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )



m5.5 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bN*N ,
    a ~ dnorm( 0 , 0.2 ) ,
    bN ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data=dcc )

precis(m5.5)

xseq <- seq( from=min(dcc$N)-0.15 , to=max(dcc$N)+0.15 , length.out=30 )
mu <- link( m5.5 , data=list(N=xseq) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( K ~ N , data=dcc )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

# using log MASS instead 

m5.6 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data=dcc )
precis(m5.6)

# Adding both M and N to the equation

m5.7 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) , # specifying that K is normally distributed 
    mu <- a + bN*N + bM*M , # equation for the mean (the regression equation)
    a ~ dnorm( 0 , 0.2 ) ,  # prior for alpha
    bN ~ dnorm( 0 , 0.5 ) , # prior for beta for N
    bM ~ dnorm( 0 , 0.5 ) , # prior for beta for M 
    sigma ~ dexp( 1 ) # prior for the standard deviation
  ) , data=dcc )
precis(m5.7)


# 5.3 - Categorical Variables ----
 

d <- read.csv("inst/stat-rethinking/Howell1.csv", sep = ";")




















