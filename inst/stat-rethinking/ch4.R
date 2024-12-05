# Given data

library(tidyverse)
library(skimr)
library(rethinking)

df <- read.csv2("inst/stat-rethinking/Howell1.csv", sep = ";") %>% 
  mutate(
    height = as.numeric(height), 
    weight = as.numeric(weight), 
    age = as.numeric(age)
  )

rethinking::precis(df)

dens(df$height)

curve( dnorm( x , 178 , 20 ) , from=100 , to=250 ) # curve just plots the likelihood 

?curve

curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )

# Every posterior can also be prior for the next analysis. So because of this we can 
# treat priors just like posteriors 

# So in this case, imagine we take both of our two priors: mu and sigma, then draw
# samples from them. These samples are each used to extract a value

sample_mu <- rnorm( 1e4 , 178 , 20 ) # drawing values from the normal distribution (mean of 178, sd of 20)
sample_sigma <- runif( 1e4 , 0 , 50 ) # drawing values from the uniform distribution with lower limit of 0 and upper of 50
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma ) # normal distribution, where the mean depends on each mean and the sigma depends on each sigma
dens( prior_h )

# Gaussian Golem using Grid Method ----

# The goal of this is the estimate the posterior distribution

mu.list <- seq(from = 150, to = 160 , length.out=100 ) # the range of means

sigma.list <- seq( from=7 , to=9 , length.out=100 ) # the range of standard deviations

post <- expand.grid( mu=mu.list , sigma=sigma.list ) # for each mean and standard deviation above, including each
# combination

post %>% view()

ggplot(data = post %>% filter(prob != 0), 
       mapping = aes(x = mu, y = sigma, color = prob)) + 
  geom_point()

post$LL <- sapply( 1:nrow(post) , function(i) sum(
  dnorm( df$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) ) # summing the likelihood 

# the log-likelihood gives a likelihood sum


# This code is to find the posterior distribution by adding the log-likelihood 
# to the log-likelihood for a distribution with a mean of 178 and sd of 20. Also 
# including the uniform distribution for the SD. This is a joint probability 

post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE ) 


post$prob <- exp( post$prod - max(post$prod) )

contour_xyz( post$mu , post$sigma , post$prob )

post$LL

# Sampling Rows from Posterior ----

sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

plot(sample.mu, sample.sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.1))

dens(sample.mu)
dens(sample.sigma)

# Sampling 20 Heights ----

d3 <- sample(df$height, size = 20)

mu.list <- seq(from = 150, to = 170, length.out = 200)
sigma.list <- seq(from = 4, to = 20, length.out = 200)
post2 <- expand.grid(mu = mu.list, sigma = sigma.list)
post2$LL <- sapply(1:nrow(post2), 
                   function(i){ 
                     sum (
                       dnorm(
                         d3, 
                         mean = post2$mu[i], 
                         sd = post2$sigma[i], 
                         log = TRUE)
                     )}
)

post2$prod <- post2$LL + dnorm(post2$mu, 178, 20, TRUE) + dunif(post2$sigma, 0, 50, TRUE)

post2$prob <- exp(post2$prod - max(post2$prod))    

sample2.rows <- sample(1:nrow(post2), size = 1000, replace = TRUE, prob = post2$prob)                   

sample2.mu <- post2$mu[sample2.rows]
sample2.sigma <- post2$sigma[sample2.rows]

plot(sample2.mu, 
     sample2.sigma, 
     cex = 0.5, 
     col = col.alpha(rangi2, 0.1), 
     xlab = "mu", 
     ylab = "sigma", 
     pch = 16,
     )

dens(sample2.sigma, norm.comp = TRUE)

# Quadratic Approximation ----

d <- read.csv2("inst/stat-rethinking/Howell1.csv", sep = ";") %>% 
  mutate(
    height = as.numeric(height), 
    weight = as.numeric(weight), 
    age = as.numeric(age)
  )

d2 <- d[d$age >= 18, ]

flist <- alist(
  height ~ dnorm(mu, sigma), 
  mu ~ dnorm(178, 20), 
  sigma ~ dunif(0, 50)
)

m4.1 <- quap(flist, data = d2)

precis(m4.1)

# We can see each sample, just like the grid approximation from before: 

extract.samples(m4.1, 1000) # each is a mean and a std.dev

precis(post)

plot(d$height ~ d$weight)

# Linear Model ----

N <- 100
a <- rnorm(N, 178, 20)
b <- rnorm(N, 0, 10)

plot(NULL, xlim = range(d2$weight), ylim = c(-100, 400),
     xlab="weight" , ylab="height" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
                        from=min(d2$weight) , to=max(d2$weight) , add=TRUE ,
                        col=col.alpha("black",0.2) )

b <- rlnorm(1e4, 0, 1)
dens(b, xlim = c(0, 5), adj = 0.1)

# Posterior Against the Data ----

plot( height ~ weight , data=d2 , col=rangi2 )
post <- extract.samples( m4.3 )
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*(x - xbar) , add=TRUE )

# Model 4.3 ----

library(MASS)

dl

# Quap is basically fitting a model using quadratic approximation. 
# has a likelihood for height
# equation for the mean mu
# prior for a & b
# prior for sigma 

m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight - xbar),
    a ~ dnorm(178, 20), 
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)


# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq( from=25 , to=70 , by=1 )
# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link( m4.3 , data=data.frame(weight=weight.seq) )
str(mu)

post <- extract.samples(m4.3)

mu_at_50 <- post$a + post$b * (50 - xbar)

dens( mu_at_50 , col=rangi2 , lwd=2 , xlab="mu|weight=50" )

plot( height ~ weight , d2 , type="n" )

# loop over samples and plot each mu value
for ( i in 1:100 )
  points( weight.seq , mu[i,] , pch=16 , col=col.alpha(rangi2,0.1) )

# Splines ----

d <- read.csv2("inst/stat-rethinking/cherry_blossoms.csv", sep = ";") 

# Linear Regression as Joint Probabilites ----

# Basically what Bayesian stats says. 

# the likelihood is based on the observed data (I think)


