
# Some background for the code (Ch 2 of Statistical Rethinking)

# We have a globe. Sometimes the water is upright, sometimes the land is upright. 
# We will make the assumption that for our prior, it's just flat 
# (a straight line)

# First we start with a grid. This is all the values we think the parameter could be
# Let's say we think it's somewhere between 0 and 1. This is beta1

p_grid <- seq(from = 0, to = 1, length.out = 20)


# define prior
prior <- rep( 1 , 20 ) # assume only globe lands on water

# compute likelihood at each value in grid

likelihood <- dbinom( 6 , size=9 , prob=p_grid )

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot(posterior)


