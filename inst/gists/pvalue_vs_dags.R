# Title: Variable Selection Based on P-Values vs DAGs 

# Description: The purpose of this code is to show how variable selection 
# based on p-values can lead to more biased results than using DAGs. 

# Setup ----

library(tidyverse) # ol faithful

# Simulating Data ----

n = 250 # arbitrary sample size 

# Notation for the variables below: 
# - X: Coffee
# - Y: Happiness level
# - C1: Number of donuts per week
# - C2: Sunshine
# - C3: Sleep
# - C4: Lifting

# Simulating data with arbitrary parameters

set.seed(456) # setting seed for reproducibility

df <- data.frame(
  c1 = rnorm(n = n, mean = 7, sd = 1.5), 
  c2 = rbinom(n = n, size = 1, prob = 0.65),
  c3 = rnorm(n = n, mean = 10, sd = 2)
) %>% 
  dplyr::mutate(
    trt = rbinom(n = n, size = 1, prob = plogis(0.1*c1 + 0.3*c2 - 0.05*c3)),
    c4 = 1.5 + 3*c3 - 2*trt,
    y = 0.5*c1 + 0.2*c2 + 0.4*c3 + 0.5*c4 + 2*trt
  )

# Variable Selection Using P-Value ----

#... Univariate Selection ----

# Here, testing the association between the outcome and each variable. Not 
# testing treatment because that needs to be included regardless. Using a 
# cutoff value of 0.20 

# For C1

unimod <- glm(y ~ c1, 
               family = stats::gaussian(),
               data = df)

broom::tidy(unimod) 

# For C2

unimod <- glm(y ~ c2, 
              family = stats::gaussian(),
              data = df)

broom::tidy(unimod)

# For C3

unimod <- glm(y ~ c3, 
              family = stats::gaussian(),
              data = df)

broom::tidy(unimod)

# For C4

unimod <- glm(y ~ c4, 
              family = stats::gaussian(),
              data = df)

broom::tidy(unimod)

#... Multivariable Model ----

# Based on running the above, I ended up with including c1, c3 and c4. 
# Including these variables to see if still significant in multivariable model. 

multimod <- glm(y ~ trt + c1 + c3 + c4, 
                family = stats::gaussian(),
                data = df)

broom::tidy(multimod) # all are significant in this example. 

# Variable Selection Based on DAG ----

# Using daggity.net know I need to adjust for c1, c2 and c3 (not c4)

dagmod <- glm(y ~ trt + c1 + c2 + c3, 
                family = stats::gaussian(),
                data = df)

broom::tidy(dagmod) # just to look at results from the model. 

# Repeating to Measure Bias ----

#... Function for Using P-values ----

# Note: For simplicity using the same variables, but in practice I'd be curious results
# of doing univariate analyses each time to determine the variables to include. 

pvaluemod <- function(n){
  
  # Simulating the data, similar as above. 
    
    df <- data.frame(
      c1 = rnorm(n = n, mean = 7, sd = 1.5), 
      c2 = rbinom(n = n, size = 1, prob = 0.65),
      c3 = rnorm(n = n, mean = 10, sd = 2)
    ) %>% 
      dplyr::mutate(
        trt = rbinom(n = n, size = 1, prob = plogis(0.1*c1 + 0.3*c2 + 0.05*c3)),
        c4 = 3*c3 + 2*trt,
        y = 0.05*c1 + 0.2*c2 + 0.04*c3 + 0.03*c4 + 2*trt + rnorm(n = n)
      )
    
    # Fitting a GLM with a Gaussian distribution for the outcome and identity link
    
    mod <- glm(y ~ trt + c1 + c3 + c4, 
               family = gaussian(), 
               data = df)
    
    # Extracting the estimate
    
    estimate = mod$coefficients[2]
    
    # Calcuating the bias as estimated effect - actual effect (2 from the above, 2*trt)
    
    bias = estimate - 2
    
    return(bias)
    
}

#... Function for using DAGs ----

# See above function for comments. Very similar here only different variables are included. 

dagmod <- function(n){
  
  df <- data.frame(
    c1 = rnorm(n = n, mean = 7, sd = 1.5), 
    c2 = rbinom(n = n, size = 1, prob = 0.65),
    c3 = rnorm(n = n, mean = 10, sd = 2)
  ) %>% 
    dplyr::mutate(
      trt = rbinom(n = n, size = 1, prob = plogis(0.1*c1 + 0.3*c2 + 0.05*c3)),
      c4 = 3*c3 + 2*trt,
      y = 0.05*c1 + 0.2*c2 + 0.04*c3 + 0.03*c4 + 2*trt + rnorm(n = n)
    )
  
  mod <- glm(y ~ trt + c1 + c2 + c3, 
             family = gaussian(), 
             data = df)
  
  estimate = mod$coefficients[2]
  
  bias = estimate - 2
  
  return(bias)
  
  
}

#... Repeating 1000 times! ----

n.sim = 250 # sample size of 250 is arbitrary. 

# Based on variables from p-values

pvalue_bias <- replicate(1000, pvaluemod(n = n.sim), simplify = FALSE) 

pvalue_bias <- do.call(rbind, pvalue_bias) # reformatting 

# Based on variables from a DAG 

dag_bias <- replicate(1000, dagmod(n = n.sim), simplify = FALSE) 

dag_bias <- do.call(rbind, dag_bias) # reformatting 

# Combining Results Into a Dataframe ----

# This code is messy but it's basically using the results to combine into a dataframe
# then calculating the value _num which will be used in the monte carlo estimate of SE for the 
# bias. 

df_bias <- data.frame(
  dag = (dag_bias %>% as.data.frame())$trt, 
  pvalue = (pvalue_bias %>% as.data.frame())$trt
) %>% 
  dplyr::mutate(
    dag_num = (dag - mean(dag))^2,
    pvalue_num = (pvalue - mean(pvalue))^2
    # _squared wasn't used in final version but could be used if you want to look at MSE. 
    # dag_squared = ((dag-2)^2)/n.sim,
    # pvalue_squared = ((pvalue-2)^2)/n.sim
  )

# Combining final results. Again, this is messy and should be fixed. Basically just 
# calculating the final results (mean of bias and monte carlo se of bias)

bias_results <- data.frame(
  bias = c(mean(df_bias$dag), mean(df_bias$pvalue)),
  mc_se = c(sqrt(sum(df_bias$dag_num)/(n.sim*(n.sim-1))),
          sqrt(sum(df_bias$pvalue_num)/(n.sim*(n.sim-1)))),
  method = c("DAG", "P-Value")
)

# Plots! ----

# Who doesn't love a good plot? 

#... Reformatting Data ----

# Reformatting data to plot. 

df_plot <- df_bias %>% 
  pivot_longer(
    cols = c(dag, pvalue),
    names_to = "method", 
    values_to = "bias"
  )

#... Actual Plot! ----

ggplot(df_plot, aes(x = method, y = bias, fill = method)) +
  
  # Using boxplot. Typically I don't like using boxplots but this is for linkedin/teaching
  # so not a big deal. In general recommend a different method to show the distribution like 
  # density or ggridges package, etc. 
  
  geom_boxplot() +
  
  # Adding labels and title/subtitle. 
  
  labs(title = "Bias in Treatment Effect Estimates using Variable Selection", 
       subtitle = "Selection based on P-Values vs. Selection based on DAG",
       y = "Bias",
       x = "Variable Selection Method") +
  
  # Making theme minimal as a basic starting point. 
  
  theme_minimal() +
  
  # Renaming the label
  scale_x_discrete(labels = c(dag = "DAG", pvalue = "P-Value")) +
  
  # Reformatting theme to have bold title text and a larger font size. 
  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "bold"),
    text = element_text(size = 20),
    legend.position = "none"
  ) +
  
  # Changing Y scale. 
  
  scale_y_continuous(limits = c(-0.7, 0.7), 
                     breaks = c(-0.7, -0.5, -0.3, -0.1, 0.1, 0.3, 0.5, 0.7)) 
