# Example of Complex Survey ----

# Simulating Data ----

library(tidyverse) # ol faithful 
library(survey)

# Set seed for reproducibility
set.seed(123)

# Define population data frame
population <- data.frame(
  stratum = rep(1:4, times=c(1000, 1500, 1200, 800)), # population size differs per stratum
  age = rnorm(4500, mean=50, sd=10),
  gender = sample(c("Male", "Female"), size=4500, replace=TRUE, prob=c(0.5, 0.5)),
  income = rnorm(4500, mean=50000, sd=15000)
)

# Create a binary outcome based on covariates
population$outcome <- ifelse(
  population$age + rnorm(4500, mean=0, sd=10) - 0.5 * population$income/1000 + 
    (population$gender == "Female") * 5 > 50, 1, 0
)

# Simulate sampling within each stratum
sample_sizes <- c(100, 150, 120, 80) # sample sizes for each stratum
survey_sample <- population %>%
  group_by(stratum) %>%
  sample_n(size=sample_sizes[stratum])

population %>% 
  slice_sample(n = c(100, 150, 120, 80), by = stratum) %>% view()

strata1 <- population %>% 
  filter(stratum == 1) %>% 
  slice_sample(n = 100)

strata2 <- population %>% 
  filter(stratum == 2) %>% 
  slice_sample(n = 200)

strata3 <- population %>% 
  filter(stratum == 3) %>% 
  slice_sample(n = 300)

strata4 <- population %>%
  filter(stratum == 4) %>% 
  slice_sample(n = 150)

sample <- rbind(strata1, strata2, strata3, strata4) # 750 total

df <- sample %>% 
  dplyr::mutate(
    expansion_weight = case_when(
      stratum == 1 ~ 1000/100, 
      stratum == 2 ~ 1500/200,
      stratum == 3 ~ 1200/300,
      stratum == 4 ~ 800/150
    ), 
    relative_weight = expansion_weight/mean(expansion_weight)
  )

mean(sample_weights$relative_weight)

?survey::svydesign()

pop_design <- survey::svydesign(ids = ~0, 
                                strata = ~stratum, 
                                data = df,
                                weights = ~expansion_weight)

infer_design <- survey::svydesign(ids = ~0, 
                                strata = ~stratum, 
                                data = df,
                                weights = ~relative_weight)

survey::svyglm(income ~ gender + age, 
               family = stats::gaussian(),
               design = infer_design)

survey::brrweights()
survey::deff(pop_design, svymean(mean(income)))

(svymean(~income, design = pop_design, deff = TRUE))

survey::calibrate()

jkstrat <- as.svrepdesign(pop_design)
svymean(~income, design = jkstrat, deff = TRUE)

?deff

survey::svyglm()

?svyglm
