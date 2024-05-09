# Different Survival Distributions

# Libraries 

library(tidyerse)
library(simsurv)
library(survival)
library(survminer)
library(patchwork)

set.seed(123)  # for reproducibility

# Exponential distribution

cov <- data.frame(
  id = 1:1000, 
  trt = rbinom(n = 1000, size = 1, prob = 0.5), 
  z = rnorm(n = 1000, mean = 5, sd = 2)
)

data_exp <- simsurv::simsurv(dist = "exponential", 
                             lambdas = 0.1,
                             betas = c(trt = -0.5, z = 0.2),
                             x = cov,
                             max = 5
)

exp(-0.5)

df1 = cov %>% 
  dplyr::left_join(
    data_exp
  )

p1 <- ggsurvplot(survfit(Surv(eventtime, status) ~ trt, data = df1))


mod1 <- coxph(Surv(eventtime, status) ~ trt, data = df1)

predict(mod3, type = "survival")

df1 = df1 %>% 
  dplyr::mutate(
    surv_prob = predict(mod1, type = "survival")
  )

p1 <- p1$plot + 
  geom_smooth(data = df1, mapping = aes(eventtime, surv_prob), color = "darkblue", fill = "darkblue")

# Weibell 

data_weibell <- simsurv::simsurv(dist = "weibull", 
                             lambdas = 0.1,
                             gammas = 1.5,
                             betas = c(trt = -0.5, z = 0.2),
                             x = cov,
                             max = 5
)

df2 = cov %>% 
  dplyr::left_join(
    data_weibell
  )

p2 <- ggsurvplot(survfit(Surv(eventtime, status) ~ trt, data = df2))

mod2 <- coxph(Surv(eventtime, status) ~ trt, data = df2)

df2 = df2 %>% 
  dplyr::mutate(
    surv_prob = predict(mod2, type = "survival")
  )

p2 <- p2$plot + 
  geom_smooth(data = df2, mapping = aes(eventtime, surv_prob), color = "darkblue", fill = "darkblue")


# Gompertz

data_gomp <- simsurv::simsurv(dist = "gompertz", 
                                 lambdas = 0.1,
                                 gammas = 0.5,
                                 betas = c(trt = -0.5, z = 0.2),
                                 x = cov,
                                 max = 5
)

df3 = cov %>% 
  dplyr::left_join(
    data_gomp
  )


p3 <- ggsurvplot(survfit(Surv(eventtime, status) ~ trt, data = df3),
                 conf.int = TRUE) 

mod3 <- coxph(Surv(eventtime, status) ~ trt, data = df3)

df3 = df3 %>% 
  dplyr::mutate(
    surv_prob = predict(mod3, type = "survival")
  )

p3 <- p3$plot + 
  geom_smooth(data = df3, mapping = aes(eventtime, surv_prob), color = "darkblue", fill = "darkblue")

# Patching Together ----

p1 + p2 + p3


# Now Try a Different Fit For The Last One 

library(flexsurv)

install.packages("flexsurv")

mod_weib <- flexsurv::flexsurvspline(Surv(eventtime, status) ~ trt, 
                                      data = df2, k = 0)

p4 <- ggsurvplot(survfit(Surv(eventtime, status) ~ trt, data = df2),
                 conf.int = TRUE) 

df4 = df2 %>% 
  dplyr::mutate(
    surv_prob = predict(mod_weib, type = "survival")
  )

p4 <- p4$plot + 
  geom_smooth(data = df2, mapping = aes(eventtime, surv_prob), color = "darkblue", fill = "darkblue")


p4

broom::tidy(mod2)
broom::tidy(mod_weib)

AIC(mod2)
AIC(mod_weib) # fits substantially better

p2

# Theoretical survival function - Exponential
surv_func_exp <- function(t) exp(-0.1 * t)

# Theoretical survival function - Weibull
surv_func_weib <- function(t) exp(-(0.1 * t)^0.5)

# Create time sequences for plotting theoretical curves
time_seq <- seq(0, max(data_weib$time), by = 0.1)

# Create a data frame for ggplot
plot_data <- data.frame(
  time = time_seq,
  Survival_Exp = surv_func_exp(time_seq),
  Survival_Weib = surv_func_weib(time_seq)
)

# Plotting with ggplot2
ggplot() +
  geom_step(aes(time, surv, color = "KM Exp"), data = as.data.frame(km_exp), direction = "hv") +
  geom_line(aes(time, Survival_Exp, color = "Theory Exp"), data = plot_data) +
  geom_step(aes(time, surv, color = "KM Weib"), data = as.data.frame(km_weib), direction = "hv") +
  geom_line(aes(time, Survival_Weib, color = "Theory Weib"), data = plot_data) +
  labs(title = "Comparison of KM and Theoretical Survival Curves",
       x = "Time",
       y = "Survival Probability") +
  scale_color_manual(values = c("KM Exp" = "blue", "Theory Exp" = "red", "KM Weib" = "green", "Theory Weib" = "orange")) +
  theme_minimal()


