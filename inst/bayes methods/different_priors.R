# Trying Different Priors

# Want to try out the following priors: 
# - Normal
# - Student's t-distribution
# - Beta for binomial outcome

# Plotting Priors 

library(tidyverse)
library(brms)
library(tidybayes)
library(bayesplot)

priors <- c(prior(normal(4, 2), class = Intercept),
            prior(student_t(3, 4, 2), class = b),
            prior(exponential(1/100), class = sigma, lb = 0),
            prior(beta(0.1, 0.3), class = b)
)

clrs <- c(
  "#FFBE00",  # MCRN yellow
  "#B92F0A",  # MCRN red
  "#792A26",  # MCRN maroon
  "#54191B",  # MCRN brown
  "#242424",  # MCRN dark gray
  "#2660ae"   # Blue from MCR flag
)



priors |> 
  parse_dist() |> 
  mutate(prior = fct_inorder(prior)) |> 
  ggplot(aes(y = 0, dist = .dist, args = .args, fill = prior)) +
  stat_slab(normalize = "panels") +
  scale_fill_manual(values = clrs[c(1, 2, 3, 4)], guide = "none") +
  labs(x = NULL, y = NULL) +
  facet_wrap(vars(prior), scales = "free_x") 

# An alternative way ----

p1 <- ggplot() +
  stat_function(geom = "area", 
                fun = ~extraDistr::dlst(., df = 1, mu = 0, sigma = 3), 
                fill = clrs[2]) +
  xlim(c(-20, 20)) +
  annotate(geom = "label", x = 0, y = 0.02, label = "Student t(1, 0, 3)") +
  labs(x = "α and βs") +
  theme_pandem(prior = TRUE)

studentt = ggplot() +
  stat_function(geom = "area", 
                fun = ~extraDistr::dlst(., df = 1, mu = 0, sigma = 3),
                fill = "lightpink") +
  xlim(c(-20, 20)) +
  annotate(geom = "label", x = 0, y = 0.02, label = "Student t(1, 0, 3)") +
  labs(x = "α and βs") #+
#theme_pandem(prior = TRUE)

norm <- ggplot() +
  stat_function(geom = "area", 
                fun = ~dnorm(., mean = 0, sd = 3),
                fill = "lightpink") +
  xlim(c(-20, 20)) +
  annotate(geom = "label", x = 0, y = 0.02, label = "Normal(0, 3)") +
  labs(x = "α and βs") #+
#theme_pandem(prior = TRUE)

library(tidyverse)
library(patchwork)
library(extraDistr)

studentt + norm
