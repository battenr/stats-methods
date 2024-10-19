# Showing the Priors ----

library(tidybayes)
library(brms)
library(tidyverse)
library(ggdist)
library(distributional)

subguide

# Priors ----

# Note: Do not need any simulated data for this part ----

# 


# Test ----

# NB these priors are made up!
priors = c(
  prior(normal(0, 10), class = b),
  prior(normal(0, 2.5), class = b), 
  prior (normal(3, 2.5), class = b)
)



priors %>%
  parse_dist(prior) %>%

  ggplot(aes(y = paste(class, "~", format(.dist_obj)), xdist = .dist_obj)) +
  stat_halfeye(subguide = subguide_inside(position = "right", title = "density"), color = "purple", fill = "pink") +
  labs(
    title = "stat_halfeye()",
    subtitle = "with parse_dist() and brms::prior() to show priors",
    x = NULL,
    y = NULL
  ) + 
  theme_minimal() 



##### ----

priors %>%
  parse_dist(prior) %>%
  ggplot(aes(y = paste(class, "~", format(.dist_obj)), xdist = .dist_obj)) +
  stat_halfeye(subguide = subguide_inside(position = "right", title = "density")) +
  labs(
    title = "stat_halfeye()",
    subtitle = "with parse_dist() and brms::prior() to show priors",
    x = NULL,
    y = NULL
  )


prior_post = data.frame(
  prior = dist_normal(0, 1),
  posterior = dist_normal(0.1, 0.5)
)

separate_scale_plot = prior_post %>%
  ggplot() +
  stat_halfeye(aes(xdist = posterior)) +
  stat_slab(aes(xdist = prior), fill = NA, color = "red") +
  labs(
    subtitle = "default: no shared thickness scale"
  )

shared_scale_plot = prior_post %>%
  ggplot() +
  stat_halfeye(aes(xdist = posterior)) +
  stat_slab(aes(xdist = prior), fill = NA, color = "#e41a1c") +
  scale_thickness_shared() +
  labs(subtitle = "with scale_thickness_shared()")

separate_scale_plot + shared_scale_plot + plot_annotation(title = "prior (slab) + posterior (halfeye)")




prior_post = data.frame(
  prior = dist_normal(0, 1),
  posterior = dist_normal(0.1, 0.5)
)

separate_scale_plot = prior_post %>%
  ggplot() +
  stat_halfeye(aes(xdist = posterior)) +
  stat_slab(aes(xdist = prior), fill = NA, color = "red") +
  labs(
    subtitle = "default: no shared thickness scale"
  )

shared_scale_plot = prior_post %>%
  ggplot() +
  stat_halfeye(aes(xdist = posterior)) +
  stat_slab(aes(xdist = prior), fill = NA, color = "#e41a1c") +
  scale_thickness_shared() +
  labs(subtitle = "with scale_thickness_shared()")

separate_scale_plot + shared_scale_plot + plot_annotation(title = "prior (slab) + posterior (halfeye)")



priors = c(
  prior(normal(1, 10), class = b),
  prior(normal(0, 2.5), class = b)
  # lb = 0 sets a lower bound of 0, i.e. a half-Normal distribution
)

priors %>% 
  parse_dist(prior)

?parse_dist()


priors %>%
  parse_dist(prior) %>%
  ggplot(aes(y = prior, xdist = .dist_obj)) +
  stat_halfeye() +
  scale_y_discrete(labels = c("t1", "t2")) +
  labs(
    title = "stat_halfeye()",
    subtitle = "with parse_dist() and brms::prior() to show priors",
    x = NULL,
    y = NULL
  )
