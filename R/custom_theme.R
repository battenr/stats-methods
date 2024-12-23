custom_theme <- function() {
  theme_minimal() %+replace% # basing this on the theme_survminer() but editing some of the components
    theme(
      plot.title = element_text(hjust = 0.5, family = "Jost", face = "bold", size = 20),
      #axis.title = element_text(family = "Jost Medium"),
      plot.subtitle = element_text(hjust = 0.5, size = 16),
      text = element_text(family = "Jost", size = 16)
    ) 
}
