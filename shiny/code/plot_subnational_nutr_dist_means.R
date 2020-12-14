
# Plot food over time
# data <- nutr_dists; country <- "Ghana"
plot_subnational_nutr_dist_means <- function(data, ears, country, base_theme){
  
  # Country
  country_do <- country
  
  # Format data
  sdata <- data %>% 
    # Country of interest
    filter(country==country_do) 
  
  # Plot data
  g <- ggplot(sdata, aes(x=age_group, y=mean_group, fill=scenario)) +
    facet_grid(nutrient~sex, scale="free_y") +
    geom_bar(stat="identity", position="dodge") +
    # Add ears
    geom_line(ears, mapping=aes(x=age_group, y=ear, group=sdi_group, linetype=sdi_group), inherit.aes=F, color="black") +
    # Labels
    labs(x="Age group", y="Mean daily intake per capita") +
    scale_fill_discrete(name="Scenario") +
    scale_linetype(name="SDI group") +
    # Theme
    theme_bw() + base_theme + 
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g
  
}