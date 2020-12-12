
# Plot nutrients over time
plot_nutrients_over_time_diff <- function(data, country, base_theme){
  
  # Country
  country_do <- country
  
  # Format data
  sdata <- data %>% 
    # Country of interest
    filter(country==country_do) %>% 
    # Reduce to total diet
    filter(food=="Total Diet") %>% 
    # Format nutrient label
    mutate(nutrient_label=paste0(nutrient, "\n(", nutrient_units, ")"))
  
  # Plot data
  g <- ggplot(sdata, aes(x=year, y=value_diff)) +
    facet_wrap(~nutrient_label, ncol=4, scale="free_y") +
    geom_line() +
    # Add horizontal line
    geom_hline(yintercept=0, color="grey30", linetype="dotted") +
    # Labels
    labs(x="", y="Difference in mean daily\nintake per capita", title=paste0("Difference in daily per capita nutrient intake: ", country_do)) +
    # Theme
    theme_bw() + base_theme + 
    theme(legend.position="bottom")
  g
  
  
}