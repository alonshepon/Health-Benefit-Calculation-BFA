
# Plot nutrients over time
plot_nutrients_over_time <- function(data, country, base_theme){
  
  # Country
  country_do <- country
  
  # Format data
  sdata <- data %>% 
    # Country of interest
    filter(country==country_do) %>% 
    # Reduce to total diet
    # filter(food=="Total Diet") %>% 
    filter(food=="Total food") %>% 
    # Reshape for plotting
    select(nutrient, nutrient_units, year, value_lo, value_hi) %>% 
    gather(key="scenario", value='value', 4:ncol(.)) %>% 
    mutate(scenario=recode_factor(scenario,
                                  "value_lo"="Base",
                                  "value_hi"="High road")) %>% 
    # Format nutrient label
    mutate(nutrient_label=paste0(nutrient, "\n(", nutrient_units, ")"))
  
  # Plot data
  g <- ggplot(sdata, aes(x=year, y=value, color=scenario)) +
    facet_wrap(~nutrient_label, ncol=4, scale="free_y") +
    geom_line() +
    # Add vertcial line
    geom_vline(xintercept=2019, color="grey30", linetype="dotted") +
    # Labels
    labs(x="", y="Mean daily intake per capita", title=paste0("Daily per capita nutrient intake: ", country_do)) +
    scale_color_discrete(name="Scenario") +
    # Theme
    theme_bw() + base_theme + 
    theme(legend.position="bottom")
  g
  
  
}