
# Plot food over time
plot_food_over_time <- function(data, country, base_theme){
  
  # Country
  country_do <- country
  
  # Format data
  sdata <- data %>% 
    # Country of interest
    filter(country==country_do) %>% 
    # Reshape for plotting
    select(food, year, value_lo, value_hi) %>% 
    gather(key="scenario", value='value', 3:ncol(.)) %>% 
    mutate(scenario=recode_factor(scenario,
                                  "value_lo"="Base",
                                  "value_hi"="High road")) %>% 
    # Format food name
    mutate(food=stringr::str_to_sentence(food),
           food=recode(food, "Hfcs"="HFCS"))
  
  # Plot data
  g <- ggplot(sdata, aes(x=year, y=value*1000/365, color=scenario)) +
    facet_wrap(~food, ncol=4, scale="free_y") +
    geom_line() +
    # Add vertcial line
    geom_vline(xintercept=2019, color="grey30", linetype="dotted") +
    # Labels
    labs(x="", y="Mean daily intake per capita (g/person/day)", title=paste0("Daily per capita food consumption: ", country_do)) +
    scale_color_discrete(name="Scenario") +
    # Theme
    theme_bw() + base_theme + 
    theme(legend.position="bottom")
  g
  
  
}