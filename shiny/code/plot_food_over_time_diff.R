
# Plot food over time
plot_food_over_time_diff <- function(data, country, base_theme){
  
  # Country
  country_do <- country
  
  # Format data
  sdata <- data %>% 
    # Country of interest
    filter(country==country_do) %>% 
    # Format food name
    mutate(food=stringr::str_to_sentence(food),
           food=recode(food, "Hfcs"="HFCS"))
  
  # Plot data
  g <- ggplot(sdata, aes(x=year, y=value_diff)) +
    facet_wrap(~food, ncol=4, scale="free_y") +
    geom_line() +
    # Add horizontal line
    geom_hline(yintercept=0, color="grey30", linetype="dotted") +
    # Labels
    labs(x="", y="Difference in mean daily\nintake per capita (kg/person/day)", title=paste0("Difference in daily per capita food consumption: ", country_do)) +
    # Theme
    theme_bw() + base_theme + 
    theme(legend.position="bottom")
  g
  
  
}