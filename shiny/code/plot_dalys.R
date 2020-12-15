
# Plot food over time
# data <- dalys; country <- "Ghana"
plot_dalys <- function(data, country, base_theme){
  
  # Country
  country_do <- country
  
  # Format data
  sdata <- data %>% 
    # Country of interest
    filter(country==country_do) %>% 
    # Gather
    gather(key="scenario", value="dalys", 7:8) %>% 
    mutate(scenario=recode(scenario,
                           "DALY2030_br"="Base",
                           "DALY2030_hr"="High road"))
    
  # Plot data
  g <- ggplot(sdata, aes(x=age_group, y=dalys, fill=scenario)) +
    facet_grid(~sex, scale="free_y") +
    geom_bar(stat="identity", position="dodge") +
    # Labels
    labs(x="Age group", y="DALYs", title=paste0("Disability-adjutsed life years (DALYs): ", country_do)) +
    scale_fill_discrete(name="Scenario") +
    # Theme
    theme_bw() + base_theme + 
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g
  
}