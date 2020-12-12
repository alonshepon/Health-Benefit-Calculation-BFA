
# Plot SEVs
# data <- sevs; country <- "Ghana"
plot_sevs <- function(data, country, base_theme){
  
  # Country
  country_do <- country
  
  # Format data
  sdata <- data %>% 
    # Country of interest
    filter(country==country_do) %>% 
    # Reshape for plotting
    select(nutrient, sex, age_group, sev_base, sev_high) %>% 
    gather(key="scenario", value='value', 4:ncol(.)) %>% 
    mutate(scenario=recode_factor(scenario,
                                  "sev_base"="Base",
                                  "sev_high"="High road")) 
  
  # Plot data
  g <- ggplot(sdata, aes(x=age_group, y=value, group=scenario, fill=scenario)) +
    facet_grid(nutrient~sex, scales="free_y") +
    geom_bar(stat="identity", position = "dodge") +
    # Labels
    labs(x="Age group", y="Summary exposure value (SEV)", title=paste("Summary exposure values (SEVs): ", country_do)) +
    scale_fill_discrete(name="Scenario") +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g
  
  
}