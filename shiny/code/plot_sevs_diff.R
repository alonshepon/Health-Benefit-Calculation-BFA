
# Plot SEVs
# data <- sevs; country <- "United States"
plot_sevs_diff <- function(data, country, base_theme){
  
  # Country
  country_do <- country
  
  # Format data
  sdata <- data %>% 
    # Country of interest
    filter(country==country_do) %>% 
    # Reshape for plotting
    select(nutrient, sex, age_group, sev_delta) %>% 
    mutate(direction=ifelse(sev_delta>0, "Increased risk", "Lowered risk"))
  
  # Plot data
  g <- ggplot(sdata, aes(x=age_group, y=sev_delta, fill=direction)) +
    facet_grid(nutrient~sex, scales="free_y") +
    geom_bar(stat="identity") +
    # Horizontal reference line
    geom_hline(yintercept=0, color="grey30", linetype="dotted") +
    # Labels
    labs(x="Age group", y="Difference in summary exposure value (SEV)\n(high road - baseline)", 
         title=paste("Difference in summary exposure values (SEVs): ", country_do)) +
    scale_fill_discrete(name="High road diets: ") +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g
  
  
}

