
# Plot food over time
# data <- dalys; country <- "Ghana"
plot_dalys_diff <- function(data, country, base_theme){
  
  # Country
  country_do <- country
  
  # Format data
  sdata <- data %>% 
    # Country of interest
    filter(country==country_do) %>% 
    # Calculate difference
    mutate(daly_diff=DALY2030_hr-DALY2030_br)
    
  # Plot data
  g <- ggplot(sdata, aes(x=age_group, y=daly_diff)) +
    facet_grid(~sex, scale="free_y") +
    geom_bar(stat="identity") +
    # Add horizontal line
    geom_hline(yintercept=0, color="grey30", linetype="dotted") +
    # Labels
    labs(x="Age group", y="Difference in 2030 DALYs\n(high road - base)", title=paste0("Difference in disability-adjusted life years (DALYs): ", country_do)) +
    # Theme
    theme_bw() + base_theme + 
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g
  
}