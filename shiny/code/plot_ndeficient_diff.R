
# Plot number of people deficient
# data <- ndeficient; country <- "Ghana"
plot_ndeficient_diff <- function(data, country, base_theme){
  
  # Country
  country_do <- country
  
  # Format data
  sdata <- data %>% 
    # Country of interest
    filter(country==country_do) %>% 
    # Add status
    mutate(type=ifelse(ndeficient_diff<0, "Fewer deficiencies", "More deficiencies"),
           type=factor(type, levels=c("Fewer deficiencies", "More deficiencies")))
  
  # Plot data
  g <- ggplot(sdata, aes(x=age_group, y=ndeficient_diff/1e3, fill=type)) +
    facet_grid(nutrient~sex, scales="free_y") +
    geom_bar(stat="identity", position = "dodge") +
    # Labels
    labs(x="Age group", y="ΔThousands of people suffering\nfrom nutrient deficiency\n(high - base)",
         title=paste("Micronutrient deficiencies: ", country_do)) +
    scale_fill_manual(name="Difference from high to base", values=c("navy", "darkred"), drop=F) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g
  
  
}