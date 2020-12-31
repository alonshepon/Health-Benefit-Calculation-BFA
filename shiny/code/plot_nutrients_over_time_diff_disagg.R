
# Plot nutrients over time
# data <- nutrients_disagg; country <- "Ghana"
plot_nutrients_over_time_diff_disagg <- function(data, country, base_theme){
  
  # Country
  country_do <- country
  
  # Format data
  sdata <- data %>% 
    # Country of interest
    filter(country==country_do) %>% 
    # Reshape for plotting
    select(-intake_diff) %>% 
    gather(key="nutr_calc", value='intake', 7:ncol(.)) %>% 
    mutate(nutr_calc=recode_factor(nutr_calc,
                                   "intake_orig"="Original",
                                   "intake"="Diversity disaggregation")) %>% 
    # Format nutrient label
    mutate(nutrient_label=paste0(nutrient, "\n(", nutrient_units, ")")) %>% 
    # Arrange
    select(iso3:nutrient_units, nutrient_label, scenario, nutr_calc, year, intake, everything()) %>% 
    # Spread
    spread(key="scenario", value="intake") %>% 
    rename(intake_high="High road", intake_base=Base) %>% 
    mutate(intake_diff=intake_high - intake_base)

  
  # Plot data
  g <- ggplot(sdata, aes(x=year, y=intake_diff, color=nutr_calc)) +
    facet_wrap(~nutrient_label, ncol=4, scale="free_y") +
    geom_line() +
    # Add horizontal line
    geom_hline(yintercept=0, color="grey30", linetype="dotted") +
    # Labels
    labs(x="", y="Difference in mean daily\nintake per capita", title=paste0("Difference in daily per capita nutrient intake: ", country_do)) +
    scale_color_discrete(name="Intake calculation") +
    # Theme
    theme_bw() + base_theme + 
    theme(legend.position="bottom")
  g
  
  
}