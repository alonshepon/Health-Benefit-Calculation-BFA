
# Plot nutrients over time
# data <- nutrients_disagg; country <- "Ghana"
plot_nutrients_over_time_disagg <- function(data, country, base_theme){
  
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
    mutate(nutrient_label=paste0(nutrient, "\n(", nutrient_units, ")"))
  
  # Plot data
  g <- ggplot(sdata, aes(x=year, y=intake, color=scenario, linetype=nutr_calc)) +
    facet_wrap(~nutrient_label, ncol=4, scale="free_y") +
    geom_line() +
    # Add vertcial line
    geom_vline(xintercept=2019, color="grey30", linetype="dotted") +
    # Labels
    labs(x="", y="Mean daily intake per capita", title=paste0("Daily per capita nutrient intake with and without diversity disaggregation: ", country_do)) +
    scale_color_discrete(name="Scenario") +
    scale_linetype_discrete(name="Intake calculation") +
    # Theme
    theme_bw() + base_theme + 
    theme(legend.position="bottom")
  g
  
  
}