
# Plot nutrient distributions
# data <- nutr_dists; country <- "Ghana"; nutrient <- "Calcium"
plot_nutr_dists <- function(data, country, nutrient, base_theme){

  # Params
  cntry_do <- country
  nutr_do <- nutrient
  
  # Subset data
  sdata <- data %>% 
    filter(country==cntry_do & nutrient==nutr_do)
  
  # Build data
  ddata <- purrr::map_df(1:nrow(sdata), function(i){
    
    # Info
    sex <- sdata$sex[i]
    age_group <- sdata$age_group[i]
    best_dist <- sdata$best_dist[i]
    scenario <- sdata$scenario[i]
    
    # Determine range to draw over
    xmax <- max(sdata$mean_group) * 4 %>% round()
    x <- seq(0, xmax, length.out = 200)
    
    # Simulate data
    if(best_dist=="gamma"){
      shape <- sdata$g_shape[i]
      rate <- sdata$g_rate[i]
      x_shift <- sdata$g_mean_diff[i]
      y <- dgamma(x-x_shift, shape=shape, rate=rate)
    }
    
    # Simulate data
    if(best_dist=="log-normal"){
      meanlog <- sdata$ln_meanlog[i]
      sdlog <- sdata$ln_sdlog[i]
      x_shift <- sdata$ln_mean_diff[i]
      y <- dlnorm(x-x_shift, meanlog=meanlog, sdlog=sdlog)
    }
    
    # Build data
    df <- data.frame(nutrient=nutr_do, 
                     scenario=scenario, 
                     country=cntry_do,
                     sex=sex,
                     age_group=age_group,
                     intake=x,
                     density=y)
    
    df
    
  })
  
  # Plot data
  title_text <- paste0("Distribution of within group intakes: ", nutr_do)
  g <- ggplot(ddata, aes(x=intake, y=density, color=scenario, linetype=sex)) + 
    facet_wrap(~age_group, ncol=4, scales="free_y") +
    geom_line(alpha=0.7) +
    # Labels
    labs(x="Habitual intake", y="Density", title=title_text) +
    scale_color_discrete(name="Scenario") +
    scale_linetype_discrete(name="Sex") +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g
  
}
