

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outputdir <- "data/cosimo_nutr_disagg/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outputdir, "COSIMO_nutrient_by_scenario_cntry_with_dissagg.Rds")) %>% 
  # Remove Belize
  filter(country!="Belize") %>% 
  # Recode omegas 
  mutate(nutrient=recode(nutrient, "Omega-3 fatty acids"="DHA+EPA fatty acids")) %>% 
  # Format nutrients
  mutate(nutrient_units=gsub("/p/d", "/d", nutrient_units))

# World
world <- rnaturalearth::ne_countries("small", returnclass = "sf")

# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_blank(),
                    axis.title=element_blank(),
                    legend.text=element_text(size=4),
                    legend.title=element_text(size=5),
                    strip.text=element_blank(),
                    plot.title=element_text(size=7),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    legend.background = element_rect(fill=alpha('blue', 0)),
                    legend.position = c(0.12, 0.35))

# Plot data
# nutrient <- "Omega-3 fatty acids"
plot_map <- function(nutrient){
  
  # Subset data
  nutrient_do <- nutrient
  
  # Subset data
  sdata <- data_orig %>% 
    filter(nutrient==nutrient_do & year==2030) %>% 
    select(-c(intake_orig, intake_diff)) %>% 
    spread(key="scenario", value="intake") %>% 
    rename(intake_base=Base, intake_high="High road") %>% 
    mutate(intake_diff=intake_high - intake_base)
  
  # If omegas
  if(nutrient_do=="DHA+EPA fatty acids"){
    
    hist(sdata$intake_base, breaks=seq(0,16,0.5))
    hist(sdata$intake_high, breaks=seq(0,20,0.5))
    hist(sdata$intake_diff, breaks=seq(0,1.2, 0.05))
    
    
    # Caps
    sdata <- sdata %>% 
      mutate(intake_base=pmin(2.5,intake_base),
             intake_high=pmin(2.5,intake_high),
             intake_diff=pmin(0.2,intake_diff))
  }
  
  # Spatialize
  sdata_sf <- world %>% 
    left_join(sdata, by=c("gu_a3"="iso3"))
  
  # Nutrient label
  units <- sdata$nutrient_units %>% unique()
  nutrient_label <- paste0(nutrient_do, " (", units, ")")
  
  # Base
  g1 <- ggplot(sdata_sf) +
    geom_sf(mapping=aes(fill=intake_base), lwd=0.1, color="grey30") +
    # Labels
    labs(title=nutrient_label) +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend
    scale_fill_gradientn(name="Base\n2030 intake", colors=RColorBrewer::brewer.pal(9, "YlGnBu")) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.4, barheight = 1.5)) +
    # Theme
    theme_bw() + base_theme
  g1
  
  # High production
  g2 <- ggplot(sdata_sf) +
    geom_sf(mapping=aes(fill=intake_high), lwd=0.1, color="grey30") +
    # Labels
    labs(title=" ") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend
    scale_fill_gradientn(name="High\n2030 intake", colors=RColorBrewer::brewer.pal(9, "YlGnBu")) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.4, barheight = 1.5)) +
    # Theme
    theme_bw() + base_theme
  g2
  
  # Difference
  g3 <- ggplot(sdata_sf) +
    geom_sf(mapping=aes(fill=intake_diff), lwd=0.1, color="grey30") +
    # Labels
    labs(title=" ") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend
    scale_fill_gradient2(name="Difference\n(high - base)",
                         midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.4, barheight = 1.5)) +
    # Theme
    theme_bw() + base_theme
  g3
  
  # Merge
  g <- gridExtra::grid.arrange(g1, g2, g3, ncol=3)
  g
  
}


# Plot maps
g1 <- plot_map("DHA+EPA fatty acids")
g2 <- plot_map("Vitamin B-12")
g3 <- plot_map("Iron")
g4 <- plot_map("Zinc")
g5 <- plot_map("Calcium")
g6 <- plot_map("Vitamin A, RAE")

# Merge maps
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, ncol=1)

# Export maps
ggsave(g, filename=file.path(plotdir, "FigS4_COSIMO_2030_nutrient_outcomes_diversity_disagg.png"), 
       width=6.5, height=7, units="in", dpi=600)





