

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outputdir <- "data/cosimo/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outputdir, "COSIMO_2010_2030_nutr_by_scenario_cntry_food.Rds"))

# World
world <- rnaturalearth::ne_countries("small", returnclass = "sf")


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  filter(food=="Total Diet")

# Read data
################################################################################

# Base theme
base_theme <-  theme(axis.text=element_text(size=6),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=8),
                     strip.text=element_text(size=8),
                     plot.title=element_text(size=9),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))








# Plot map
plot_map <- function(nutrient){
  
  # Format data
  nutr_do <- nutrient
  data_sf <- world %>% 
    left_join(data %>% filter(nutrient==nutr_do), by=c("gu_a3"="iso3"))
  
  # Plot
  g <- ggplot(data_sf) +
    geom_sf(mapping=aes(fill=value_pdiff), lwd=0.2) +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    labs(title=nutr_do) +
    scale_fill_gradient2(name="% change in intake\n(high relative to base)", 
                         low="darkred", high="navy", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position=c(0.11,0.37),
          axis.text = element_blank(),
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)))
  g
  
}

# Plot maps
zinc <- plot_map("Zinc")
calcium <- plot_map("Calcium")
vitaminA <- plot_map("Vitamin A")
iron <- plot_map("Iron")
omegas <- plot_map("Omega-3 fatty acids")
zinc <- plot_map("Zinc")
vitaminB12 <- plot_map("Vitamin B-12")

# Merge

# Export


