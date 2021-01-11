

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "data/cosimo/raw"
outputdir <- "data/cosimo/processed"
plotdir <- "data/cosimo/figures"

# Read data
data_orig <- readRDS(file.path(outputdir, "COSIMO_2010_2030_food_by_scenario_cntry.Rds"))

# World
world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")


#  Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce to 2030
  filter(year==2030) %>% 
  # Simplify
  select(iso3, country, food, value_lo:value_diff_perc)


# Plot data (all)
################################################################################

# Add expanded data to SF
data_all_sf <- world %>% 
  left_join(data, by=c("gu_a3"="iso3"))

# Plot data
g <- ggplot(data_all_sf) +
  facet_wrap(~ food, ncol=3) +
  geom_sf(mapping=aes(fill=value_diff_perc), lwd=0.1, color="grey30") +
  # Legend
  scale_fill_gradient2(name="% difference in 2030\nunder high and low roads", midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text=element_blank(),
        axis.title=element_blank(),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        strip.text=element_text(size=6),
        plot.title=element_text(size=8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_perc_food_diff_all.pdf"), 
       width=8.5, height=11, units="in", dpi=600)


# Loop through individually
################################################################################

# Foods
foods <- sort(unique(data$food))

# Loop through foods
for(i in 1:length(foods)){
  
  # Subset 
  food_do <- foods[i]
  sdata <- data_all_sf %>% 
    filter(food==food_do)
  
  # Plot
  g <- ggplot(sdata) +
    geom_sf(mapping=aes(fill=value_diff_perc), lwd=0.1, color="grey30") +
    # Labels
    labs(title=food_do) +
    # Legend
    scale_fill_gradient2(name="% difference in 2030\nunder high and low roads", midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Theme
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text=element_blank(),
          axis.title=element_blank(),
          legend.text=element_text(size=6),
          legend.title=element_text(size=8),
          strip.text=element_text(size=6),
          plot.title=element_text(size=8),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))
  g
  
  # Export
  figname <- paste0("figure_perc_food_diff", food_do, ".png")
  ggsave(g, filename=file.path(plotdir, figname), 
         width=6.5, height=4, units="in", dpi=600)
  
}

