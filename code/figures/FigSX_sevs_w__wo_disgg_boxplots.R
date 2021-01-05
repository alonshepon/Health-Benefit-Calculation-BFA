

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outputdir <- "output"
plotdir <- "figures"

# Read data
# data_orig <- readRDS(file.path(outputdir, "2030_sevs_base_high_road_final.Rds"))
data_orig <- readRDS(file.path(outputdir, "2030_sevs_base_high_road_final_diversity_disagg.Rds"))

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
# nutrient <- "Calcium"
plot_map <- function(nutrient){
  
  # Subset data
  nutrient_do <- nutrient
  sdata <- data_orig %>% 
    filter(nutrient==nutrient_do) %>% 
    group_by(country, iso3) %>% 
    summarize(sev_base=mean(sev_base, na.rm=T),
              sev_high=mean(sev_high, na.rm=T)) %>% 
    ungroup() %>% 
    mutate(sev_diff=sev_high-sev_base)
  
  # Spatialize
  sdata_sf <- world %>% 
    left_join(sdata, by=c("gu_a3"="iso3"))
  
  # Base
  g1 <- ggplot(sdata_sf) +
    geom_sf(mapping=aes(fill=sev_base), lwd=0.1, color="grey30") +
    # Labels
    labs(title=nutrient_do) +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend
    scale_fill_gradientn(name="Base\n2030 SEVs", colors=RColorBrewer::brewer.pal(9, "YlOrRd"), lim=c(0,100)) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.4, barheight = 1.5)) +
    # Theme
    theme_bw() + base_theme
  g1
  
  # High production
  g2 <- ggplot(sdata_sf) +
    geom_sf(mapping=aes(fill=sev_high), lwd=0.1, color="grey30") +
    # Labels
    labs(title=" ") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend
    scale_fill_gradientn(name="High\n2030 SEVs", colors=RColorBrewer::brewer.pal(9, "YlOrRd"), lim=c(0,100)) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.4, barheight = 1.5)) +
    # Theme
    theme_bw() + base_theme
  g2
  
  # Difference
  g3 <- ggplot(sdata_sf) +
    geom_sf(mapping=aes(fill=sev_diff), lwd=0.1, color="grey30") +
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
g1 <- plot_map("Omega-3 fatty acids")
g2 <- plot_map("Vitamin B-12")
g3 <- plot_map("Iron")
g4 <- plot_map("Zinc")
g5 <- plot_map("Calcium")
g6 <- plot_map("Vitamin A, RAE")

# Merge maps
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, ncol=1)

# Export maps
ggsave(g, filename=file.path(plotdir, "FigSX_sevs_base_high_diff_diversity_disagg.png"), 
       width=6.5, height=7, units="in", dpi=600)

