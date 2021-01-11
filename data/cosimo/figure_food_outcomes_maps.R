

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outputdir <- "data/cosimo/processed"
plotdir <- "data/cosimo/figures"

# Read data
data_orig <- readRDS(file.path(outputdir, "COSIMO_2010_2030_food_by_scenario_cntry.rds"))

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
# food <- "Fish"
plot_map <- function(food){
  
  # Subset data
  food_do <- food
  
  # Subset data
  sdata <- data_orig %>% 
    filter(food==food_do & year==2030)

  # Plot hist
  if(food=="Fish"){
    hist(sdata$value_lo, breaks=seq(0,1600,5))
    abline(v=300)
    
    hist(sdata$value_hi, breaks=seq(0,2000,5))
    abline(v=300)
    
    hist(sdata$value_diff, breaks=seq(0,120,2))
    abline(v=20)
    
    sdata <- sdata %>% 
      mutate(value_lo=pmin(value_lo, 300),
             value_hi=pmin(value_hi, 300),
             value_diff=pmin(value_diff, 20))
    
  }
  
  # Spatialize
  sdata_sf <- world %>% 
    left_join(sdata, by=c("gu_a3"="iso3"))
  
  # Base
  g1 <- ggplot(sdata_sf) +
    geom_sf(mapping=aes(fill=value_lo), lwd=0.1, color="grey30") +
    # Labels
    labs(title=paste0(food,  " (g/p/d)")) +
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
    geom_sf(mapping=aes(fill=value_hi), lwd=0.1, color="grey30") +
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
    geom_sf(mapping=aes(fill=value_diff), lwd=0.1, color="grey30") +
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

sort(unique(data_orig$food))

# Plot maps
g1 <- plot_map("Fish")
g2 <- plot_map("Beef and Veal")
g3 <- plot_map("Sheep")
g4 <- plot_map("Pork")
g5 <- plot_map("Poultry")
g6 <- plot_map("Eggs")

# Plot maps
g7 <- plot_map("Butter")
g8 <- plot_map("Milk")
g9 <- plot_map("Maize")
g10 <- plot_map("Other Coarse Grains")
g11 <- plot_map("Other Oilseeds")
g12 <- plot_map("Pulses")

g13 <- plot_map("Rice")
g14 <- plot_map("Roots tubers")
g15 <- plot_map("Sugar")
g16 <- plot_map("Vegetable oils")
g17 <- plot_map("Wheat")

# Merge maps
merge1 <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, ncol=1)
merge2 <- gridExtra::grid.arrange(g7, g8, g9, g10, g11, g12, ncol=1)
merge3 <- gridExtra::grid.arrange(g13, g14, g15, g16, g17, g1, ncol=1)

# Export maps
ggsave(merge1, filename=file.path(plotdir, "COSIMO_2030_food_outcomes_panel1.png"), 
       width=6.5, height=7, units="in", dpi=600)

ggsave(merge2, filename=file.path(plotdir, "COSIMO_2030_food_outcomes_panel2.png"), 
       width=6.5, height=7, units="in", dpi=600)

# Remember to cut fish panel
ggsave(merge3, filename=file.path(plotdir, "COSIMO_2030_food_outcomes_panel3.png"), 
       width=6.5, height=7, units="in", dpi=600)
