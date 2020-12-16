

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
sevs <- readRDS(file.path(outputdir, "2030_sevs_base_high_road_final.Rds")) %>% 
  mutate(sev_delta_cap=pmin(sev_delta, 2) %>% pmax(., -2))

# World
world <- rnaturalearth::ne_countries("small", returnclass = "sf")


# Build data
################################################################################


# Calculate country-level means
c_avgs <- sevs %>% 
  group_by(nutrient, iso3, country) %>% 
  summarize(sev_delta_avg=mean(sev_delta, na.rm=T)) %>% 
  ungroup() %>% 
  filter(!is.na(c_avgs))


# Plot data
################################################################################

# Base theme
base_theme <-  theme(axis.text=element_text(size=6),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=8),
                     strip.text=element_text(size=8),
                     plot.title=element_text(size=10),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"))

# Plot map
plot_map <- function(nutrient){
  
  # Format data
  nutr_do <- nutrient
  c_avgs_sf <- world %>% 
    left_join(c_avgs %>% filter(nutrient==nutr_do), by=c("gu_a3"="iso3"))
  
  # Plot
  g <- ggplot(c_avgs_sf) +
    geom_sf(mapping=aes(fill=sev_delta_avg), lwd=0.2) +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    labs(title=nutr_do) +
    scale_fill_gradient2(name="ΔSEVs in 2030 (%)\n(high - base)", 
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position=c(0.2,0.4),
          axis.text = element_blank(),
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)))
  g
  
}

# Plot boxplot
plot_boxplot <- function(nutrient){
  
  # Format data
  nutr_do <- nutrient
  sdata <- sevs %>% 
    filter(nutrient==nutr_do)
  
  # Plot data
  g <- ggplot(sdata, aes(x=age_group, y=sev_delta_cap, fill=sex)) +
    geom_boxplot(outlier.size = 0.3, lwd=0.3) +
    # Add horizontal line
    geom_hline(yintercept=0, col="grey30", linetype="dotted") +
    # Labels
    labs(x="Age group", y="ΔSEVs in 2030 (%)\n(high - base)", title=nutr_do) +
    # Theme
    theme_bw() +
    base_theme +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g
  
}


# Plot maps
map1 <- plot_map("Calcium")
map2 <- plot_map("Iron")
map3 <- plot_map("Vitamin A, RAE")
map4 <- plot_map("Zinc")
map5 <- plot_map("Omega-3 fatty acids")
map6 <- plot_map("Red meat")

# Plot boxplots
box1 <- plot_boxplot("Calcium")
box2 <- plot_boxplot("Iron")
box3 <- plot_boxplot("Vitamin A, RAE")
box4 <- plot_boxplot("Zinc")
box5 <- plot_boxplot("Omega-3 fatty acids")
box6 <- plot_boxplot("Red meat")

# Merge plots
g <- gridExtra::grid.arrange(map1, box1,
                             map2, box2,
                             map3, box3, 
                             map4, box4,
                             map5, box5,
                             map6, box6, ncol=2, widths=c(0.6, 0.4))

# Export
ggsave(g, filename=file.path(plotdir, "Fig3_sevs.png"), 
       width=6.5, height=10.5, units="in", dpi=600)


