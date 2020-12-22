

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
  mutate(sev_delta_cap=pmin(sev_delta, 2) %>% pmax(., -2)) %>% 
  # Rename Vitamin A
  mutate(nutrient=recode(nutrient, 
                         "Vitamin A, RAE"="Vitamin A"))

# World
world <- rnaturalearth::ne_countries("small", returnclass = "sf")


# Build data
################################################################################

# Calculate country-level means
c_avgs <- sevs %>% 
  # Remove NAs
  filter(!is.na(sev_delta)) %>%
  # Calculate country means
  group_by(nutrient, iso3, country) %>% 
  summarize(sev_delta_avg=mean(sev_delta, na.rm=T)) %>% 
  ungroup()


# Plot data
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
  c_avgs_sf <- world %>% 
    left_join(c_avgs %>% filter(nutrient==nutr_do), by=c("gu_a3"="iso3"))
  
  # Plot
  g <- ggplot(c_avgs_sf) +
    geom_sf(mapping=aes(fill=sev_delta_avg), lwd=0.2) +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    labs(title=nutr_do) +
    scale_fill_gradient2(name="ΔSEVs (%)\n(high - base)", 
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position=c(0.11,0.37),
          axis.text = element_blank(),
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)))
  g
  
}

# Plot boxplot
plot_boxplot <- function(nutrient, legend=F){
  
  # Format data
  nutr_do <- nutrient
  sdata <- sevs %>% 
    filter(nutrient==nutr_do)
  
  # Legend position
  if(legend){
    pos <- c(0.66,0.17)
  }else{
    pos <- "none"
  }
  
  # Plot data
  g <- ggplot(sdata, aes(x=age_group, y=sev_delta_cap, fill=sex)) +
    geom_boxplot(outlier.size = 0.3, lwd=0.1, outlier.color = "grey50", outlier.alpha = 0.3) +
    # Add horizontal line
    geom_hline(yintercept=0, col="grey30", linetype="dotted") +
    # Labels
    labs(x="Age group", y="ΔSEVs in 2030 (%)\n(high - base)", title="") +
    scale_fill_discrete(name="") +
    # Theme
    theme_bw() +
    base_theme +
    theme(legend.position = pos,
          legend.direction = "horizontal",
          legend.background = element_rect(fill=alpha('blue', 0)),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g
  
}

# Plot maps
map1 <- plot_map("Calcium")
map2 <- plot_map("Iron")
map3 <- plot_map("Vitamin A")
map4 <- plot_map("Zinc")
map5 <- plot_map("Omega-3 fatty acids")

# Plot boxplots
box1 <- plot_boxplot("Calcium", legend=T)
box2 <- plot_boxplot("Iron")
box3 <- plot_boxplot("Vitamin A")
box4 <- plot_boxplot("Zinc")
box5 <- plot_boxplot("Omega-3 fatty acids")

# Merge plots
g <- gridExtra::grid.arrange(map1, box1,
                             map2, box2,
                             map3, box3, 
                             map4, box4,
                             map5, box5,
                             ncol=2, widths=c(0.6, 0.4))

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_sevs.png"), 
       width=6.5, height=8.75, units="in", dpi=600)


