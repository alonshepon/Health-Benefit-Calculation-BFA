

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
data_orig <- readRDS(file=file.path(outputdir, "2030_ndeficient_base_high_diversity_disagg.Rds")) %>% 
  mutate(nutrient=recode(nutrient, "Omega-3 fatty acids"="DHA+EPA fatty acids"))

# World
world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")


# Build data
################################################################################

# Map stats
stats1 <- data_orig %>% 
  group_by(nutrient, iso3, country) %>% 
  summarize(npeople=sum(ndeficient_diff, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(npeople_cap=pmax(npeople, -1*1e6) %>% pmin(., 1*1e6))

hist(stats1$npeople/1e6, breaks=seq(-35,5, 0.5))
abline(v=-1)

# Raster stats
stats2 <- data_orig %>% 
  # Calculate npeople by nutrient-sex-age
  group_by(nutrient, sex, age_group) %>% 
  summarize(npeople=sum(ndeficient_diff, na.rm=T)) %>% 
  ungroup() %>% 
  # Complete set to fill NAs
  complete(nutrient, sex, age_group) %>% 
  # Recode nutrients
  mutate(nutrient=factor(nutrient,
                         levels=c("Vitamin A, RAE", "Calcium", "Zinc", "Iron", "Vitamin B-12", "DHA+EPA fatty acids"))) %>% 
  # Remove 100 group
  filter(age_group!="100+")


# Plotting functions
################################################################################

# Base theme
base_theme <- theme(axis.text=element_blank(),
                    axis.title=element_blank(),
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=5),
                    strip.text=element_blank(),
                    plot.title=element_text(size=7),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.position=c(0.11,0.35),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Function to plot map: nutrient <- "Calcium"
plot_map <- function(nutrient){
  
  # Subset data
  nutrient_do <- nutrient
  sdata <- stats1 %>% 
    filter(nutrient==nutrient_do)
  sdata_sf <- world %>% 
    left_join(sdata, by=c("gu_a3"="iso3"))
  
  # Plot data
  g <- ggplot(sdata_sf) +
    geom_sf(mapping=aes(fill=npeople_cap/1e6), lwd=0.1, color="grey30") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Labels
    labs(title=nutrient_do) +
    # Legend
    scale_fill_gradient2(name="ΔMillions of people\nwith deficiencies\n(high - base)",
                         midpoint=0, low="navy", mid="white", high="darkred") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.7, barheight = 2.5)) +
    # Theme
    theme_bw() + base_theme
  g
  
}


# Plot #1
################################################################################

# Build maps
g1 <- plot_map("DHA+EPA fatty acids")
g2 <- plot_map("Vitamin B-12")
g3 <- plot_map("Iron")
g4 <- plot_map("Zinc")
g5 <- plot_map("Calcium")
g6 <- plot_map("Vitamin A, RAE")

# Merge maps
g <- gridExtra::grid.arrange(g1, g2,
                             g3, g4,
                             g5, g6, ncol=2)

# Explort plot
ggsave(g, filename=file.path(plotdir, "FigSX_nutrient_deficiency_maps_diversity_disagg.png"), 
       width=6.5, height=4.75, units="in", dpi=600)


# Plot #2
################################################################################

# Plot raster
g7 <- ggplot(stats2, aes(x=age_group, y=nutrient, fill=npeople/1e6)) +
  facet_wrap(~sex) +
  geom_raster() +
  # Labels
  labs(x="Age group", y="") +
  scale_fill_gradient2(name="ΔMillions of people\nwith deficiencies\n(high - base)",
                       midpoint=0, low="navy", mid="white", high="darkred", na.value="grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",  barwidth = 1, barheight = 4)) +
  # Theme
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=5),
        axis.title=element_text(size=7),
        legend.text=element_text(size=5),
        legend.title=element_text(size=6),
        strip.text=element_text(size=6),
        plot.title=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "right")
g7

# Merge maps
plot2 <- gridExtra::grid.arrange(g, g7, nrow=2, heights=c(0.75,0.25))

ggsave(plot2, filename=file.path(plotdir, "FigSX_nutrient_deficiency_maps_raster_diversity_disagg.png"), 
       width=6.5, height=6.5, units="in", dpi=600)
