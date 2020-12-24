

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outputdir <- "data/cosimo/old/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outputdir, "COSIMO_2010_2030_perc_nutr_diff_by_food.Rds"))

# World
world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")


# Plot data
################################################################################


# Nutrients to plot
nutrients_plot <- c("Iron", "Zinc", "Calcium", 
                    "Vitamin A", "Vitamin B-12", "Omega-3 fatty acids")

# Data
data <- data_orig %>% 
  filter(year==2030 & food=="Total Diet" & nutrient %in% nutrients_plot)

# Expand data to match map countries
data_expanded <- world %>% 
  sf::st_drop_geometry() %>% 
  # Reduce
  select(gu_a3) %>% 
  # Add nutrient info
  left_join(data, by=c("gu_a3"="country_iso3")) %>% 
  # Expand
  complete(gu_a3, nutrient) %>% 
  # Remove missing data
  filter(!is.na(nutrient))

# Add expanded data to SF
data_sf <- world %>% 
  left_join(data_expanded, by="gu_a3")


# Plot data
################################################################################

# Plot data
g <- ggplot(data_sf) +
  facet_wrap(~nutrient, ncol=3) +
  geom_sf(mapping=aes(fill=perc_diff), lwd=0.1, color="grey30") +
  # Legend
  scale_fill_gradient2(name="% difference in 2030 intakes\n(high vs. base)", midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
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
ggsave(g, filename=file.path(plotdir, "Fig3_nutrient_diffs.png"), 
       width=6.5, height=3, units="in", dpi=600)



