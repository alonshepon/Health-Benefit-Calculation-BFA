

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
data_orig <- readRDS(file.path(outputdir, "COSIMO_2010_2030_perc_nutr_diff_by_food.Rds"))

# World
world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")


# Plot data
################################################################################

# Build data
data <- data_orig %>% 
  # Reduce to totals in 2030
  filter(food_code=="TOT" & year==2030) %>% 
  # Add nutrient label
  mutate(nutrient_label=paste0(nutrient, " (", nutrient_units, ")"))

# Add
data_sf <- world %>% 
  left_join(data, by=c("gu_a3"="country_iso3")) 

# Plot data
g <- ggplot(data_sf) +
  facet_wrap(~nutrient_label, ncol=4) +
  geom_sf(mapping=aes(fill=perc_diff), lwd=0.1, color="grey30") +
  # Legend
  scale_fill_gradient2(name="% difference in 2030\nunder high and low roads", midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop out Antarctica
  coord_sf(y=c(-60, NA)) +
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
ggsave(g, filename=file.path(plotdir, "figure_perc_nutr_diff.png"), 
       width=10.5, height=6, units="in", dpi=600)














