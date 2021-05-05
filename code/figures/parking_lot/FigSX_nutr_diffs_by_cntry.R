

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
data_orig <- readRDS(file.path(outputdir, "COSIMO_nutrient_by_scenario_cntry_with_dissagg.Rds"))

# World
world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")


# Plot data
################################################################################

# Nutrients to plot
nutrients_plot <- c("Iron", "Zinc", "Calcium", 
                    "Vitamin A", "Vitamin B-12", "Omega-3 fatty acids")

# Data
data <- data_orig %>% 
  # Reduce to 2030
  filter(year==2030) %>% 
  # Cap values
  group_by(nutrient) %>% 
  mutate(ymin=quantile(intake_diff, 0.05),
         ymax=quantile(intake_diff, 0.95),
         intake_diff_cap=pmin(intake_diff, ymax) %>% pmax(., ymin)) %>%  
  ungroup() %>% 
  select(-c(ymin, ymax))
write.csv(data, file=file.path(outputdir, "2030_nutrient_intake_by_scenario_cntry_w_disagg.csv"), row.names = T)

# Expand data to match map countries
data_expanded <- world %>% 
  sf::st_drop_geometry() %>% 
  # Reduce
  select(gu_a3) %>% 
  # Add nutrient info
  left_join(data, by=c("gu_a3"="iso3")) %>% 
  # Expand
  complete(gu_a3, nutrient) %>% 
  # Remove missing data
  filter(!is.na(nutrient))

# Add expanded data to SF
data_sf <- world %>% 
  left_join(data_expanded, by="gu_a3")

# Setup theme
my_theme <-  theme(axis.text=element_blank(),
                   axis.title=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_blank(),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")

# Function to plot map
# nutrient <- "Omega-3 fatty acids"; scenario <- "High"
plot_map <- function(nutrient, scenario){
  
  # Subset data
  nutr_do <- nutrient
  scen_do <- scenario
  sdata <- data_sf %>% 
    filter(scenario==scen_do & nutrient==nutr_do)
  
  # Build nutrient label
  nutr_units <- unique(sdata$units)
  nutr_label <- paste0(nutr_do, " (", nutr_units, ")")
  
  # Plot data
  g <- ggplot(sdata) +
    geom_sf(mapping=aes(fill=intake_diff_cap), lwd=0.2, color="grey30") +
    # Labels
    labs(title=nutr_label) +
    # Legend
    scale_fill_gradient2(name="ΔDaily per capita nutrient intake\n(disaggregation - GND values)", 
                         low="darkred", high="navy", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) + # barwidth = 0.8, barheight = 3
    # Theme
    theme_bw() + my_theme
  g
  
}

# Plot maps
g1 <- plot_map(nutrient="Omega-3 fatty acids", scenario = "High road")
g2 <- plot_map(nutrient="Vitamin B-12", scenario = "High road")
g3 <- plot_map(nutrient="Iron", scenario = "High road")
g4 <- plot_map(nutrient="Zinc", scenario = "High road")
g5 <- plot_map(nutrient="Calcium", scenario = "High road")
g6 <- plot_map(nutrient="Vitamin A, RAE", scenario = "High road")

# Merge plots
g <- gridExtra::grid.arrange(g1, g2,
                             g3, g4,
                             g5, g6,
                             ncol=2)

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_nutr_diffs_w_disagg.png"), 
       width=6.5, height=7, units="in", dpi=600)

