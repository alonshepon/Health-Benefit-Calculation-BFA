

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


# Plot data
################################################################################

# Format data
data <- data_orig %>% 
  filter(year==2030) %>% 
  select(-intake_diff) %>% 
  gather(key="type", value="intake", 7:8) %>% 
  mutate(scenario=recode(scenario, "High road"="High")) %>% 
  mutate(type=recode_factor(type, 
                            "intake_orig"="Original (GND)",
                            "intake"="Diversity disaggregation"),
         nutrient_label=paste0(nutrient, "\n(", nutrient_units, ")"))
  
# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_blank(),
                   strip.text=element_text(size=7),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")


# Plot data
g <- ggplot(data, aes(x=scenario, y=intake)) +
  facet_grid(nutrient_label~type, scales='free_y') +
  geom_boxplot(outlier.size=0.5, lwd=0.2) +
  # Labels
  labs(x="Scenario", y="Mean daily per capita intake") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_nutrient_boxplots_w_wo_disagg.png"), 
       width=4.5, height=6.5, units="in", dpi=600)




