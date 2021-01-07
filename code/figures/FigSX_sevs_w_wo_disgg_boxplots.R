

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
sevs1_orig <- readRDS(file.path(outputdir, "2030_sevs_base_high_road_final.Rds")) %>% 
  mutate(type="Original (GND)")
sevs2_orig <- readRDS(file.path(outputdir, "2030_sevs_base_high_road_final_diversity_disagg.Rds")) %>% 
  mutate(type="Diversity disaggregation")

# Build data
################################################################################

# Format data
sevs <- bind_rows(sevs1_orig, sevs2_orig) %>% 
  select(-sev_delta) %>% 
  select(nutrient:age_group, type, everything()) %>% 
  gather(key="scenario", value="sev", 9:10) %>% 
  mutate(scenario=recode(scenario, "sev_base"="Base", "sev_high"="High")) %>% 
  mutate(type=factor(type, levels=c("Original (GND)", "Diversity disaggregation")))


# Build data
################################################################################

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
g <- ggplot(sevs, aes(x=scenario, y=sev)) +
  facet_grid(nutrient~type) +
  geom_boxplot(outlier.size=0.3, lwd=0.5, outlier.color = "grey30") +
  # Labels
  labs(x="Scenario", y="Summary exposure value (SEV)") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_sevs_w_wout_disagg_boxplot.png"), 
       width=4.5, height=6.5, units="in", dpi=600)
