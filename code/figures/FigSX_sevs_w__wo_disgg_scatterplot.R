

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
sevs1_orig <- readRDS(file.path(outputdir, "2030_sevs_base_high_road_final.Rds"))
sevs2_orig <- readRDS(file.path(outputdir, "2030_sevs_base_high_road_final_diversity_disagg.Rds"))

# Build data data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_blank(),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")

# Format data
sevs <- sevs1_orig %>% 
  left_join(sevs2_orig, by=c("nutrient", "country", "iso3", "sex_id", "sex", "age_id", "age_group"))

# Plot data
g <- ggplot(sevs, aes(x=sev_high.x, y=sev_high.y, color=sex)) +
  facet_wrap(~nutrient, ncol=3) +
  geom_point(size=0.5) +
  # One-to-one line
  geom_abline(slope=1) +
  # Labels
  labs(x="SEV without diversity disaggregation", y="SEV with diversity disaggregation") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_sevs_w_wout_disagg.png"), 
       width=6.5, height=4.5, units="in", dpi=600)
