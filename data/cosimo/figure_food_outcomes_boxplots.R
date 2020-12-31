

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


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # 2030
  filter(year==2030) %>% 
  # Reshape
  select(-c(value_diff, value_diff_perc)) %>% 
  gather(key="scenario", value="intake", 6:7) %>% 
  mutate(scenario=recode(scenario, "value_lo"="Base", "value_hi"="High"))


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=5),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=7),
                    strip.text=element_text(size=7),
                    plot.title=element_text(size=7),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    legend.position = "bottom")

# Plot data
g <- ggplot(data, aes(x=scenario, y=intake, fill=scenario)) +
  facet_wrap(~food, scales="free_y", ncol=4) +
  geom_boxplot(outlier.size=0.8, lwd=0.3) +
  # Labels
  labs(x="", y="Mean daily per capita intake (g/p/d)") +
  scale_fill_discrete(name="Scenario") +
  # Theme
  theme_bw() + base_theme
g

# Export maps
ggsave(g, filename=file.path(plotdir, "COSIMO_2030_food_outcomes_boxplots.png"), 
       width=6.5, height=6.5, units="in", dpi=600)


