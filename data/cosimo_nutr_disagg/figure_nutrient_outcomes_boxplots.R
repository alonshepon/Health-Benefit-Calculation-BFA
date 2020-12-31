

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outputdir <- "data/cosimo_nutr_disagg/processed"
plotdir <- "data/cosimo_nutr_disagg/figures"

# Read data
data_orig <- readRDS(file.path(outputdir, "COSIMO_nutrient_by_scenario_cntry_with_dissagg.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # 2030
  filter(year==2030) %>% 
  # Nutrient label
  mutate(nutrient_label=paste0(nutrient, "\n(", nutrient_units, ")"))

# Key
key <- data %>% 
  select(nutrient, nutrient_units, nutrient_label) %>% 
  unique() %>% 
  mutate(intake=ifelse(nutrient=="Omega-3 fatty acids", measurements::conv_unit(300, "mg", "g"), NA))


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
  facet_wrap(~nutrient_label, scales="free_y", ncol=4) +
  geom_boxplot(outlier.size=0.8, lwd=0.3) +
  # Reference line
  geom_hline(key, mapping=aes(yintercept=intake)) +
  # Labels
  labs(x="", y="Mean daily per capita intake") +
  scale_fill_discrete(name="Scenario") +
  # Theme
  theme_bw() + base_theme
g

# Export maps
ggsave(g, filename=file.path(plotdir, "COSIMO_2030_nutrient_outcomes_boxplots_w_diversity_disagg.png"), 
       width=6.5, height=4, units="in", dpi=600)


