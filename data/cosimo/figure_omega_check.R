

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
data_orig <- readRDS(file.path(outputdir, "COSIMO_2010_2030_nutr_by_scenario_cntry_food.rds"))


# Build data
################################################################################

# Fatty acids
sort(unique(data_orig$nutrient))
fatty_acids <- c("Monounsaturated fatty acids", "Polyunsaturated fatty acids", "Saturated fatty acids")
  
# Build data
data <- data_orig %>% 
  # Reshape
  select(-c(value_diff, value_pdiff)) %>% 
  gather(key="scenario", value="intake", 9:ncol(.)) %>% 
  mutate(scenario=recode(scenario, "value_lo"="Base", "value_hi"="High")) %>% 
  filter(!is.na(intake)) %>% 
  # Group by
  group_by(iso3, country, year) %>% 
  summarize(omega3_seafood=sum(intake[nutrient=="Omega-3 fatty acids" & food=="Fish"]),
            omega3_total=sum(intake[nutrient=="Omega-3 fatty acids" & food!="Fish"]),
            fatty_acid_total=sum(intake[nutrient %in% fatty_acids])) %>% 
  ungroup() %>% 
  mutate(omega3_prop=omega3_total/fatty_acid_total)

# Plot data
g1 <- ggplot(data, aes(x=year, y=omega3_seafood, group=year)) +
  geom_boxplot() +
  labs(x="Year", y="Omega-3 intake from fish (g/p/d)") +
  theme_bw()
g1

# Plot data
g2 <- ggplot(data, aes(x=year, y=omega3_prop, group=year)) +
  geom_boxplot() +
  labs(x="Year", y="Omega-3 / All fatty acids") +
  theme_bw()
g2



