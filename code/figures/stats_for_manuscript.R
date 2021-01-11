
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "figures"
tabledir <- "tables"



# Habitual intake distributions
################################################################################

# Read intake fists and add column
intake_fits <- read.csv("data/intakes/output/habitual_nutrient_intakes_by_age_sex_13countries_distribution_fits.csv", as.is = T) %>% 
  # Add best kf
  mutate(best_ks=ifelse(best_dist=="gamma", g_ks, ln_ks))

range(intake_fits$best_ks, na.rm=T)


# COSIMO intake estimates
################################################################################

# Read data
cosimo_orig <- readRDS("data/cosimo/processed/COSIMO_2010_2030_nutr_by_scenario_cntry_food.rds")

# Calculate % of nutrition from each nutrient
stats <- cosimo_orig %>% 
  # 2030
  filter(year==2030) %>% 
  # Gather
  select(-c(value_diff, value_pdiff)) %>% 
  gather(key="scenario", value="value", 9:ncol(.)) %>% 
  mutate(scenario=recode(scenario, 
                         "value_lo"="Base",
                         "value_hi"="High")) %>% 
  # Group by
  group_by(iso3, country, scenario, nutrient) %>% 
  summarize(intake_af=value[food=="Fish"],
            intake_tot=sum(value),
            af_perc=intake_af/intake_tot*100 %>% round(.,1)) %>% 
  ungroup() %>% 
  # Get rid of the 50s which seem to be errors
  filter(af_perc!=50) %>% 
  # Summarize by nutrient
  group_by(scenario, nutrient) %>% 
  summarize(perc_min=min(af_perc, na.rm=T),
            perc_max=max(af_perc, na.rm=T))
  