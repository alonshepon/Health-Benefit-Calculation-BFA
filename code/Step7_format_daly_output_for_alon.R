

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)


# Directories
outputdir <- "output"

# Read data
dalys_orig <- read.csv(file.path(outputdir, "2030_dalys_base_high_road.csv"), as.is=T)

# Read age key
age_key <- readxl::read_excel("data/age_group_key.xlsx")

# Cause key
cause_key <- read.csv("data/cause_key.csv", as.is=T)

# Read COSIMO output
red_meat <- readRDS("data/cosimo/processed/COSIMO_2010_2030_country_red_meat_age_sex_means_and_distributions.Rds")
nutrient_orig <- readRDS("data/cosimo/processed/COSIMO_2010_2030_country_nutrient_age_sex_means_and_distributions.Rds")

# Read data
################################################################################

# Format red meat for merge
rm2030 <- red_meat %>% 
  filter(year==2030) %>% 
  # Simplify
  select(scenario, year, iso3, sex, age_group, mean_group) %>% 
  # Spread
  spread(key="scenario", value="mean_group") %>% 
  # Rename
  rename(intake_rm_base=Base, intake_rm_high='High road') %>% 
  mutate(intake_rm_diff=intake_rm_high-intake_rm_base)

# Format red meat for merge
omega2030 <- nutrient_orig %>% 
  filter(nutrient=="Omega-3 fatty acids" & year==2030) %>% 
  # Simplify
  select(scenario, year, iso3, sex, age_group, mean_group) %>% 
  # Spread
  spread(key="scenario", value="mean_group") %>% 
  # Rename
  rename(intake_omega_base=Base, intake_omega_high='High road') %>% 
  mutate(intake_omega_diff=intake_omega_high-intake_omega_base)

# Format data
dalys <- dalys_orig %>% 
  # Add age group
  left_join(age_key) %>% 
  rename(age_group_id=age_id) %>% 
  mutate(age_group=factor(age_group, levels=age_key$age_group)) %>% 
  # Format sex
  mutate(sex=ifelse(sex_id==1, "men", "women")) %>%
  # Add cause
  rename(cause_id=cause) %>% 
  left_join(cause_key) %>% 
  # Arrange
  select(country, iso3, sex_id, sex, age_group_id, age_group, cause_id, cause, everything()) %>% 
  # Add red meat intake
  left_join(rm2030) %>% 
  left_join(omega2030)


# Export
################################################################################

# Export
write.csv(dalys, file.path(outputdir, "2030_dalys_base_high_road_w_rm_omega_intakes_for_alon.csv"), row.names = F)




