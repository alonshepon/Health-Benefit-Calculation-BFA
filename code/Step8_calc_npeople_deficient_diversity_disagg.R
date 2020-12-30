

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
sevs <- readRDS(file.path(outputdir, "2030_sevs_base_high_road_final_diversity_disagg.Rds"))

# Read population projection
pop_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/nutrition/nutrient_endowment/data/population_growth/processed/UN_WPP_2019_pop_proj_by_cntry_agesex.Rds")

# Build data
################################################################################

# Format population data
pop <- pop_orig %>% 
  filter(year==2030) %>% 
  select(iso3_use, sex, age_range, pop_size_50perc) %>% 
  rename(iso3=iso3_use, age_group=age_range, npeople=pop_size_50perc) %>% 
  mutate(sex=recode(sex, "male"="Males", "female"="Females"))

# Build data
data <- sevs %>% 
  # Add population size
  left_join(pop) %>% 
  # Arrange
  select(country:age_group, npeople, nutrient, everything()) %>% 
  # Calculate
  mutate(ndeficient_base=npeople*sev_base,
         ndeficient_high=npeople*sev_high,
         ndeficient_diff=ndeficient_high-ndeficient_base)

# Export data
saveRDS(data, file=file.path(outputdir, "2030_ndeficient_base_high_diversity_disagg.Rds"))

