

# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "figures"
tabledir <- "tables"

# Read problem country key
prob_key <- read.csv("data/countries_with_bug.csv", as.is=T)


# Food data
################################################################################

# Read data
food_orig <- readRDS("data/cosimo/processed/COSIMO_2010_2030_food_by_scenario_cntry.Rds")

# Format foods
food <- food_orig %>% 
  # Eliminate problem countries
  filter(!iso3 %in% prob_key$iso) %>% 
  # Reduce to 2030 values
  filter(year==2030) %>% 
  # Remove useless columns
  select(-c(year, food_code)) %>%
  # Rename
  rename(base=value_lo, high=value_hi, diff=value_diff, pdiff=value_diff_perc) %>% 
  # Format foods
  mutate(food=stringr::str_to_sentence(food))

# Export
write.csv(food, file=file.path(tabledir, "AppendixA_food_results_table.csv"), row.names=F)


# Nutrient data
################################################################################

# Read data
nutrient_orig <- readRDS("data/cosimo_nutr_disagg/processed/COSIMO_nutrient_by_scenario_cntry_with_dissagg.Rds")

# Format nutrient
nutrient <- nutrient_orig %>% 
  # Eliminate problem countries
  filter(!iso3 %in% prob_key$iso) %>% 
  # Reduce to 2030 values
  filter(year==2030) %>% 
  # Eliminate useless columns
  select(-c(year, intake_orig, intake_diff)) %>% 
  # Formats nutrients
  mutate(nutrient=recode(nutrient, 
                         "Vitamin B-12"="Vitamin B12",
                         "Omega-3 fatty acids"="DHA+EPA fatty acids")) %>% 
  mutate(nutrient_label=paste0(nutrient, " (", nutrient_units, ")")) %>% 
  select(-c(nutrient, nutrient_units)) %>% 
  # Spread
  spread(key="scenario", value="intake") %>% 
  rename(base="Base", high="High road") %>% 
  # Add differences
  mutate(diff=high-base,
         pdiff=(high-base)/base*100)

# Export
write.csv(nutrient, file=file.path(tabledir, "AppendixB_nutrient_results_table.csv"), row.names=F)


# SEVs data
################################################################################
  
# Read data
sevs_orig <- readRDS("output/2030_sevs_base_high_road_final_diversity_disagg.Rds") 

# Format data
sevs <- sevs_orig %>% 
  # Rename
  mutate(nutrient=recode(nutrient, 
                         "Vitamin A, RAE"="Vitamin A",
                         "Omega-3 fatty acids"="DHA+EPA fatty acids")) %>% 
  # Arrange
  select(iso3, country, nutrient, sex, age_group, sev_base, sev_high, sev_delta) %>% 
  # Group by
  filter(!is.na(sev_delta)) %>% 
  group_by(iso3, country, nutrient) %>% 
  summarize(sev_base=mean(sev_base, na.rm=T),
            sev_high=mean(sev_high, na.rm=T),
            sev_delta=mean(sev_delta, na.rm=T))

# Export
write.csv(sevs, file=file.path(tabledir, "AppendixC_sevs_results_table.csv"), row.names=F)


