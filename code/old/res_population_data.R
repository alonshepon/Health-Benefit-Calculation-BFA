
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories (outside repository)
datadir <- "/Users/cfree/Dropbox/Health Benefits calculations/Data/population_sources/" # Chris Free's computer

# Directories (in repository)
outputdir <- "output"
plotdir <- "figures"
codedir <- "code"

# Read country key
country_key_level3 <- readRDS(file.path(outputdir, "countries_level3.rds"))

# Read population data
pop_m_orig <- readxl::read_excel(file.path(datadir, "WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx"))
pop_f_orig <- readxl::read_excel(file.path(datadir, "WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx"))

# !!!! WARNING !!!!
# 1. The filter to country is based on countries_level3 -- this better be correct
# 2. This script exports data with the same name as data produced elsewhere 


# Format data
################################################################################

# Format male population data
pop_m <- pop_m_orig %>% 
  # Rename columns
  janitor::clean_names("snake") %>% 
  rename(country=region_subregion_country_or_area, year=reference_date_as_of_1_july) %>% 
  # Reduce to columns of interest
  select(country, year, x0_4:x100) %>% 
  # Reduce to countries
  filter(country %in% country_key_level3$location_name) %>% 
  # Gather population sizes
  gather(key="age_group", value="npeople", 3:ncol(.)) %>% 
  # Convert population size to numeric
  mutate(npeople=npeople %>% as.numeric()) %>% 
  # Format age group
  mutate(age_group=age_group %>% gsub("x", "", .) %>% gsub("_", "-", .),
         age_group=recode(age_group, "100"="100+")) %>% 
  # Add sex
  mutate(sex="male") %>% 
  # Arrange
  select(country, sex, age_group, npeople)
  
# Format male population data
pop_f <- pop_f_orig %>% 
  # Rename columns
  janitor::clean_names("snake") %>% 
  rename(country=region_subregion_country_or_area, year=reference_date_as_of_1_july) %>% 
  # Reduce to columns of interest
  select(country, year, x0_4:x100) %>% 
  # Reduce to countries
  filter(country %in% country_key_level3$location_name) %>% 
  # Gather population sizes
  gather(key="age_group", value="npeople", 3:ncol(.)) %>% 
  # Convert population size to numeric
  mutate(npeople=npeople %>% as.numeric()) %>% 
  # Format age group
  mutate(age_group=age_group %>% gsub("x", "", .) %>% gsub("_", "-", .),
         age_group=recode(age_group, "100"="100+")) %>% 
  # Add sex
  mutate(sex="female") %>% 
  # Arrange
  select(country, sex, age_group, npeople)

# Merge male/female population data
pop <- bind_rows(pop_m, pop_f)

# Export data
################################################################################

# Export data
saveRDS(pop, file = file.path(outputdir, "population_all_other.rds"))


