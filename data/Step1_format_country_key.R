

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "data/cosimo/raw"
outputdir <- "data/cosimo/processed"
plotdir <- "data/cosimo/figures"

# Read country key
cntry_key_orig <- readxl::read_excel(file.path(inputdir, "AglinkCosimo2020countriesregions.xlsx"))


# Build country key
################################################################################

# Format country key 
cntry_key <- cntry_key_orig %>% 
  # Important columns
  select(1:6) %>% 
  # Clean names
  janitor::clean_names("snake") %>% 
  rename(iso3=code, 
         group_code=group, 
         country=country_name) %>%
  # Eliminate blank rows
  filter(!is.na(unm49)) %>% 
  # Arrange
  select(iso3, country, unm49, group_code, group_name, continent_region, everything())

# Export country key
write.csv(cntry_key, file=file.path(outputdir, "COSIMO_2020_country_key.csv"), row.names=F)
