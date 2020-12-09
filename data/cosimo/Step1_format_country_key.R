

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
  # Add corrected ISO3s/countryname
  mutate(country_use=countrycode(iso3, "iso3c", "country.name"),
         country_use=ifelse(is.na(country_use), country, country_use),
         iso3_use=countrycode(country_use, "country.name", "iso3c"),
         iso3_use=ifelse(is.na(iso3_use), iso3, iso3_use),
         iso3_use=ifelse(iso3_use=="PSE", iso3, iso3_use)) %>% 
  # Arrange
  select(iso3, country, iso3_use, country_use, unm49, group_code, group_name, continent_region, everything()) 

# Did any of the new
freeR::which_duplicated(cntry_key$iso3_use)
freeR::which_duplicated(cntry_key$country_use)

# Export country key
write.csv(cntry_key, file=file.path(outputdir, "COSIMO_AGLINK_2020_country_key.csv"), row.names=F)
