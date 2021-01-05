
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
tabledir <- "tables"

# Read country key
data_orig <- readxl::read_excel(file.path(inputdir, "AglinkCosimo2020countriesregions.xlsx"))

# Format data
################################################################################

# Format
data <- data_orig %>% 
  filter(Group=="EUN" & Code!="CSK") %>% 
  select(Code, "Country name") %>% 
  rename(iso3=Code, country_orig="Country name") %>% 
  mutate(country=countrycode(iso3, "iso3c", "country.name")) %>% 
  arrange(country) %>% 
  select(iso3, country)

# Export
write.csv(data, file=file.path(tabledir, "TableSX_EU27_countries.csv"), row.names=F)
