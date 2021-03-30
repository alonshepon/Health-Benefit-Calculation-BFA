
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
cntry_orig <- read.csv(file.path(outputdir, "COSIMO_country_key.csv"), as.is=T)

# Read problem country key
prob_key <- read.csv("data/countries_with_bug.csv", as.is=T)

# World
world <- rnaturalearth::ne_countries(scale="large")

# Format data
################################################################################

# Country key
cntry_key <- cntry_orig %>% 
  filter