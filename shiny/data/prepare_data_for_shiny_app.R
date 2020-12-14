

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories (in repository)
shinydir <- "shiny/data"

# Read age group key
age_key <- readxl::read_excel("data/age_group_key.xlsx")

# Subnational distributions
################################################################################

# Read distributions (micronutrients)
dists_orig <- readRDS(file.path("data/cosimo/processed/COSIMO_2010_2030_country_nutrient_age_sex_means_and_distributions.Rds"))

# Format distributions
dists <- dists_orig %>% 
  filter(year==2030) %>% 
  mutate(age_group=factor(age_group, levels=age_key$age_group))

# Export
saveRDS(dists, file=file.path(shinydir, "COSIMO2030_country_nutrient_age_sex_means_and_distributions.Rds"))


# EARS
################################################################################

# EARs original
ears_orig <- readxl::read_excel("data/EAR_requirements_GBDgroups.xlsx")

# Format EARs
ears <- ears_orig %>% 
  # Remove empty rows
  filter(!is.na(age_groups)) %>% 
  # Remove useless columns
  select(-'AGE GROUP') %>% 
  # Gather
  gather(key="nutrient_long", value="ear", 3:ncol(.)) %>% 
  # Convert EAR to value
  mutate(ear=as.numeric(ear)) %>% 
  # Rename
  rename(age_id=age_groups, 
         sex_id=sex_groups) %>% 
  # Format nutrient
  mutate(nutrient_long=tidyr::seperate(col="nutrient_long", sep=" ", into=c("nutrient", "")))
  # Add sex/age
  mutate(sex=ifelse(sex_id==1, "men", "women")) %>% 
  left_join(age_key) %>% 
  mutate(age_group=factor(age_group, levels=age_key$age_group)) %>% 
  # Arrange
  select(nutrient, sex_id, sex, age_id, age_group, ear, everything())

# Export
saveRDS(ears, "data/ears.Rds")
  
  
  
