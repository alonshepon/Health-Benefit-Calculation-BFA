

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
shinydir <- "shiny/data"
cosimodir <- "data/cosimo/processed"
outputdir <- "output"

# Read age group key
age_key <- readxl::read_excel("data/age_group_key.xlsx")

# COSIMO data
################################################################################

# Read data
nutrients <- readRDS(file.path(cosimodir, "COSIMO_2010_2030_nutr_by_scenario_cntry_food.rds"))
food <- readRDS(file.path(cosimodir, "COSIMO_2010_2030_food_by_scenario_cntry.rds"))

# Export data
saveRDS(nutrients, file=file.path(shinydir, "COSIMO_2010_2030_nutr_by_scenario_cntry_food.rds"))
saveRDS(food, file=file.path(shinydir, "COSIMO_2010_2030_food_by_scenario_cntry.Rds"))

# Read output
sevs <- readRDS(file.path(outputdir, "2030_sevs_base_high_road_final.Rds"))
dalys <- readRDS(file.path(outputdir, "2030_dalys_base_high_road_summarized.Rds"))

# Export output
saveRDS(sevs, file=file.path(shinydir, "2030_sevs_base_high_road_final.Rds"))
saveRDS(dalys, file=file.path(shinydir, "2030_dalys_base_high_road_summarized.Rds"))


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
  separate(col="nutrient_long", sep=" ", into=c("nutrient", "sdi_group")) %>%
  mutate(nutrient=recode(nutrient, 
                         "VitA"="Vitamin A, RAE"),
         sdi_group=ifelse(is.na(sdi_group), "All", sdi_group),
         sdi_group=stringr::str_to_title(sdi_group),
         sdi_group=recode(sdi_group,
                          "5%"="Very low",
                          "10%"="Low",
                          "12%"="Middle",
                          "15%"="High",
                          "Mod"="Middle"),
         sdi_group=factor(sdi_group, levels=c("All", "High", "Middle", "Low", "Very low"))) %>% 
  # Add sex/age
  mutate(sex=ifelse(sex_id==1, "men", "women")) %>% 
  left_join(age_key) %>% 
  mutate(age_group=factor(age_group, levels=age_key$age_group)) %>% 
  # Arrange
  select(nutrient, sdi_group, sex_id, sex, age_id, age_group, ear, everything())

# Inspect data
str(ears)
table(ears$nutrient)
table(ears$sdi_group)

# Export
saveRDS(ears, "shiny/data/ears.Rds")
  
  
  
