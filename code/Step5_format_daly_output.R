

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

# Read data
################################################################################

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
  select(country, iso3, sex_id, sex, age_group_id, age_group, cause_id, cause, everything())

# Summarize DALYs
dalys_sum <- dalys %>% 
  group_by(country, iso3, sex_id, sex, age_group_id, age_group) %>%
  summarize(DALY2030_br=sum(DALY2030_all, na.rm = TRUE),
            DALY2030_hr=sum(DALY2030_all_hr, na.rm = TRUE))

# Export
################################################################################

# Export
saveRDS(dalys, file.path(outputdir, "2030_dalys_base_high_road_cleaned.Rds"))
saveRDS(dalys_sum, file.path(outputdir, "2030_dalys_base_high_road_summarized.Rds"))



