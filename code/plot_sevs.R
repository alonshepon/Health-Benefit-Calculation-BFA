

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
sevs_mn <- read.csv(file.path(outputdir, "2030_sevs_base_high_road_micronutrients.csv"), as.is=T)
sevs_omega <- read.csv(file.path(outputdir, "2030_sevs_base_high_road_omega3s.csv"), as.is=T)
sevs_meat <- read.csv(file.path(outputdir, "2030_sevs_base_high_road_meat.csv"), as.is=T)
  

# Build data
################################################################################

# Build data
data <- bind_rows(sevs_mn, sevs_omega, sevs_meat) %>% 
  # Format sex
  mutate(sex=ifelse(sex_id==1, "Males", "Females")) %>% 
  # Format age group
  mutate(age_group=as.character(age_id),
         age_group=recode_factor(age_group,
                                 "5"="0-4",
                                 "6"="5-9",
                                 "7"="10-14",
                                 "8"="15-19",
                                 "9"="20-24",
                                 "10"="25-29",
                                 "11"="30-34",
                                 "12"="35-39",
                                 "13"="40-44",
                                 "14"="45-49",
                                 "15"="50-54",
                                 "16"="55-59",
                                 "17"="60-64",
                                 "18"="65-69",
                                 "19"="70-74",
                                 "20"="75-79",
                                 "30"="80-84",
                                 "31"="85-89",
                                 "32"="90-94",
                                 "33"="95-99"))  %>% 
  # Arrange
  select(nutrient, country, iso3, sex_id, sex, age_id, age_group, everything())



# Build data
################################################################################

# Plot data
g <- ggplot(data %>% filter(iso3=="FRA"), aes(x=age_group, y=nutrient, fill=sev_delta)) +
  facet_wrap(~sex) +
  geom_raster() +
  # Labels
  labs(x="Age group", y="") +
  # Legend
  scale_fill_gradient2(name="DeltaSEV", low="darkred", high="navy", mid="grey80", midpoint=0) +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g





