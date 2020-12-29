
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories (outside repository)
genusdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/nutrition/nutrient_endowment/data/genus/processed/" # Chris Free's computer

# Directories (inside repository)
inputdir <- "data/cosimo/raw"
outputdir <- "data/cosimo/processed"
plotdir <- "data/cosimo/figures"

# Read COSIMO output
data_orig <- readRDS(file.path(outputdir, "COSIMO_2010_2030_food_by_scenario_cntry.rds"))


# Setup keys
################################################################################

# Read SPADE-derived scalars
spade_scalars <- readRDS(file.path("data/intakes/output/intake_distribution_age_sex_scalars.Rds")) %>% 
  # Reduce to red meat
  filter(nutrient=="Red meat") %>% 
  # Format age/sex columns
  mutate(sex=recode(sex, "men"="Males", "women"="Females"),
         age_group=as.character(age_group)) %>% 
  # Reduce
  select(country_iso3, sex, age_group, scalar)

# Build country-sex-age key
cntry_sex_age_key <- expand.grid(iso3=sort(unique(data_orig$iso3)),
                                 sex=c("Males", "Females"),
                                 age_group=sort(unique(spade_scalars$age_group))) %>% 
  arrange(iso3, sex, age_group)


# Build data
################################################################################

# Red meat food groups
sort(unique(data_orig$food))
red_meat_foods <- c("Bovine", "Ovine", "Pork")

# Build data
data <- data_orig %>% 
  # Reduce to red meats
  filter(food %in% red_meat_foods) %>% 
  # Sum red meat intake by country
  group_by(country, iso3, year) %>% 
  summarize(value_lo=sum(value_lo, na.rm=T),
            value_hi=sum(value_hi, na.rm=T)) %>% 
  ungroup() %>% 
  # Gather
  gather(key="scenario", value="mean_cntry", 4:ncol(.)) %>% 
  mutate(scenario=recode(scenario, 
                         "value_lo"="Base", 
                         "value_hi"="High road")) %>% 
  # Add sex-age groups
  left_join(cntry_sex_age_key, by="iso3") %>% 
  # Add scalar
  left_join(spade_scalars, by=c("iso3"="country_iso3", "sex", "age_group")) %>% 
  # Add group intake
  mutate(mean_group=mean_cntry*scalar) %>% 
  # Arrange
  select(scenario, year,
         country, iso3, sex, age_group, scalar, mean_cntry, mean_group, everything()) %>% 
  # Remove countries without scalars
  filter(!is.na(mean_group))

# Inspect
str(data)
freeR::complete(data)

# Check
# The countries missing scalars are not in the COSIMO output data (so filtered out above)
check <- data %>% 
  filter(is.na(scalar))
sort(unique(check$country))


# Add the distribution fits
################################################################################

# Read fits
dists <- readRDS("data/intakes/output/intake_distributions_for_all_cosimo_countries.Rds") %>% 
  filter(nutrient=="Red meat")

# Add distribution fits
data1 <- data %>% 
  # Recode sex for merge
  mutate(sex=recode(sex, "Females"="women", "Males"="men")) %>% 
  # Add distribution fits
  left_join(dists, by=c("iso3"="country_iso3", "sex", "age_group")) %>% 
  # Add means and differences
  mutate(g_mean=g_shape/g_rate,
         g_mean_diff=mean_group-g_mean) %>% 
  mutate(ln_mean=exp(ln_meanlog + ln_sdlog^2/2),
         ln_mean_diff=mean_group-ln_mean)

freeR::complete(data1)


# Export
################################################################################

# Export
saveRDS(data1, file=file.path(outputdir, "COSIMO_2010_2030_country_red_meat_age_sex_means_and_distributions.Rds"))




