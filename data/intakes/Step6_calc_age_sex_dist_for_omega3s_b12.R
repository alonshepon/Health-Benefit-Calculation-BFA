

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "data/intakes/processed"
outputdir <- "data/intakes/output"
plotdir <- "data/intakes/figures"

# Read data
dists_orig <- readRDS(file.path(outputdir, "intake_distributions_for_all_cosimo_countries.Rds"))


# Format data
################################################################################

# Format distributions
dists <- dists_orig %>% 
  # Calculate means
  mutate(mean_gamma=g_shape / g_rate,
         mean_ln=exp(ln_meanlog+ln_sdlog^2/2),
         mean_best=ifelse(best_dist=="gamma", mean_gamma, mean_ln)) %>% 
  # Calculate scalars based on means
  select(country_id, country_iso3, country_name, nutrient, sex, age_group, mean_best) %>% 
  rename(intake=mean_best) %>% 
  group_by(country_id, country_iso3, country_name, nutrient) %>% 
  mutate(scalar=intake/mean(intake)) %>% 
  ungroup()


# Export data
################################################################################

# Export
saveRDS(dists, file=file.path(outputdir, "intake_distribution_age_sex_scalars.Rds"))




