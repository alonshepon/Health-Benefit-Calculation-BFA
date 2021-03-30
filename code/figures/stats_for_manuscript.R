
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "figures"
tabledir <- "tables"

# Read problem country key
prob_key <- read.csv("data/countries_with_bug.csv", as.is=T)


# Habitual intake distributions
################################################################################

# Read intake fists and add column
intake_fits <- read.csv("data/intakes/output/habitual_nutrient_intakes_by_age_sex_13countries_distribution_fits.csv", as.is = T) %>% 
  # Add best kf
  mutate(best_ks=ifelse(best_dist=="gamma", g_ks, ln_ks))

range(intake_fits$best_ks, na.rm=T)


# COSIMO intake estimates
################################################################################

# Read data
cosimo_orig <- readRDS("data/cosimo/processed/COSIMO_2010_2030_nutr_by_scenario_cntry_food.rds")

# Calculate % of nutrition from each nutrient
stats1 <- cosimo_orig %>% 
  # 2030
  filter(year==2030) %>% 
  # Eliminate problem countries
  filter(!iso3 %in% prob_key$iso) %>% 
  # Gather
  select(-c(value_diff, value_pdiff)) %>% 
  gather(key="scenario", value="value", 9:ncol(.)) %>% 
  mutate(scenario=recode(scenario, 
                         "value_lo"="Base",
                         "value_hi"="High")) %>% 
  # Group by
  group_by(iso3, country, scenario, nutrient) %>% 
  summarize(intake_af=value[food=="Fish"],
            intake_tot=sum(value[food!="Total food"]),
            af_perc=intake_af/intake_tot*100 %>% round(.,1)) %>% 
  ungroup() %>% 
  # Summarize by nutrient
  group_by(scenario, nutrient) %>% 
  summarize(perc_max=mean(af_perc, na.rm=T)) %>% 
  spread(key="scenario", value="perc_max")

# Read data
cosimo_nutr_orig <- read.csv("data/cosimo_nutr_disagg/raw/Disaggregated_NutrientScen_FAO_FW.csv", as.is=T)

# Calculate % of nutrition from each nutrient
stats2 <- cosimo_nutr_orig %>% 
  # 2030
  filter(year==2030) %>% 
  # Eliminate problem countries
  filter(!iso3c %in% prob_key$iso) %>% 
  # Format
  select(-c(nutrient_supply)) %>% 
  rename(intake=nutrient_supply_dis) %>% 
  # Group by
  group_by(iso3c, scenario, nutrient) %>% 
  summarize(intake_af=intake[products=="Fish"],
            intake_tot=sum(intake[products!="Total food"]),
            af_perc=intake_af/intake_tot*100 %>% round(.,1)) %>% 
  ungroup() %>% 
  # Summarize by nutrient
  group_by(scenario, nutrient) %>% 
  summarize(perc_max=mean(af_perc, na.rm=T)) %>% 
  spread(key="scenario", value="perc_max")


# SEVs t-test
################################################################################

# Read SEVs
sevs_orig <- read.csv("output/2030_sevs_base_high_road_final_diversity_disagg_no_problem_countries.csv", as.is=T)

# Format
sevs <- sevs_orig %>% 
  select(nutrient, country, sex, age_group, sev_delta) %>% 
  spread(key="sex", value="sev_delta") %>% 
  mutate(difference=Females-Males)

hist(sevs$difference)

# Build key
stats3 <- sevs %>% 
  group_by(nutrient, age_group) %>% 
  summarize(n=n(), 
            pvalue=NA) %>% 
  ungroup()

# Perform t-test
for(i in 1:nrow(stats3)){
  
  # Subset data
  nutr_do <- stats3$nutrient[i]
  age_do <- stats3$age_group[i]
  sdata <- sevs %>% 
    filter(nutrient==nutr_do & age_group==age_do & difference!=0) 
  
  # Perform t-test
  ttest <- t.test(sdata$Females, sdata$Males, paired=T)
  
  # Record results
  stats3$pvalue[i] <- ttest$p.value
  
}

# Format
stats4 <- stats3 %>% 
  mutate(sig=ifelse(pvalue<=0.05, "Significantly different", "Not significantly different"))

# Plot
g <- ggplot(stats4, aes(x=age_group, y=nutrient, fill=sig)) +
  geom_raster() +
  theme_bw()
g



  