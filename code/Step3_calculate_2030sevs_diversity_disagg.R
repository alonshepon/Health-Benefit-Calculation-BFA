

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories (in repository)
outputdir <- "output"
plotdir <- "figures"
codedir <- "code"

# Read other data (in repository)
omega_N_raw_2019 <- openxlsx::read.xlsx('data/omega_RR_2019.xlsx')
red_meat_raw_2019 <- openxlsx::read.xlsx('data/meat_RR_2019.xlsx')
EAR_requirements <- openxlsx::read.xlsx('data/EAR_requirements_GBDgroups.xlsx')

# Read distributions (micronutrients)
dists <- readRDS(file.path("data/cosimo_nutr_disagg/processed/COSIMO_2010_2030_country_nutrient_age_sex_means_and_distributions_with_disagg.Rds"))

# Read distributions (red meat)
# dists_meat <- readRDS(file=file.path("data/cosimo/processed/COSIMO_2010_2030_country_red_meat_age_sex_means_and_distributions.Rds"))

# Read HDI/SDI key
sdi_hdi_key <- readRDS("data/cosimo/processed/COSIMO_country_key_with_SDI_HDI_info.rds") %>% 
  select(iso3, sdi, sdi_group, hdi)

# Source helpher functions
source("code/RR_functions.R")


# Notes
################################################################################

# AGE GROUP CODES
# 5 1-4 years 
# 6 5-9 years 
# 7 10-14 years
# 8 15-19 years
# 9 20-24 years
# 10 25-29 years
# 11 30-34 years
# 12 35-39 years
# 13 40-44 years 
# 14 45-49 years 
# 15 50-54 years 
# 16 55-59 years 
# 17 60-64 years 
# 18 65-69 years 
# 19 70-74 years 
# 20 75-79 years
# 30 80-84 years
# 31 85-89 years
# 32 90-94 years
# 33 95-99 years

# CAUSE CODES
# 409 noncommunicable diseases
# 295 Communicable, maternal, neonatal, and nutritional diseases
# 294 all cause

# CODES
# 121 low seafood
# 97 zinc
# 96 vitamin A
# 95 iron
# 117 processed meat
# 116 red meat

# CAUSE
# 429 breast cancer 
# 441 Colon and rectum cancer 
# 493 Ischemic heart disease 
# 495 Ischemic stroke
# 496 Intracerebral hemorrhage 
# 497 Subarachnoid hemorrhage 
# 976 Diabetes mellitus type 2  

# Build data
################################################################################

# Meat causes
cause_meat <- c(429,441,493,495,496,497,976) 
cause_meat_no_ischemic <- c(429,441,495,496,497,976)



# Format intake distributions
##########################################################################################

# Merge data
dists2030 <- dists %>% 
  # Reduce to 2030
  filter(year==2030) %>% 
  # Reduce to distributions with a mean
  filter(!is.na(mean_group)) %>% 
  # Simplify 
  select(country, iso3, nutrient, sex, age_group, scenario, mean_group, best_dist, 
         g_shape, g_rate, g_mean, g_mean_diff,
         ln_meanlog, ln_sdlog, ln_mean, ln_mean_diff) %>% 
  # Add age id and sex id
  mutate(sex_id=recode(sex, 
                       "Males"=1,
                       "Females"=2) %>% as.numeric(),
         age_id=recode(age_group,
                       "0-4"="5",
                       "5-9"="6",
                       "10-14"="7",
                       "15-19"="8",
                       "20-24"="9",
                       "25-29"="10",
                       "30-34"="11",
                       "35-39"="12",
                       "40-44"="13",
                       "45-49"="14",
                       "50-54"="15",
                       "55-59"="16",
                       "60-64"="17",
                       "65-69"="18",
                       "70-74"="19",
                       "75-79"="20",
                       "80-84"="30",
                       "85-89"="31",
                       "90-94"="32",
                       "95-99"="33") %>% as.numeric()) %>% 
  select(-c(sex, age_group))

# Merge data
# dists2030_meat <- dists_meat %>% 
#   # Reduce to 2030
#   filter(year==2030) %>%
#   # Reduce to distributions with a mean
#   filter(!is.na(mean_group)) %>% 
#   # Simplify 
#   select(country, iso3, nutrient, sex, age_group, scenario, mean_group, best_dist, 
#          g_shape, g_rate, g_mean, g_mean_diff,
#          ln_meanlog, ln_sdlog, ln_mean, ln_mean_diff) %>% 
#   # Add age id and sex id
#   mutate(sex_id=recode(sex, 
#                        "men"=1,
#                        "women"=2) %>% as.numeric(),
#          age_id=recode(age_group,
#                        "0-4"="5",
#                        "5-9"="6",
#                        "10-14"="7",
#                        "15-19"="8",
#                        "20-24"="9",
#                        "25-29"="10",
#                        "30-34"="11",
#                        "35-39"="12",
#                        "40-44"="13",
#                        "45-49"="14",
#                        "50-54"="15",
#                        "55-59"="16",
#                        "60-64"="17",
#                        "65-69"="18",
#                        "70-74"="19",
#                        "75-79"="20",
#                        "80-84"="30",
#                        "85-89"="31",
#                        "90-94"="32",
#                        "95-99"="33") %>% as.numeric()) %>% 
#   select(-c(sex, age_group))



# Calculate changes in summary exposure values (SEVs) -- micronutrients
##########################################################################################

# Nutrients to calculate SEVS for
nutr_sevs <- c("Zinc", "Iron", "Calcium", "Vitamin A, RAE", "Vitamin B-12")

# Build data required for micronutrient SEV calculations
data_sev_mn <- dists2030 %>% 
  # Reduce to nutrients of interest
  filter(nutrient %in% nutr_sevs) %>% 
  # Add SDI group
  left_join(sdi_hdi_key, by=c("iso3")) %>% 
  # Reduce to age groups with required data
  filter(age_id>=5)

# Loop through micronutrients to calculate SEVs for
x <- 141
# sevs_micronutrients <- purrr::map_df(1:nrow(data_sev_mn), function(x){
for(x in 1:nrow(data_sev_mn)){
  
  # Parameters
  print(x)
  scenario_do <- data_sev_mn$scenario[x]
  iso3_do <- data_sev_mn$iso3[x]
  nutr_do <- data_sev_mn$nutrient[x]
  sdi_group_do <- data_sev_mn$sdi_group[x]
  age_id_do <- data_sev_mn$age_id[x]
  sex_id_do <- data_sev_mn$sex_id[x]
  best_dist <- data_sev_mn$best_dist[x]
  
  # If gamma distribution....
  if(best_dist=="gamma"){
    shape <- data_sev_mn$g_shape[x]
    rate <- data_sev_mn$g_rate[x]
    x_shift <- data_sev_mn$g_mean_diff[x]
    intake_function <- function(x){y <- dgamma(x-x_shift, shape=shape, rate=rate)}
    val_hi <- qgamma(p=0.9999, shape = shape, rate=rate) + x_shift
    val_lo <- qgamma(p=0.0001, shape = shape, rate=rate) + x_shift
  }
  
  # If lognormal distribution...
  if(best_dist=="log-normal"){
    mu <- data_sev_mn$ln_meanlog[x]
    sigma <- data_sev_mn$ln_sdlog[x]
    x_shift <- data_sev_mn$ln_mean_diff[x]
    intake_function <- function(x){y <- dlnorm(x-x_shift, meanlog=mu, sdlog=sigma)}
    val_hi <- qlnorm(p=0.9999, meanlog=mu, sdlog=sigma) + x_shift
    val_lo <- qlnorm(p=0.0001, meanlog=mu, sdlog=sigma) + x_shift
  }
  
  # Plot dists 
  if(F){
    x <- seq(-200, 2000, by=1)
    y <- intake_function(x=x)
    plot(x, y, type="l")
    abline(v=c(val_lo, val_hi), lty=1)
  }
  
  # Calculate SEV
  sev <- try(micronutrient_SEV(Intake=intake_function, 
                               age=age_id_do, 
                               sex=sex_id_do,
                               nutrient=nutr_do, 
                               country_SDIgroup=sdi_group_do, 
                               EAR_requirements,
                               val_lo=val_lo,
                               val_hi=val_hi))
  
  # Record based on try()
  if(inherits(sev, "try-error")){
    sev_out <- NA
  }else{
    sev_out <- sev
  }
  
  # Record
  data_sev_mn$sev[x] <- sev_out
  
}

# Format SEVs
sev_mn_final <- data_sev_mn %>% 
  select(scenario, nutrient, country, iso3, sex_id, age_id, sev) %>% 
  spread(key="scenario", value="sev") %>% 
  rename(sev_high="High road", sev_base="Base") %>% 
  mutate(sev_delta=sev_high-sev_base)

# Export
write.csv(sev_mn_final, file=file.path(outputdir, "2030_sevs_base_high_road_micronutrients_diversity_disagg.csv"), row.names=F)


# Calculate changes in summary exposure values (SEVs) -- omega-3 fatty acids
##########################################################################################

# Build data required for micronutrient SEV calculations
data_sev_omega <- dists2030 %>% 
  # Reduce to nutrients of interest
  filter(nutrient %in% "Omega-3 fatty acids") %>% 
  # Reduce to age groups with required data
  filter(age_id>=10)

# Loop through micronutrients to calculate SEVs for
x <- 1
for(x in 1:nrow(data_sev_omega)){
  
  # Parameters
  print(x)
  scenario_do <- data_sev_omega$scenario[x]
  iso3_do <- data_sev_omega$iso3[x]
  nutr_do <- data_sev_omega$nutrient[x]
  age_id_do <- data_sev_omega$age_id[x]
  sex_id_do <- data_sev_omega$sex_id[x]
  best_dist <- data_sev_omega$best_dist[x]

  # If gamma distribution....
  if(best_dist=="gamma"){
    shape <- data_sev_omega$g_shape[x]
    rate <- data_sev_omega$g_rate[x]
    x_shift <- data_sev_omega$g_mean_diff[x]
    intake_function <- function(x){y <- dgamma(x-x_shift, shape=shape, rate=rate)}
    val_hi <- qgamma(p=0.9999, shape = shape, rate=rate) + x_shift
    val_lo <- qgamma(p=0.0001, shape = shape, rate=rate) + x_shift
  }
  
  # If lognormal distribution...
  if(best_dist=="log-normal"){
    mu <- data_sev_omega$ln_meanlog[x]
    sigma <- data_sev_omega$ln_sdlog[x]
    x_shift <- data_sev_omega$ln_mean_diff[x]
    intake_function <- function(x){y <- dlnorm(x-x_shift, meanlog=mu, sdlog=sigma)}
    val_hi <- qlnorm(p=0.9999, meanlog=mu, sdlog=sigma) + x_shift
    val_lo <- qlnorm(p=0.0001, meanlog=mu, sdlog=sigma) + x_shift
  }
  
  # Plot dists 
  if(F){
    x <- seq(-1,5,by=0.01)
    y <- intake_function(x=x)
    plot(x, y, type="l")
    abline(v=c(val_lo, val_hi), lty=1)
  }
  
  # Calculate SEV
  sev <- try(omega_n3_SEV(Intake=intake_function,
                          age=age_id_do, 
                          val_hi=val_hi,
                          val_lo=val_lo,
                          omega_N_raw_2019,
                          omega_n3_RR))
  
  # Record based on try()
  if(inherits(sev, "try-error")){
    sev_out <- NA
  }else{
    sev_out <- sev
  }
  
  # Record
  data_sev_omega$sev[x] <- sev_out
  
}

# Format SEVs
sev_omega_final <- data_sev_omega %>% 
  mutate(nutrient="Omega-3 fatty acids") %>% 
  select(scenario, nutrient, country, iso3, sex_id, age_id, sev) %>% 
  spread(key="scenario", value="sev") %>% 
  rename(sev_high="High road", sev_base="Base") %>% 
  mutate(sev_delta=sev_high-sev_base)

# Export
write.csv(sev_omega_final, file=file.path(outputdir, "2030_sevs_base_high_road_omega3s_diversity_disagg.csv"), row.names=F)


# Calculate changes in summary exposure values (SEVs) -- red meat
##########################################################################################

# 
# # Build data required for micronutrient SEV calculations
# data_sev_meat_1cause <- dists2030_meat %>% 
#   # Reduce to age groups with required data
#   filter(age_id>=10)
# 
# # Expand for each each
# data_sev_meat <- purrr::map_df(cause_meat, function(x){
#   
#   df <- data_sev_meat_1cause %>% 
#     mutate(cause=x)
#   
# })
# 
# data_sev_meat <- data_sev_meat %>% 
#   arrange(country, scenario, sex_id, age_id)
# 
# # Loop through
# for(x in 1:nrow(data_sev_meat)){
#   
#   # Parameters
#   if(x %in% seq(100,80000,100)){print(x)}
#   scenario_do <- data_sev_meat$scenario[x]
#   iso3_do <- data_sev_meat$iso3[x]
#   nutr_do <- data_sev_meat$nutrient[x]
#   age_id_do <- data_sev_meat$age_id[x]
#   sex_id_do <- data_sev_meat$sex_id[x]
#   cause_do <- data_sev_meat$cause[x]
#   best_dist <- data_sev_meat$best_dist[x]
#   
#   # If gamma distribution....
#   if(best_dist=="gamma"){
#     shape <- data_sev_meat$g_shape[x]
#     rate <- data_sev_meat$g_rate[x]
#     x_shift <- data_sev_meat$g_mean_diff[x]
#     intake_function <- function(x){y <- dgamma(x-x_shift, shape=shape, rate=rate)}
#     val_hi <- qgamma(p=0.9999, shape = shape, rate=rate) + x_shift
#     val_lo <- qgamma(p=0.0001, shape = shape, rate=rate) + x_shift
#   }
#   
#   # If lognormal distribution...
#   if(best_dist=="log-normal"){
#     mu <- data_sev_meat$ln_meanlog[x]
#     sigma <- data_sev_meat$ln_sdlog[x]
#     x_shift <- data_sev_meat$ln_mean_diff[x]
#     intake_function <- function(x){y <- dlnorm(x-x_shift, meanlog=mu, sdlog=sigma)}
#     val_hi <- qlnorm(p=0.9999, meanlog=mu, sdlog=sigma) + x_shift
#     val_lo <- qlnorm(p=0.0001, meanlog=mu, sdlog=sigma) + x_shift
#   }
#   
#   # Plot dists 
#   if(F){
#     x_vals <- seq(-100,500,by=1)
#     y <- intake_function(x=x_vals)
#     plot(x_vals, y, type="l")
#     abline(v=c(val_lo, val_hi), lty=1)
#   }
#   
#   # Calculate SEV 
#   sev <- try(red_meat_SEV(Intake=intake_function,
#                           age=age_id_do,
#                           val_hi=val_hi,
#                           val_lo=val_lo,
#                           meat_outcome=cause_do,
#                           red_meat_2019=red_meat_raw_2019,
#                           red_meat_RR))
#   
#   # Record based on try()
#   if(inherits(sev, "try-error")){
#     sev_out <- NA
#   }else{
#     sev_out <- sev
#   }
#   
#   # Record
#   data_sev_meat$sev[x] <- sev_out
#   
# }
# 
# # Format SEVs
# sev_meat_final <- data_sev_meat %>% 
#   mutate(nutrient="Red meat") %>% 
#   select(scenario, nutrient, country, iso3, sex_id, age_id, cause, sev) %>% 
#   spread(key="scenario", value="sev") %>% 
#   rename(sev_high="High road", sev_base="Base") %>% 
#   mutate(sev_delta=sev_high-sev_base)
# 
# # Export
# write.csv(sev_meat_final, file=file.path(outputdir, "2030_sevs_base_high_road_meat.csv"), row.names=F)



