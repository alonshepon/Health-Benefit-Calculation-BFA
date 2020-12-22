

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(dosresmeta)
library(rms)
library(openxlsx)
library(dbplyr)
library(tidyverse)
library(MASS)
library(ggplot2)
library(fitdistrplus)
library(Hmisc)

# Directories (in repository)
outputdir <- "output"
plotdir <- "figures"
codedir <- "code"

# Read DALYs data
dalys_fishANDmeat_orig <- readRDS(file.path(outputdir, "my_data.rds"))

# Read population data
pop_orig <- readRDS(file.path(outputdir, "population_all.rds")) %>% 
  # Remove Georgia the state
  filter(location_id!=533)

# Read other data (in repository)
omega_N_raw_2019 <- read.xlsx('data/omega_RR_2019.xlsx')
red_meat_raw_2019 <- read.xlsx('data/meat_RR_2019.xlsx')
EAR_requirements <- read.xlsx('data/EAR_requirements_GBDgroups.xlsx')

# Read distributions (micronutrients)
dists <- readRDS(file.path("data/cosimo/processed/COSIMO_2010_2030_country_nutrient_age_sex_means_and_distributions.Rds"))

# Read distributions (red meat)
dists_meat <- readRDS(file=file.path("data/cosimo/processed/COSIMO_2010_2030_country_red_meat_age_sex_means_and_distributions.Rds"))

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
#cause
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

# Age ids
age_id <- c(seq(5,20), 30, 31, 32, 33) 


#---Function to predict DALYs in 2030 (baseline) based on extrapolating data from 1990. 
r30 <- function(val, year){
  tt <- loess(val~year, span=10, control = loess.control(surface = "direct"))
  tt1 <- max(predict(tt, newdata = 2030),0)  # make sure DALYS are not negative
  return(tt1)
} 

# Format dataset fish AND meat
dalys_fishANDmeat <- dalys_fishANDmeat_orig %>% 
  # Reduce to data of interest 
  select(-c("upper","lower","measure")) %>%
  filter(age %in% age_id) %>% 
  # Add population information
  left_join(pop_orig, by=c("location_name", "year"="year_id", "age"="age_group_id", "sex"="sex_id")) %>%  
  # Rename columns
  rename(population=val.y, DALY=val.x) %>%select(-c("location_id","lower","upper"))

# Calculate DALYs in 2030 based on extrapolation 
j <- dalys_fishANDmeat %>% 
  group_by(location_name,age,sex,cause) %>% 
  summarize(year=year,
            DALY=DALY,
            DALY2030 = r30(DALY,year),
            HDI=`Human Development Index (UNDP)`,
            SDI=sdi,cause=cause, 
            SDI_group=sdi_group,
            population=population) %>%
  filter(year==2017)


# Format intake distributions
##########################################################################################

# Merge data
dists2030 <- dists %>% 
  # Reduce to 2030
  filter(year==2030) %>% 
  # Simplify 
  select(country, iso3, nutrient, sex, age_group, scenario, mean_group, best_dist, 
         g_shape, g_rate, g_mean, g_mean_diff,
         ln_meanlog, ln_sdlog, ln_mean, ln_mean_diff) %>% 
  # Add age id and sex id
  mutate(sex_id=recode(sex, 
                       "men"=1,
                       "women"=2) %>% as.numeric(),
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
dists2030_meat <- dists_meat %>% 
  # Reduce to 2030
  filter(year==2030) %>% 
  # Simplify 
  select(country, iso3, nutrient, sex, age_group, scenario, mean_group, best_dist, 
         g_shape, g_rate, g_mean, g_mean_diff,
         ln_meanlog, ln_sdlog, ln_mean, ln_mean_diff) %>% 
  # Add age id and sex id
  mutate(sex_id=recode(sex, 
                       "men"=1,
                       "women"=2) %>% as.numeric(),
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


# Calculate DALYs
#####################################################################################

# Build data for DALY calculations
###############################################

# Distribution data
#############################

# Format omega distributions for merge
omega_dists <- dists2030 %>% 
  filter(nutrient=="Omega-3 fatty acids") %>% 
  select(scenario, country, iso3, nutrient, sex_id, age_id, everything())

# Format red meat distributions for merge
meat_dists <- dists2030_meat %>% 
  select(scenario, country, iso3, nutrient, sex_id, age_id, everything())

# Merge
dists4dalys <- bind_rows(omega_dists, meat_dists)

# Check data availability
missing_isos <- dists4dalys %>% 
  select(iso3, nutrient) %>% 
  unique() %>% 
  mutate(data="yes") %>% 
  mutate(nutrient=recode(nutrient, 
                         "Red meat"="meat",
                         "Omega-3 fatty acids"="omegas")) %>% 
  spread(key="nutrient", value="data") %>% 
  filter(is.na(meat)) %>% 
  pull(iso3)

# Remove missing ISOs
dists4dalys <- dists4dalys %>% 
  filter(!iso3 %in% missing_isos)

# DALY data
#############################

# Country key
cntry_key_daly <- j %>% 
  ungroup() %>% 
  select(location_name) %>% 
  unique() %>% 
  mutate(iso3=countrycode(location_name, "country.name", "iso3c"),
         country=countrycode(iso3, "iso3c", "country.name"))

# Formats DALY's before adding to distributons
dalys_clean <- j %>% 
  ungroup() %>% 
  # Remove useless columns
  select(-c(year, HDI, SDI, SDI_group, population)) %>% 
  # Add country info
  left_join(cntry_key_daly) %>% 
  select(-location_name) %>% 
  # Rename columns
  rename(age_id=age, sex_id=sex) %>% 
  # Rearrange
  select(country, iso3, sex_id, age_id, everything()) %>% 
  arrange(country, iso3, sex_id, age_id)

# Merge DALY and distributions
#############################

# Build data
data <- dists4dalys %>% 
  left_join(dalys_clean %>% select(-country), by=c("iso3", "sex_id", "age_id")) %>% 
  # Remove missing data
  filter(!is.na(cause))

# Baseline
###############################################################

# Setup DALY container
dalys <- data %>% 
  select(country, iso3, sex_id, age_id, cause, DALY2030) %>% 
  unique() %>% 
  arrange(country, sex_id, age_id, cause) %>% 
  mutate(DALY2030_all=NA,
         deltaDALY2030_all_hr=NA,
         DALY2030_all_hr=NA)

# Loop through base data
for(i in 1:nrow(dalys)){
  
  # Parameters
  if(i %in% seq(100, 100000, 100)){print(i)}
  iso3_do <-dalys$iso3[i]
  age_id_do <- dalys$age_id[i]
  sex_id_do <- dalys$sex_id[i]
  cause_do <- dalys$cause[i]
  DALY2030 <- dalys$DALY2030[i]
  
  # Setup omega intake distribution
  ############################################
  
  # Base
  ###################
  
  # Extract omega dist info
  omega_dist_base <- data %>% 
    filter(iso3==iso3_do & sex_id==sex_id_do & age_id==age_id_do & cause==cause_do & nutrient=="Omega-3 fatty acids" & scenario=="Base")
  omega_dist_base_type <- omega_dist_base$best_dist
  
  # If gamma distribution....
  if(omega_dist_base_type=="gamma"){
    shape_o_b <- omega_dist_base$g_shape
    rate_o_b <- omega_dist_base$g_rate
    x_shift_o_b <- omega_dist_base$g_mean_diff
    omega_intake_base <- function(x){y <- dgamma(x-x_shift_o_b, shape=shape_o_b, rate=rate_o_b)}
    val_hi_o_br <- qgamma(p=0.9999, shape = shape_o_b, rate=rate_o_b) + x_shift_o_b
    val_lo_o_br <- qgamma(p=0.0001, shape = shape_o_b, rate=rate_o_b) + x_shift_o_b
  }
  
  # If lognormal distribution...
  if(omega_dist_base_type=="log-normal"){
    mu_o_b <- omega_dist_base$ln_meanlog
    sigma_o_b <- omega_dist_base$ln_sdlog
    x_shift_o_b <- omega_dist_base$ln_mean_diff
    omega_intake_base <- function(x){y <- dlnorm(x-x_shift_o_b, meanlog=mu_o_b, sdlog=sigma_o_b)}
    val_hi_o_br <- qlnorm(p=0.9999, meanlog=mu_o_b, sdlog=sigma_o_b) + x_shift_o_b
    val_lo_o_br <- qlnorm(p=0.0001, meanlog=mu_o_b, sdlog=sigma_o_b) + x_shift_o_b
  }
  
  # High road
  ###################
  
  # Extract omega dist info
  omega_dist_high <- data %>% 
    filter(iso3==iso3_do & sex_id==sex_id_do & age_id==age_id_do & cause==cause_do & nutrient=="Omega-3 fatty acids" & scenario=="High road")
  omega_dist_high_type <- omega_dist_high$best_dist
  
  # If gamma distribution....
  if(omega_dist_high_type=="gamma"){
    shape_o_hr <- omega_dist_high$g_shape
    rate_o_hr <- omega_dist_high$g_rate
    x_shift_o_hr <- omega_dist_high$g_mean_diff
    omega_intake_high <- function(x){y <- dgamma(x-x_shift_o_hr, shape=shape_o_hr, rate=rate_o_hr)}
    val_hi_o_hr <- qgamma(p=0.9999, shape = shape_o_hr, rate=rate_o_hr) + x_shift_o_hr
    val_lo_o_hr <- qgamma(p=0.0001, shape = shape_o_hr, rate=rate_o_hr) + x_shift_o_hr
  }
  
  # If lognormal distribution...
  if(omega_dist_high_type=="log-normal"){
    mu_o_hr <- omega_dist_high$ln_meanlog
    sigma_o_hr <- omega_dist_high$ln_sdlog
    x_shift_o_hr <- omega_dist_high$ln_mean_diff
    omega_intake_high <- function(x){y <- dlnorm(x-x_shift_o_hr, meanlog=mu_o_hr, sdlog=sigma_o_hr)}
    val_hi_o_hr <- qlnorm(p=0.9999, meanlog=mu_o_hr, sdlog=sigma_o_hr) + x_shift_o_hr
    val_lo_o_hr <- qlnorm(p=0.0001, meanlog=mu_o_hr, sdlog=sigma_o_hr) + x_shift_o_hr
  }
  
  # Setup meat intake distribution
  ############################################
  
  # Base
  ###################
  
  # Extract meat dist info
  meat_dist_base <- data %>% 
    filter(iso3==iso3_do & sex_id==sex_id_do & age_id==age_id_do & cause==cause_do & nutrient=="Red meat" & scenario=="Base")
  meat_dist_base_type <- meat_dist_base$best_dist
  
  # If gamma distribution....
  if(meat_dist_base_type=="gamma"){
    shape_m_b <- meat_dist_base$g_shape
    rate_m_b <- meat_dist_base$g_rate
    x_shift_m_b <- meat_dist_base$g_mean_diff
    meat_intake_base <- function(x){y <- dgamma(x-x_shift_m_b, shape=shape_m_b, rate=rate_m_b)}
    val_hi_m_br <- qgamma(p=0.9999, shape = shape_m_b, rate=rate_m_b) + x_shift_m_b
    val_lo_m_br <- qgamma(p=0.0001, shape = shape_m_b, rate=rate_m_b) + x_shift_m_b
  }
  
  # If lognormal distribution...
  if(meat_dist_base_type=="log-normal"){
    mu_m_b <- meat_dist_base$ln_meanlog
    sigma_m_b <- meat_dist_base$ln_sdlog
    x_shift_m_b <- meat_dist_base$ln_mean_diff
    meat_intake_base <- function(x){y <- dlnorm(x-x_shift_m_b, meanlog=mu_m_b, sdlog=sigma_m_b)}
    val_hi_m_br <- qlnorm(p=0.9999, meanlog=mu_m_b, sdlog=sigma_m_b) + x_shift_m_b
    val_lo_m_br <- qlnorm(p=0.0001, meanlog=mu_m_b, sdlog=sigma_m_b) + x_shift_m_b
  }
  
  # High road
  ###################
  
  # Extract meat dist info
  meat_dist_high <- data %>% 
    filter(iso3==iso3_do & sex_id==sex_id_do & age_id==age_id_do & cause==cause_do & nutrient=="Red meat" & scenario=="High road")
  meat_dist_high_type <- meat_dist_high$best_dist
  
  # If gamma distribution....
  if(meat_dist_high_type=="gamma"){
    shape_m_hr <- meat_dist_high$g_shape
    rate_m_hr <- meat_dist_high$g_rate
    x_shift_m_hr <- meat_dist_high$g_mean_diff
    meat_intake_high <- function(x){y <- dgamma(x-x_shift_m_hr, shape=shape_m_hr, rate=rate_m_hr)}
    val_hi_m_hr <- qgamma(p=0.9999, shape = shape_m_hr, rate=rate_m_hr) + x_shift_m_hr
    val_lo_m_hr <- qgamma(p=0.0001, shape = shape_m_hr, rate=rate_m_hr) + x_shift_m_hr
  }
  
  # If lognormal distribution...
  if(meat_dist_high_type=="log-normal"){
    mu_m_hr <- meat_dist_high$ln_meanlog
    sigma_m_hr <- meat_dist_high$ln_sdlog
    x_shift_m_hr <- meat_dist_high$ln_mean_diff
    meat_intake_high <- function(x){y <- dlnorm(x-x_shift_m_hr, meanlog=mu_m_hr, sdlog=sigma_m_hr)}
    val_hi_m_hr <- qlnorm(p=0.9999, meanlog=mu_m_hr, sdlog=sigma_m_hr) + x_shift_m_hr
    val_lo_m_hr <- qlnorm(p=0.0001, meanlog=mu_m_hr, sdlog=sigma_m_hr) + x_shift_m_hr
  }
  
  if(F){
    
    x <- -300:2000
    y_lo <- meat_intake_base(x)
    y_hi <- meat_intake_high(x)
    plot(x, y_lo, type="l", col="red")
    lines(x, y_hi, col="blue")
    abline(v=c(val_lo_m_br, val_hi_m_br))
    
  }
  
  # Calculate DALYs
  ############################################
  
  # If heart disease
  ######################################
  
  if(cause_do==493){
    
    # Step 1
    ########################
    
    # DALY OMEGA
    DALY2030_omega1 <- try(omega_n3_PAF(Intake_br = omega_intake_base,
                                    Intake_hr = omega_intake_high,
                                    age = age_id_do,
                                    omega_N_raw_2019,
                                    omega_n3_RR,
                                    flag_omega = 0,
                                    val_lo_br=val_lo_o_br,
                                    val_hi_br=val_hi_o_br,
                                    val_lo_hr=val_lo_o_hr,
                                    val_hi_hr=val_hi_o_hr))
    DALY2030_omega2 <- try(DALY2030_omega1 * DALY2030)
    
    # DALY red meat
    DALY2030_meat1 <- try(red_meat_PAF(Intake_br = meat_intake_base, 
                                       Intake_hr = meat_intake_high,
                                       age = age_id_do,
                                       meat_outcome = cause_do,
                                       red_meat_2019 = red_meat_raw_2019, 
                                       red_meat_RR,
                                       flag_meat = 0,
                                       val_lo_br=val_lo_m_br,
                                       val_hi_br=val_hi_m_br,
                                       val_lo_hr=val_lo_m_hr,
                                       val_hi_hr=val_hi_m_hr)) 
    DALY2030_meat2 <- try(DALY2030_meat1 * DALY2030)
    
    # DALY combined
    DALY2030_all <- try((1 - (1-DALY2030_meat1) * (1-DALY2030_omega1) )  * (DALY2030_meat2 + DALY2030_omega2))
    
    # Step 2
    ########################
    
    DALY2030_omega_hr <- try(omega_n3_PAF(Intake_br = omega_intake_base,
                                      Intake_hr = omega_intake_high,
                                      age = age_id_do,
                                      omega_N_raw_2019,
                                      omega_n3_RR,
                                      flag_omega = 1,
                                      val_lo_br=val_lo_o_br,
                                      val_hi_br=val_hi_o_br,
                                      val_lo_hr=val_lo_o_hr,
                                      val_hi_hr=val_hi_o_hr))
    
    DALY2030_red_meat_hr <- try(red_meat_PAF(Intake_br = meat_intake_base, 
                                         Intake_hr = meat_intake_high,
                                         age = age_id_do,
                                         meat_outcome = cause_do,
                                         red_meat_2019 = red_meat_raw_2019, 
                                         red_meat_RR,
                                         flag_meat = 1,
                                         val_lo_br=val_lo_m_br,
                                         val_hi_br=val_hi_m_br,
                                         val_lo_hr=val_lo_m_hr,
                                         val_hi_hr=val_hi_m_hr))
    
    deltaDALY2030_all_hr <- try((1 - (1 - DALY2030_red_meat_hr) * (1 - DALY2030_omega_hr) )  * (DALY2030_all)) 
    
    DALY2030_all_hr <-  try(deltaDALY2030_all_hr + DALY2030_all)
    
  
  # If not heart disease
  ######################################
    
  }else{
    
    # Step 1
    ########################
    
    # DALY combined
    DALY2030_all <- try(red_meat_PAF(Intake_br = meat_intake_base, 
                                     Intake_hr = meat_intake_high,
                                     age = age_id_do,
                                     meat_outcome = cause_do,
                                     red_meat_2019 = red_meat_raw_2019,
                                     red_meat_RR,
                                     flag_meat = 0,
                                     val_lo_br=val_lo_m_br,
                                     val_hi_br=val_hi_m_br,
                                     val_lo_hr=val_lo_m_hr,
                                     val_hi_hr=val_hi_m_hr) * DALY2030)
    
    # Step 2
    ########################
    
    deltaDALY2030_all_hr <- try(red_meat_PAF(Intake_br = meat_intake_base, 
                                         Intake_hr = meat_intake_high,
                                         age = age_id_do,
                                         meat_outcome = cause_do,
                                         red_meat_2019 = red_meat_raw_2019, 
                                         red_meat_RR,
                                         flag_meat = 1,
                                         val_lo_br=val_lo_m_br,
                                         val_hi_br=val_hi_m_br,
                                         val_lo_hr=val_lo_m_hr,
                                         val_hi_hr=val_hi_m_hr) * DALY2030_all)
    
    DALY2030_all_hr <- try(deltaDALY2030_all_hr + DALY2030_all)
    
  }
  
  # Handle try errors
  if(inherits(DALY2030_all, "try-error")){DALY2030_all <- NA}
  if(inherits(deltaDALY2030_all_hr, "try-error")){deltaDALY2030_all_hr <- NA}
  if(inherits(DALY2030_all_hr, "try-error")){DALY2030_all_hr <- NA}
  
  # Record results
  dalys$DALY2030_all[i] <- DALY2030_all
  dalys$deltaDALY2030_all_hr[i] <- deltaDALY2030_all_hr
  dalys$DALY2030_all_hr[i] <- DALY2030_all_hr
  
}

# Export DALYs
write.csv(dalys, file=file.path(outputdir, "2030_dalys_base_high_road.csv"), row.names = F)

