

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
pop_orig <- readRDS(file.path(outputdir, "population_all.rds"))

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
  summarize(year=year,DALY=DALY,DALY2030 = r30(DALY,year),
            HDI=`Human Development Index (UNDP)`,
            SDI=sdi,cause=cause, 
            SDI_group=sdi_group,
            population=population) %>%
  filter(year==2017)


# Example
#####################################################################################

# example of intake distributions for quality control (can delete it later)
m=10
m1=255
Intake_bs_omega<-function(x){y=1/sqrt(2*pi*(m/5)^2)*exp(-(x-m)^2/(2*(m/5)^2))}#normal distribution for example
Intake_hr_omega<-function(x){y=1/sqrt(2*pi*(m1/5)^2)*exp(-(x-m1)^2/(2*(m1/5)^2))}#normal distribution for example
r<-seq(0:500)
df<-omega_n3_SEV(Intake_bs_omega,10,omega_N_raw_2019,omega_n3_RR)
plot(Intake_bs_omega(r),col='blue')

m=200
m1=60
Intake_bs_meat<-function(x){y=1/sqrt(2*pi*(m/5)^2)*exp(-(x-m)^2/(2*(m/5)^2))}#normal distribution for example
Intake_hr_meat<-function(x){y=1/sqrt(2*pi*(m1/5)^2)*exp(-(x-m1)^2/(2*(m1/5)^2))}#normal distribution for example
df<-red_meat_SEV(Intake_bs_meat,10,493,red_meat_raw_2019,red_meat_RR)
plot(Intake_bs_meat(r),col='blue')

#c<-zinc_iron_vita_SEV(Intake, 10, 1, "Zinc", "low", EAR_requirements)
#b<-red_meat_SEV(Intake,10,976, red_meat_2019,red_meat_RR)
#plot(Intake(seq(0:500)))
integrant<-function(x){Intake_bs_meat(x)}
inter<-(integrate(integrant,lower=-Inf,upper=Inf))
inter$value


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



# Calculate changes in summary exposure values (SEVs) -- micronutrients
##########################################################################################

# Nutrients to calculate SEVS for
nutr_sevs <- c("Zinc", "Iron", "Calcium", "Vitamin A")

# Build data required for micronutrient SEV calculations
data_sev_mn <- dists2030 %>% 
  # Reduce to nutrients of interest
  filter(nutrient %in% nutr_sevs) %>% 
  # Add SDI group
  left_join(sdi_hdi_key, by=c("iso3")) %>% 
  # Reduce to age groups with required data
  filter(age_id>=5)

# Loop through micronutrients to calculate SEVs for
x <- 1
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
  
  # Rename nutrient
  if(nutr_do=="Vitamin A"){nutr_do <- "VitA"}
  
  # If gamma distribution....
  if(best_dist=="gamma"){
    shape <- data_sev_mn$g_shape[x]
    rate <- data_sev_mn$g_rate[x]
    x_shift <- data_sev_mn$g_mean_diff[x]
    intake_function <- function(x){y <- dgamma(x-x_shift, shape=shape, rate=rate)}
  }
  
  # If lognormal distribution...
  if(best_dist=="log-normal"){
    mu <- data_sev_mn$ln_meanlog[x]
    sigma <- data_sev_mn$ln_sdlog[x]
    x_shift <- data_sev_mn$ln_mean_diff[x]
    intake_function <- function(x){y <- dlnorm(x-x_shift, meanlog=mu, sdlog=sigma)}
  }
  
  # Calculate SEV
  sev <- try(micronutrient_SEV(Intake=intake_function, 
                               age=age_id_do, 
                               sex=sex_id_do,
                               nutrient=nutr_do, 
                               country_SDIgroup=sdi_group_do, 
                               EAR_requirements))
  
  # Record based on try()
  if(inherits(sev, "try-error")){
    sev_out <- NA
  }else{
    sev_out <- sev
  }
  
  # Record
  data_sev_mn$sev[x] <- sev_out
  
  # Build dataframe row
  # row_out <- tibble(scenario=scenario_do,
  #                   nutrient=nutr_do,
  #                   iso3=iso3_do,
  #                   sex=sex_id_do,
  #                   age=age_id_do,
  #                   sev=sev_out)
  
}

# Format SEVs
sev_mn_final <- data_sev_mn %>% 
  select(scenario, nutrient, country, iso3, sex_id, age_id, sev) %>% 
  spread(key="scenario", value="sev") %>% 
  rename(sev_high="High road", sev_base="Base") %>% 
  mutate(sev_delta=sev_high-sev_base)

# Export
write.csv(sev_mn_final, file=file.path(outputdir, "2030_sevs_base_high_road_micronutrients.csv"), row.names=F)


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
# sevs_micronutrients <- purrr::map_df(1:nrow(data_sev_mn), function(x){
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
  }
  
  # If lognormal distribution...
  if(best_dist=="log-normal"){
    mu <- data_sev_omega$ln_meanlog[x]
    sigma <- data_sev_omega$ln_sdlog[x]
    x_shift <- data_sev_omega$ln_mean_diff[x]
    intake_function <- function(x){y <- dlnorm(x-x_shift, meanlog=mu, sdlog=sigma)}
  }
  
  # Calculate SEV
  sev <- try(omega_n3_SEV(Intake=intake_function,
                          age=age_id_do, 
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
  
  # Build dataframe row
  # row_out <- tibble(scenario=scenario_do,
  #                   nutrient=nutr_do,
  #                   iso3=iso3_do,
  #                   sex=sex_id_do,
  #                   age=age_id_do,
  #                   sev=sev_out)
  
}

# Format SEVs
sev_omega_final <- data_sev_omega %>% 
  mutate(nutrient="Omega-3 fatty acids") %>% 
  select(scenario, nutrient, country, iso3, sex_id, age_id, sev) %>% 
  spread(key="scenario", value="sev") %>% 
  rename(sev_high="High road", sev_base="Base") %>% 
  mutate(sev_delta=sev_high-sev_base)

# Export
write.csv(sev_omega_final, file=file.path(outputdir, "2030_sevs_base_high_road_omega3s.csv"), row.names=F)


# Calculate changes in summary exposure values (SEVs) -- red meat
##########################################################################################

# Build data required for micronutrient SEV calculations
data_sev_meat <- dists2030_meat %>% 
  # Reduce to age groups with required data
  filter(age_id>=10)

# Loop through
for(x in 1:nrow(data_sev_meat)){
  
  # Parameters
  print(x)
  scenario_do <- data_sev_meat$scenario[x]
  iso3_do <- data_sev_meat$iso3[x]
  nutr_do <- data_sev_meat$nutrient[x]
  age_id_do <- data_sev_meat$age_id[x]
  sex_id_do <- data_sev_meat$sex_id[x]
  best_dist <- data_sev_meat$best_dist[x]
  
  # If gamma distribution....
  if(best_dist=="gamma"){
    shape <- data_sev_meat$g_shape[x]
    rate <- data_sev_meat$g_rate[x]
    x_shift <- data_sev_meat$g_mean_diff[x]
    intake_function <- function(x){y <- dgamma(x-x_shift, shape=shape, rate=rate)}
  }
  
  # If lognormal distribution...
  if(best_dist=="log-normal"){
    mu <- data_sev_meat$ln_meanlog[x]
    sigma <- data_sev_meat$ln_sdlog[x]
    x_shift <- data_sev_meat$ln_mean_diff[x]
    intake_function <- function(x){y <- dlnorm(x-x_shift, meanlog=mu, sdlog=sigma)}
  }
  
  # Calculate SEV
  sev <- try(red_meat_SEV(Intake=intake_function,
                          age=age_id_do,
                          meat_outcome=493,
                          red_meat_2019=red_meat_raw_2019,
                          red_meat_RR))
  
  # Record based on try()
  if(inherits(sev, "try-error")){
    sev_out <- NA
  }else{
    sev_out <- sev
  }
  
  # Record
  data_sev_meat$sev[x] <- sev_out
  
  # Build dataframe row
  # row_out <- tibble(scenario=scenario_do,
  #                   nutrient=nutr_do,
  #                   iso3=iso3_do,
  #                   sex=sex_id_do,
  #                   age=age_id_do,
  #                   sev=sev_out)
  
}

# Format SEVs
sev_meat_final <- data_sev_meat %>% 
  mutate(nutrient="Red meat") %>% 
  select(scenario, nutrient, country, iso3, sex_id, age_id, sev) %>% 
  spread(key="scenario", value="sev") %>% 
  rename(sev_high="High road", sev_base="Base") %>% 
  mutate(sev_delta=sev_high-sev_base)

# Export
write.csv(sev_meat_final, file=file.path(outputdir, "2030_sevs_base_high_road_meat.csv"), row.names=F)





# Calculate DALYs
#####################################################################################

if(F){

  DALYs1<-dalys_fishANDmeat %>%
    #step 1: For ischemic heart disease (which include omega n-3 and meat) For each age-sex-location:
    filter(cause==493) %>% #ischematic heart disease
    #for omega
    mutate(DALY2030_omega<-omega_n3_PAF(Intake_bs_omega,intake_hr_omega,age,omega_N_raw_2019,omega_n3_RR,0)*DALY2030) %>%
    #for meat
    mutate(DALY2030_red_meat<-red_meat_PAF(Intake_bs_meat,intake_hr_meat,age,cause,red_meat_raw_2019,red_meat_RR,0)*DALY2030) %>%
    #add while taking overlap into consideration using Joint_PAF=1-(1-PAF1)(1-PAF2)      where PAF1 is the population attributable factor for meat, and PAF2 - for omega n-3
    mutate(DALY2030_all=(1-(1-red_meat_PAF(Intake_bs_meat,intake_hr_meat,age,cause,red_meat_raw_2019,red_meat_RR,0)
                               *(1-omega_n3_PAF(Intake_bs_omega,intake_hr_omega,age,omega_N_raw_2019,omega_n3_RR,0))))*(DALY2030_red_meat+DALY2030_omega))%>%
  
  
    filter(cause %in% cause_meat_no_ischemic)  %>% #all other causes of meat
    #for meat
    mutate(DALY2030_all<-red_meat_PAF(Intake_bs_meat,intake_hr_meat,age,cause,red_meat_raw_2019,red_meat_RR,0)*DALY2030)  %>%
  
  
    ##------------calculate DALYs for high road (hr) scenario
    ## PAF* when flag of PAF function (last entry) is set to 1; otherwise its regular PAF
  
    #step 1: For ischemic heart disease (which include omega n-3 and meat) For each age-sex-location:
    filter(cause==493) %>% #ischematic heart disease
    mutate(deltaDALY2030_all_hr=(1-(1-red_meat_PAF(Intake_bs_meat,intake_hr_meat,age,cause,red_meat_raw_2019,red_meat_RR,1)
                            *(1-omega_n3_PAF(Intake_bs_omega,intake_hr_omega,age,omega_N_raw_2019,omega_n3_RR,1))))*(DALY2030_all),
           DALY2030_all_hr=deltaDALY2030_all_hr+DALY2030_all)%>%
  
    #step 2: For all other meat DALYs (except ischemic heart disease) perform per each age-sex-location-outcome
    filter(cause %in% cause_meat_no_ischemic)  %>% #all other causes of meat
    #for meat
    mutate(deltaDALY2030_all_hr<-red_meat_PAF(Intake_bs_meat,intake_hr_meat,age,cause,red_meat_raw_2019,red_meat_RR,1)*DALY2030_all,
           DALY2030_all_hr=deltaDALY2030_all_hr+DALY2030_all) %>%
    #sum all DALYS of all outcomes
    group_by(location_name,age,sex) %>%summarize(DALY2030_br=sum(DALY2030_all,na.rm = TRUE),DALY2030_hr=sum(DALY2030_all_hr,na.rm = TRUE))

}

