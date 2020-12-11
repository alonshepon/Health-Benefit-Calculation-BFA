

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories (outside repo)
mypath <- "output" # Chris Free's computer
# mypath <-"d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/code/Health Benefit claculation BFA/Health-Benefit-Calculation-BFA - Copy/" # Alon's computer

# Directories (in repository)
outputdir <- "output"
plotdir <- "figures"
codedir <- "code"

# Read DALYs data
dalys_fish_orig <- readRDS(file.path(mypath, "my_data.rds"))
# dalys_meat_orig <- readRDS(file.path(mypath, "my_meat_1_data.rds")) # Alon's computer
dalys_meat_orig <- readRDS(file.path(mypath, "my_meat_data.rds")) # Chris's computer

# Read population data
pop_orig <- readRDS(file.path(mypath, "population_all.rds"))

# Read country list
countries_level_3 <- readRDS(file.path(mypath, "countries_level3.rds"))

# Read other data
omega_N_raw_2019 <- readxl::read_excel('code/omega_RR_2019.xlsx')
red_meat_raw_2019 <- readxl::read_excel('code/meat_RR_2019.xlsx')
EAR_requirements <- readxl::read_excel('code/EAR_requirements_GBDgroups.xlsx')

# Read distributions (micronutrients)
dists <- readRDS(file.path("data/cosimo/processed/COSIMO_2010_2030_country_nutrient_age_sex_means_and_distributions.Rds"))

# Read distributions (red meat)
dists_meat <- readRDS(file=file.path("data/cosimo/processed/COSIMO_2010_2030_country_red_meat_age_sex_means_and_distributions.Rds"))

# Read SDI key
sdi_key <- read.csv(file=file.path(outputdir, "sdi_key.csv"), as.is=T)

# Source helpher functions
source("code/RR_functions_cmf.R")


# Build data
################################################################################

# Meat causes
cause_meat<- c(429,441,493,495,496,497,976) 
cause_meat_no_ischemic<- c(429,441,495,496,497,976)

# Age ids
age_id <- c(seq(5,20), 30, 31, 32, 33) 

# Format population data
pop <- pop_orig %>%
  # Remove columns
  select(-c(lower, upper)) %>% 
  # Reduce to countries
  # This fixes the duplication error that used to occur
  filter(location_id %in% countries_level_3$location_id) %>% 
  # Remove another column (b/c Alon did)
  select(-location_id)

# Vector of country ids
countries_id<-countries_level_3[,c("location_name","location_id")]

# Format DALYs fish
omega <- dalys_fish_orig %>% 
  # Reduce to data of interest (EXPAND NOTES HERE)
  filter(measure==2 & metric==1 & sex!=3 & rei==121 & location %in% countries_level_3$location_id & age %in% age_id) %>% 
  left_join(countries_level_3) %>% 
  # Add population information
  left_join(pop, by=c("location_name", "year"="year_id", "age"="age_group_id", "sex"="sex_id")) %>%  
  # Rename columns
  rename(population=val.y, DALY=val.x)

# Calculate omega DALYs in 2030 based on extrapolation
# This is the baseline values for 2030 
j <- omega %>% 
  group_by(location_name,age,sex) %>% 
  summarize(year=year,
            DALY_omega=DALY,
            DALY2030_omega = r30(DALY,year), 
            # HDI=`Human.Development.Index.(UNDP)`, # alon's computer
            # SDI=SDI, # alon's computer
            # SDI_group=group.SDI, # alon's computer
            HDI=`Human Development Index (UNDP)`, # chris's computer
            SDI=sdi, # chris's computer
            SDI_group=sdi_group, # chris's computer
            population=population) %>%
  filter(year==2017)

# Calculate country level data for plotting ratio 
j1 <- j %>%
  filter(year==2017)%>%
  group_by(location_name) %>%
  mutate(ratio_DALY=DALY2030_omega/DALY_omega)%>%
  summarize(pop_adjust_DALY = sum(population*DALY_omega)/sum(population),
            pop_adjust_delta_DALY = sum(population*ratio_DALY)/sum(population), 
            population_total = sum(population))

# Format DALYs meat
meat <- dalys_meat_orig %>% 
  filter(measure==2 & metric==1 & sex!=3 &rei==116 & cause %in% cause_meat & location %in% countries_level_3$location_id & age %in% age_id) %>% 
  # Add population information
  # left_join(countries_id, by=c("location"="location_id")) %>% # commented out on chris' computer, not alon's
  left_join(pop, by=c("location_name"="location_name", "year"="year_id", "age"="age_group_id", "sex"="sex_id")) %>%
  # Rename columns
  rename(population=val.y, DALY=val.x) %>%
  select(location_name, everything())

# Calculate red meat DALYs in 2030 based on extrapolation
k <- meat %>% 
  group_by(location_name,age,sex,cause) %>% 
  summarize(year=year,DALY_meat=DALY,
            DALY2030_meat = r30(DALY,year),
            population=population) %>%
  filter(year==2017)

# merge meat and omega n-3 datasets
tog <- merge(j,k,by=c("location_name"="location_name","age"="age","sex"="sex"))
tog <- subset(tog, select=-c(population.y))


# Format intake distributions
##########################################################################################

# Merge data
dists2030 <- dists %>% 
  # Remove incomplete data (fix later)
  filter(!is.na(g_shape)) %>% 
  # Reduce to 2030
  filter(year==2030) %>% 
  # Simplify 
  select(country, iso3, nutrient, sex, age_group, scenario, mean_group, g_shape, g_rate, g_mean, g_mean_diff) %>% 
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
  # Remove incomplete data (fix later)
  filter(!is.na(g_shape)) %>% 
  # Reduce to 2030
  filter(year==2030) %>% 
  # Simplify 
  select(country, iso3, nutrient, sex, age_group, scenario, mean_group, g_shape, g_rate, g_mean, g_mean_diff) %>% 
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
##########################################################################################

# # Merge distributions with DALYs
# tog1 <- dists2030 %>% 
#   # Add DALYs to distributions (fix so that merge isn't based on country)
#   left_join(tog %>% select(-c(year.x, year.y)), by=c("country"="location_name", "age_id"="age", "sex_id"="sex")) %>% 
#   # Eliminate age groups below ID=10
#   filter(age_id>=10) %>% 
#   # Reduce to ischematic heart disease 
#   filter(cause==493)
# 
# # Loop through each row and compute
# i <- 1
# for(i in 1:nrow(tog1)){
#   
#   # Build intake distribution function
#   shape <- tog1$g_shape[i]
#   rate <- tog1$g_rate[i]
#   mean_diff <- tog1$g_mean_diff[i]
#   intake_dist_function <- function(x){
#     y <- dgamma(x-mean_diff, shape=shape, rate=rate)
#   }
#   
#   # Calculate the DALY2030_omega_hr
#   age_do <- tog1$age_id[i]
#   DALY2030_omega <- tog1$DALY2030_omega[i]
#   DALY2030_omega_hr <- omega_n3_PAF(Intake_br=intake_dist_function, 
#                                     Intake_hr=intake_dist_function, 
#                                     age=age_do, 
#                                     omega_N_raw_2019, 
#                                     omega_n3_RR, 
#                                     flag_omega=1) * DALY2030_omega
#   
#   # Calculate the DALY2030_red_meat_hr
#   cause_do <- tog1$cause[i]
#   DALY2030_meat <- tog1$DALY2030_meat[i]
#   DALY2030_red_meat_hr = red_meat_PAF(Intake_br=intake_dist_function,  
#                                       Intake_hr=intake_dist_function, 
#                                       age=age_do,  
#                                       meat_outcome=cause_do, 
#                                       red_meat_raw_2019, 
#                                       red_meat_RR, 
#                                       flag_meat=1) * DALY2030_meat
#   
#   
#   #add while taking overlap into consideration using Joint_PAF=1-(1-PAF1)(1-PAF2)      
#   # where PAF1 is the population attributable factor for meat, and PAF2 - for omega n-3
#   red_meat_paf <- red_meat_PAF(Intake_br=intake_dist_function,  
#                                Intake_hr=intake_dist_function, 
#                                age=age_do,  
#                                meat_outcome=cause_do, 
#                                red_meat_raw_2019, 
#                                red_meat_RR, 
#                                0)
#   omega_n3_paf <- omega_n3_PAF(Intake_br=intake_dist_function, 
#                                Intake_hr=intake_dist_function, 
#                                age=age_do, 
#                                omega_N_raw_2019, 
#                                omega_n3_RR,
#                                0)
#   DALY2030_hr_all = (1-(1-red_meat_paf*(1-omega_n3_paf))) * (DALY2030_red_meat_hr + DALY2030_omega_hr)
#   
#   
#   
#   
#   #step 2: For all other meat DALYs (except ischemic heart disease) perform per each age-sex-location-outcome
#   filter(cause %in% cause_meat_no_ischemic)  %>% #all other causes
#     #for meat
#     mutate(DALY2030_red_meat_hr = red_meat_PAF(Intake_bs_meat,intake_hr_meat,age,cause,red_meat_raw_2019,red_meat_RR,1)*DALY2030_meat)
#   mutate(DALY2030_hr_all = DALY2030_red_meat_hr+DALY2030_omega_hr)
#   #step 3: Sum all DALYs for each age-sex-group. This is the overall burden for the highroad per age-sex-location:
#   group_by(location,age,sex) %>% #summarize all DALYs per age-sex-location
#     summarize(DALY2030_hr_total=sum(DALY2030_hr_all))
#   
#   
#   
# }




# Calculate changes in summary exposure values (SEVs) -- micronutrients
##########################################################################################

# Nutrients to calculate SEVS for
nutr_sevs <- c("Zinc", "Iron", "Calcium", "Vitamin A")

# Build data required for micronutrient SEV calculations
data_sev_mn <- dists2030 %>% 
  # Reduce to nutrients of interest
  filter(nutrient %in% nutr_sevs) %>% 
  # Add SDI group
  left_join(sdi_key, by=c("country"="location")) %>% 
  # Reduce to age groups with required data
  filter(age_id>=5) %>% 
  # Reduce to rows with the required SEV ingredients: SDI group
  filter(!is.na(sdi_group) & !is.na(g_shape)) 
  
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
  
  # Rename nutrient
  if(nutr_do=="Vitamin A"){nutr_do <- "VitA"}
  
  # If gamma distribution....
  shape <- data_sev_mn$g_shape[x]
  rate <- data_sev_mn$g_rate[x]
  x_shift <- data_sev_mn$g_mean_diff[x]
  intake_function <- function(x){y <- dgamma(x-x_shift, shape=shape, rate=rate)}

  # If lognormal distribution...
  
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
  filter(age_id>=10) %>%
  # Reduce to rows with the required SEV ingredients: SDI group
  filter(!is.na(g_shape))

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
  
  # If gamma distribution....
  shape <- data_sev_omega$g_shape[x]
  rate <- data_sev_omega$g_rate[x]
  x_shift <- data_sev_omega$g_mean_diff[x]
  intake_function <- function(x){y <- dgamma(x-x_shift, shape=shape, rate=rate)}
  
  # If lognormal distribution...
  
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
  filter(age_id>=10) %>%
  # Reduce to rows with the required SEV ingredients: SDI group
  filter(!is.na(g_shape))

# Loop through
for(x in 1:nrow(data_sev_meat)){
  
  # Parameters
  print(x)
  scenario_do <- data_sev_meat$scenario[x]
  iso3_do <- data_sev_meat$iso3[x]
  nutr_do <- data_sev_meat$nutrient[x]
  age_id_do <- data_sev_meat$age_id[x]
  sex_id_do <- data_sev_meat$sex_id[x]
  
  # If gamma distribution....
  shape <- data_sev_meat$g_shape[x]
  rate <- data_sev_meat$g_rate[x]
  x_shift <- data_sev_meat$g_mean_diff[x]
  intake_function <- function(x){y <- dgamma(x-x_shift, shape=shape, rate=rate)}
  
  # If lognormal distribution...
  
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


