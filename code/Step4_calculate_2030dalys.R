

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories (in repository)
outputdir <- "output"
plotdir <- "figures"
codedir <- "code"
mypath<-"d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/code/Health Benefit claculation BFA/Health-Benefit-Calculation-BFA - Copy/"
# Read DALYs data
dalys_fish_orig <- readRDS(file.path(mypath, "my_data.rds"))
dalys_meat_orig <- readRDS(file.path(mypath, "my_meat_1_data.rds"))

# Read population data
pop_orig <- readRDS(file.path(mypath, "population_all.rds"))

# Read country list
countries_level_3 <- readRDS(file.path(mypath, "countries_level3.rds"))

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

countries_id<-countries_level_3[,c("location_name","location_id")]

#---Function to predict DALYs in 2030 (baseline) based on extrapolating data from 1990. 

r30 <- function(val, year){
  tt <- loess(val~year, span=10, control = loess.control(surface = "direct"))
  tt1 <- max(predict(tt, newdata = 2030),0)  # make sure DALYS are not negative
  return(tt1)
} 


# Format DALYs fish
omega <- dalys_fish_orig %>% 
  # Reduce to data of interest (EXPAND NOTES HERE)
  filter(measure==2 & metric==1 & sex!=3 & rei==121 & location %in% countries_level_3$location_id & age %in% age_id) %>% 
  left_join(countries_level_3)
  # Add population information
  left_join(pop, by=c("location_name", "year"="year_id", "age"="age_group_id", "sex"="sex_id")) %>%  
  # Rename columns
  rename(population=val.y, DALY=val.x)



# Calculate omega DALYs in 2030 based on extrapolation
# This is the baseline values for 2030 
j <- omega %>% 
  group_by(location_name,age,sex) %>% 
  summarize(year=year,DALY_omega=DALY,DALY2030_omega = r30(DALY,year),HDI=`Human.Development.Index.(UNDP)`,
            SDI=SDI,SDI_group=group.SDI,population=population) %>%filter(year==2017)

  # Calculate country level data for plotting ratio 
  j1<-j %>%filter(year==2017)%>%group_by(location_name) %>%mutate(ratio_DALY=DALY2030_omega/DALY_omega)%>%
  summarize(pop_adjust_DALY = sum(population*DALY_omega)/sum(population),
  pop_adjust_delta_DALY = sum(population*ratio_DALY)/sum(population), 
  population_total = sum(population))



# Format DALYs meat
  meat <- dalys_meat_orig %>% filter(measure==2 & metric==1 & sex!=3 &rei==116 & cause %in% cause_meat & location %in% countries_level_3$location_id & age %in% age_id) %>% 
    # Add population information
    left_join(countries_id,by=c("location"="location_id")) %>%
    left_join(pop, by=c("location_name"="location_name", "year"="year_id", "age"="age_group_id", "sex"="sex_id")) %>%
    # Rename columns
    rename(population=val.y, DALY=val.x) %>%select(location_name, everything())
 
# Calculate red meat DALYs in 2030 based on extrapolation
  k <- meat %>% 
    group_by(location_name,age,sex,cause) %>% 
    summarize(year=year,DALY_meat=DALY,DALY2030_meat = r30(DALY,year),
              population=population) %>%filter(year==2017)

  
# merge meat and omega n-3 datasets
  
 tog<- merge(j,k,by=c("location_name"="location_name","age"="age","sex"="sex"))
 tog<-subset(tog, select=-c(population.y))
 
 
#####################################################################################

 
 #example of intake for quality control
 m=150
 m1=155
 Intake_bs_omega<-function(x){y=1/sqrt(2*pi*(m/5)^2)*exp(-(x-m)^2/(2*(m/5)^2))}#normal distribution for example
 Intake_hr_omega<-function(x){y=1/sqrt(2*pi*(m1/5)^2)*exp(-(x-m1)^2/(2*(m1/5)^2))}#normal distribution for example
 
 
 m=50
 m1=30
 Intake_bs_meat<-function(x){y=1/sqrt(2*pi*(m/5)^2)*exp(-(x-m)^2/(2*(m/5)^2))}#normal distribution for example
 Intake_hr_meat<-function(x){y=1/sqrt(2*pi*(m1/5)^2)*exp(-(x-m1)^2/(2*(m1/5)^2))}#normal distribution for example
 
 
 #c<-zinc_iron_vita_SEV(Intake, 10, 1, "Zinc", "low", EAR_requirements)
 #b<-red_meat_SEV(Intake,10,976, red_meat_2019,red_meat_RR)
 #plot(Intake(seq(0:500)))
 #integrant<-function(x){Intake(x)}
 #inter<-(integrate(integrant,lower=-Inf,upper=Inf))
 #inter$value
 ###--------------------
 
 
 #--------------------------calculate DALYs in 2030 for the high road scenario
 source("RR_functions") 
 
 #do not forget to integrate intakes of meat, omega n-3, and micronutrients per age-sex-location to tog data.frame

DALYs1<-tog %>%  
   #step 1: For ischemic heart disease (which include omega n-3 and meat) For each age-sex-location:
 
   filter(cause==493) %>% #ischematic heart disease 
   #for omega
   mutate(DALY2030_omega_hr<-omega_n3_PAF(Intake_bs_omega,intake_hr_omega,age,omega_N_raw_2019,omega_n3_RR,1)*DALY2030_omega) %>%
    #for meat
   mutate(DALY2030_red_meat_hr<-red_meat_PAF(Intake_bs_meat,intake_hr_meat,age,cause,red_meat_raw_2019,red_meat_RR,1)*DALY2030_meat) %>%
   #add while taking overlap into consideration using Joint_PAF=1-(1-PAF1)(1-PAF2)      where PAF1 is the population attributable factor for meat, and PAF2 - for omega n-3
   mutate(DALY2030_hr_all=(1-(1-red_meat_PAF(Intake_bs_meat,intake_hr_meat,age,cause,red_meat_raw_2019,red_meat_RR,0)
                              *(1-omega_n3_PAF(Intake_bs_omega,intake_hr_omega,age,omega_N_raw_2019,omega_n3_RR,0))))*(DALY2030_red_meat_hr+DALY2030_omega_hr))%>%
   
   #step 2: For all other meat DALYs (except ischemic heart disease) perform per each age-sex-location-outcome
   filter(cause %in% cause_meat_no_ischemic)  #all other causes
   #for omega
   mutate(DALY2030_omega_hr<-omega_n3_PAF(Intake_bs_omega,intake_hr_omega,age,omega_N_raw_2019,omega_n3_RR,1)*DALY2030_omega) %>%
   #for meat
   mutate(DALY2030_red_meat_hr<-red_meat_PAF(Intake_bs_meat,intake_hr_meat,age,cause,red_meat_raw_2019,red_meat_RR,1)*DALY2030_meat)
   mutate(DALY2030_hr_all=DALY2030_red_meat_hr+DALY2030_omega_hr)
   #step 3: Sum all DALYs for each age-sex-group. This is the overall burden for the highroad per age-sex-location:
   group_by(location,age,sex) %>% #summarize all DALYs per age-sex-location
     summarize(DALY2030_hr_total=sum(DALY2030_hr_all))
  

   #--------------------------calculate changes in SEVs
   source("RR_functions") 
   
   #do not forget to integrate intakes of meat, omega n-3, and micronutrients per age-sex-location to tog data.frame
   #br are intakes of baseline, while hr = high road
   
   SEVs1<-tog %>%  
     # For each age-sex-location:
     #for omega n-3 %mg/d
     mutate(delta_SEV_omega<-omega_n3_SEV(Intake_bs_omega,age,omega_N_raw_2019,omega_n3_RR)-
              omega_n3_SEV(Intake_hr_omega,age,omega_N_raw_2019,omega_n3_RR)) %>%  #changes in %
     #for meat %g/d
     mutate(delta_SEV_meat<-red_meat_SEV(Intake_bs_meat,age,cause, red_meat_2019,red_meat_RR)-
              red_meat_SEV(Intake_hr_meat,age,cause,red_meat_2019,red_meat_RR)) %>%   #changes in % 
     #for zinc  %intakes mg/d
     mutate(delta_SEV_zinc<-micronutrient_SEV(Intake_bs_zinc, age, sex, "Zinc", country_SDIgroup, EAR_requirements)-
              micronutrient_SEV(Intake_hr_zinc, age, sex, "Zinc", country_SDIgroup, EAR_requirements)) %>% #changes in %

     #for iron %intakes mg/d
     mutate(delta_SEV_iron<-micronutrient_SEV(Intake_bs_iron, age, sex, "Iron", country_SDIgroup, EAR_requirements)-
              micronutrient_SEV(Intake_hr_iron, age, sex, "Iron", country_SDIgroup, EAR_requirements))  #changes in %
   
     #for calcium %intakes mg/d
     mutate(delta_SEV_calcium<-micronutrient_SEV(Intake_bs_iron, age, sex, "Calcium", country_SDIgroup, EAR_requirements)-
              micronutrient_SEV(Intake_hr_iron, age, sex, "Calcium", country_SDIgroup, EAR_requirements))  #changes in %  
   
     #for vitamin A %intakes RAE/d
     mutate(delta_SEV_vitA<-micronutrient_SEV(Intake_bs_vitA, age, sex, "vitA", country_SDIgroup, EAR_requirements)-
              micronutrient_SEV(Intake_hr_vitA, age, sex, "vitA", country_SDIgroup, EAR_requirements))  #changes in %  
     
     

# Zinc
#zinc <- dalys_fish_orig %>% 
#  filter(measure==2 & metric==1 & sex!=3 & rei==97 & age %in% age_id & location %in% countries_level_3$location_id) %>%
#  arrange(val)

# Iron
#iron <- dalys_fish_orig %>% 
#  filter(measure==2 & metric==1 & sex!=3 & rei==95 & age %in% age_id & location %in% countries_level_3$location_id) %>% 
#  arrange(val)




# Alon's plots
################################################################################

# Plot 1
##################################

# Plot DALY ratio
g1 <- ggplot(j, aes(x=fct_reorder(location_name, delta_DALY, .fun='median', .desc = TRUE), y=delta_DALY, color=sdi_group)) +
  # Plot DALY ratio distribution by age/sex group
  geom_boxplot(outlier.size = 0.3, alpha=0.1) +
  # Plot population adjusted DALY
  geom_point(aes(y=pop_adjust_delta_DALY), alpha=1, shape=19, na.rm=TRUE, size=2.5) +
  # Limits
  lims(y=c(0,5)) +
  # Labels
  labs(x="", y="DALY ratio (2030 / 2017)") +
  scale_color_discrete(name="SDI group") +
  # Theme
  theme(axis.text.x=element_text(angle = -90, hjust = 0,size=6),
        legend.position = "none")
g1

# Plot 2
##################################

# Prep data for plotting
j1 <- j %>% 
  group_by(location) %>% 
  summarise(population=sum(population),
            location_name=unique(location_name),
            pop_adjust_delta_DALY=unique(pop_adjust_delta_DALY),
            pop_adjust_DALY=unique(pop_adjust_DALY),
            SDI=unique(sdi_group))

# Plot data
g2 <- ggplot(j1,aes(x=log10(pop_adjust_DALY), y=pop_adjust_delta_DALY, fill=SDI, size = population)) +
  geom_point(alpha=0.5, shape=21, color='black')+
  # Point labels
  geom_text(aes(label=location_name), alpha=0.5, nudge_x = -0.18, nudge_y = 0.1, size=3, check_overlap = TRUE) +
  # Limits
  coord_cartesian(ylim = c(0, 3), xlim = c(0,6)) +
  # Reference lines
  geom_hline(yintercept=1, size=0.5, alpha=.2) +
  geom_vline(xintercept=log10(median(j1$pop_adjust_DALY, na.rm=TRUE)), size=0.5, alpha=.2) +
  # Axis labels
  labs(x="log10(DALY) (2017)", y="log10(DALY)(2017)") +
  # Legends
  scale_size(name="Population",range = c(2,15), breaks=c(1e+7,1e+8,5e+8,1e+9)) +
  scale_fill_viridis_d(name="SDI", aesthetics = "colour", option="A") +
  # Theme
  theme(legend.position="right")
g2

# Export plot
ggsave(g2, file=file.path(plotdir, "plot.pdf"), width=40, height=20, units = "cm", dpi=500) 



# Chris's plots
################################################################################

# DALY's over time
data_yr <- j %>% 
  # Reduce to data of interest
  select(location_name, )



# Plot 3 (CMF appraoch)
##################################

# Cap for delta_daly_adj
delta_daly_adj_cap <- 3

# Clean data
data_cntry <- j1 %>%
  ungroup() %>% 
  # Add ISO3 and country
  mutate(iso3=countrycode::countrycode(location_name, "country.name", "iso3c"),
         country=countrycode::countrycode(iso3, "iso3c", "country.name")) %>% 
  # Arrange columns
  select(iso3, country, population, SDI, pop_adjust_DALY, pop_adjust_delta_DALY) %>% 
  rename(sdi_group=SDI, daly_adj=pop_adjust_DALY, delta_daly_adj=pop_adjust_delta_DALY) %>% 
  # Reduce to useable data
  filter(!is.na(delta_daly_adj)) %>% 
  # Add a cap
  mutate(delta_daly_adj_cap=pmin(delta_daly_adj, delta_daly_adj_cap))

# World
world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")

# Add data to world
data_sf <- world %>% 
  left_join(data_cntry, by=c("gu_a3"="iso3"))

# Plot data
g3 <- ggplot(data_sf) +
  geom_sf(mapping=aes(fill=delta_daly_adj_cap), color="grey30", lwd=0.1)  +
  # Legend
  scale_fill_gradientn(name="ΔDALY\n(2030/2017)", colors=RColorBrewer::brewer.pal(n=9, "Greens"), na.value = "grey80",
                       breaks=seq(0, delta_daly_adj_cap, 1), labels=c(0, 1, 2, "≥3")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g3

# Export plot
ggsave(g3, file=file.path(plotdir, "delta_daly_map.png"), width=6.5, height=4, units = "in", dpi=600) 

