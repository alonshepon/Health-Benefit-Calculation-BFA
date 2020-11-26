

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

# Read DALYs data
dalys_fish_orig <- readRDS(file.path(outputdir, "my_data.rds"))
dalys_meat_orig <- readRDS(file.path(outputdir, "my_meat_data.rds"))

# Read population data
pop_orig <- readRDS(file.path(outputdir, "population_all.rds"))

# Read country list
countries_level_3 <- readRDS(file.path(outputdir, "countries_level3.rds"))

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

# REI CODES
# 121 low seafood
# 97 zinc
# 96 vitamin A
# 95 iron

# Build data
################################################################################

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

# Format DALYs fish
omega <- dalys_fish_orig %>% 
  # Reduce to data of interest (EXPAND NOTES HERE)
  filter(measure==2 & metric==1 & sex!=3 & rei==121 & location %in% countries_level_3$location_id & age %in% age_id) %>% 
  # Add population information
  left_join(pop, by=c("location_name", "year"="year_id", "age"="age_group_id", "sex"="sex_id")) %>%  
  # Rename columns
  rename(population=val.y, DALY=val.x)

# Format DALYs meat
# Alon's code included this chunk but it doesn't work b/c no rei==121 in dals_meat_orig$rei 
if(F){
  dalys_meat <- dalys_meat_orig %>% 
    # Reduce to data of interest (EXPAND NOTES HERE)
    filter(measure==2 & metric==1 & sex!=3 & rei==121 & location %in% countries_level_3$location_id & age %in% age_id) %>% 
    # Add population information
    left_join(pop, by=c("location_name", "year"="year_id", "age"="age_group_id", "sex"="sex_id")) %>%
    # Rename columns
    rename(population=val.y, DALY=val.x)
}

# Calculate DALYs in 2030 based on extrapolation for meat and omega n-3 based on the function below this chunk
# This is the baseline values for 2030 
j <- omega %>% 
  # Calculate 2030 DALY by country-sex-age
  group_by(location_name, sex, age) %>% 
  mutate(DALY2030 = r30(DALY,year)) %>%
  # Reduce to 2017
  filter(year==2017) %>% 
  # Calculate ratio 2030 DALY to 2017 DALY
  mutate(delta_DALY = (DALY2030/DALY)) %>%
  # Calculate population adjusted DALY and DALY ratio and total population
  group_by(location_name, year) %>%
  mutate(pop_adjust_DALY = sum(population*DALY)/sum(population)) %>%
  mutate(pop_adjust_delta_DALY = sum(population*delta_DALY)/sum(population)) %>% 
  mutate(population_total = sum(population))


#---Function to predict DALYs in 2030 (baseline) based on extrapolating data from 1990. 

r30 <- function(val, year){
  tt <- loess(val~year, span=10, control = loess.control(surface = "direct"))
  tt1 <- max(predict(tt, newdata = 2030),0)  # make sure DALYS are not negative
  return(tt1)
} 


#divide DALY meat to the various outcomes 


#--------------------------calculate DALYs in 2030 for the high road scenario


#step 1: For meat DALYs (except ischemic heart disease) perform per each age-sex-location-outcome

#DALY2030meat_highroad=DALY2030_baseline*(SEV_highroad)/SEV_baseline




#step 2: For ischemic heart disease (which include omega n-3 and meat) For each age-sex-location:

#DALY2030ischemic_highroad=Joint_PAF(DALY2030_baseline_meat*(SEV_highroad_meat)/SEV_baseline_meat+DALY2030_baseline_omega*(SEV_highroad_omega)/SEV_baseline_omega)

#Joint_PAF=1-(1-PAF1)(1-PAF2)      where PAF1 is the population attributable factor for meat, and PAF2 - for omega n-3




#step 3: Sum all DALYs for each age-sex-group. This is the overall burden for the highroad per age-sex-location:

#That is, DALY2030ischemic_highroad+DALY2030meat_highroad, where the last term is for all the non-ischemic heart disease outcomes




















# Zinc
zinc <- dalys_fish_orig %>% 
  filter(measure==2 & metric==1 & sex!=3 & rei==97 & age %in% age_id & location %in% countries_level_3$location_id) %>%
  arrange(val)

# Iron
iron <- dalys_fish_orig %>% 
  filter(measure==2 & metric==1 & sex!=3 & rei==95 & age %in% age_id & location %in% countries_level_3$location_id) %>% 
  arrange(val)




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

