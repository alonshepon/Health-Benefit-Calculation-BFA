

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "data/cosimo/raw/v4"
outputdir <- "data/cosimo/processed"
plotdir <- "data/cosimo/figures"

# Read data
data_lo_orig <- read.csv(file.path(inputdir, "NutrientsBaseRev2.csv"), as.is=T)
data_hi_orig <- read.csv(file.path(inputdir, "NutrientsScenRev2.csv"), as.is=T, skip=1)

# Read country key
eu27_key <- read.csv(file.path(outputdir, "COSIMO_AGLINK_2020_country_key.csv"), as.is=T) %>% 
  filter(group_code=="EUN") %>% 
  select(iso3, country, iso3_use, country_use) %>% 
  filter(country!="Czechoslovakia")


# Helper functions
################################################################################

# Function to extract info from long code
strsplit_extract <- function(x, split, which){
  
  split_escaped <- paste0("\\", split)
  vals <- purrr::map(x,  function(x){
    split_val <- strsplit(x, split_escaped)
    extracted_val <- split_val[[1]][which]
  })
  
  vals <- vals %>% unlist() %>% as.character()
  
  return(vals)
  
}

# Format data
################################################################################

# Format nutrient
data_lo <- data_lo_orig %>%
  # Rename columns
  rename(long_code=OUTPUT.0, country=countries, iso3="X", food=products, food_code="X.1", nutrient=elements, nutrient_code="X.2") %>% 
  # Rearrange columns
  select(iso3, country, food_code, food, nutrient_code, nutrient, everything()) %>% 
  select(-long_code) %>% 
  # Gather
  gather(key="year", value="value_lo", 7:ncol(.)) %>% 
  # Format year
  mutate(year=gsub("X|A", "", year) %>% as.numeric(),
         value_lo=value_lo %>% as.numeric()) 

# Format nutrient
data_hi <- data_hi_orig %>%
  # Rename columns
  rename(long_code=OUTPUT.0, country=countries, iso3="X", food=products, food_code="X.1", nutrient=elements, nutrient_code="X.2") %>% 
  # Rearrange columns
  select(iso3, country, food_code, food, nutrient_code, nutrient, everything()) %>% 
  select(-long_code) %>% 
  # Gather
  gather(key="year", value="value_hi", 7:ncol(.)) %>% 
  # Format year
  mutate(year=gsub("X|A", "", year) %>% as.numeric(),
         value_hi=value_hi %>% as.numeric()) 

# Merge data
data_orig <- data_lo %>% 
  left_join(data_hi %>% select(-country)) %>% 
  # Compute differences
  mutate(value_diff=value_hi-value_lo,
         value_pdiff=(value_hi-value_lo)/value_lo*100) %>% 
  # Fix nutrient values and units
  mutate(nutrient_orig=nutrient,
         nutrient=recode(nutrient_orig, 
                         "Calcium, Ca [mg/p/d]"="Calcium",        
                         "Energy [Kcal/p/d]"="Energy",                
                         "Iron, Fe [mg/p/d]"="Iron",                
                         "Monounsaturated fatty acids,  [g/p/d]"="Monounsaturated fatty acids",  
                         "Omega3 fatty acids [g/p/d]"="Omega-3 fatty acids",      
                         "Polyunsaturated fatty acids,  [g/p/d]"="Polyunsaturated fatty acids",   
                         "Protein [g/p/d]"="Protein",                  
                         "Saturated Fatty acids, total  [g/p/d]"="Saturated fatty acids",  
                         "Total lipid [g/p/d]"="Total lipids",              
                         "Vitamin A, [IU/p/g]"="Vitamin A",              
                         "Vitamin A, RAE [mg/p/d retinol]"="Vitamin A, RAE",   
                         "Vitamin B-12 [ug/p/d]"="Vitamin B-12",          
                         "Zinc, Zn [mg/p/d]"="Zinc"),
         nutrient_units=recode(nutrient_orig, 
                               "Calcium, Ca [mg/p/d]"="mg/p/d",        
                               "Energy [Kcal/p/d]"="Kcal/p/d",                
                               "Iron, Fe [mg/p/d]"="mg/p/d",                
                               "Monounsaturated fatty acids,  [g/p/d]"="g/p/d",  
                               "Omega3 fatty acids [g/p/d]"="g/p/d",      
                               "Polyunsaturated fatty acids,  [g/p/d]"="g/p/d",   
                               "Protein [g/p/d]"="g/p/d",                  
                               "Saturated Fatty acids, total  [g/p/d]"="g/p/d",  
                               "Total lipid [g/p/d]"="g/p/d",              
                               "Vitamin A, [IU/p/g]"="IU/p/d",              
                               "Vitamin A, RAE [mg/p/d retinol]"="mg/p/d retinol",   
                               "Vitamin B-12 [ug/p/d]"="ug/p/d",          
                               "Zinc, Zn [mg/p/d]"="mg/p/d")) %>% 
  select(-nutrient_orig) %>% 
  # Fix country
  mutate(iso3=ifelse(iso3=="", "EUN", iso3)) %>% 
  # Arrange
  select(iso3, country, food_code, food, nutrient_code, nutrient, nutrient_units, year, everything()) %>% 
  arrange(country, food, nutrient, year) #%>% 
  # Remove Total Food (which is missing in high road)
  # filter(food!="Total food")

# Inspect data
str(data_orig)
sort(unique(data_orig$nutrient))
sort(unique(data_orig$nutrient_units))
freeR::complete(data_orig)
range(data_orig$year)


# Build keys
################################################################################

# Nutrient key
nutr_key <- data_orig %>% 
  select(nutrient_code, nutrient, nutrient_units) %>% 
  unique() %>% 
  arrange(nutrient_code)

# Food key
food_key <- data_orig %>% 
  select(food_code, food) %>% 
  unique()

# Country key
cntry_key <- data_orig %>% 
  select(iso3, country) %>% 
  unique() %>% 
  arrange(iso3) %>% 
  mutate(country_use=countrycode(iso3, "iso3c", "country.name"),
         country_use=ifelse(is.na(country_use), country, country_use),
         iso3_use=countrycode(country_use, "country.name", "iso3c"),
         iso3_use=ifelse(is.na(iso3_use), iso3, iso3_use),
         iso3_use=ifelse(country%in%c("USSR", "Ethiopia PDR"), iso3, iso3_use),
         country_use=ifelse(country%in%c("USSR", "Ethiopia PDR"), country, country_use)) %>% 
  select(iso3, country, iso3_use, country_use) %>% 
  # Mark countries in EU27
  mutate(eu27=iso3_use %in% eu27_key$iso3_use)

# Any EU countries?
sum(cntry_key$eu27) # No

# Any duplictaed
freeR::which_duplicated(cntry_key$iso3_use)
freeR::which_duplicated(cntry_key$country_use)


# Add corrected country names
################################################################################

# Add corrected country to data
data1 <- data_orig %>% 
  # Add country names
  left_join(cntry_key %>% select(iso3, iso3_use, country_use), by="iso3") %>% 
  select(-c(iso3, country)) %>% 
  rename(country=country_use, iso3=iso3_use) %>% 
  select(iso3, country, everything())

# Inspect data
str(data1)
freeR::complete(data1)


# Check time series
################################################################################

# Global stats
gstats <- data1 %>% 
  drop_na() %>% 
  filter(iso3!="World") %>% 
  filter(food_code!="Total food") %>% 
  group_by(iso3, country, nutrient, year) %>% 
  summarize(value_lo=sum(value_lo, na.rm=T),
            value_hi=sum(value_hi, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(nutrient, year) %>% 
  summarize(value_lo=mean(value_lo, na.rm=T),
            value_hi=mean(value_hi, na.rm=T)) %>% 
  gather(key="scenario", value="intake", 3:ncol(.)) %>% 
  mutate(scenario=recode(scenario, "value_lo"="Base", "value_hi"="High"))

# Plot
g <- ggplot(gstats, aes(x=year, y=intake, color=scenario)) +
  geom_line() +
  facet_wrap(~nutrient, ncol=4, scales="free_y") +
  labs(x="", y="Mean intake") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        strip.text=element_text(size=8),
        plot.title=element_text(size=10),
        # Gridlines
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # Legend
        legend.position="bottom")
g


# Fix E27 countries
################################################################################

# Examine EU27 countries
data1_eu <- data1 %>% 
  filter(iso3 %in% c(eu27_key$iso3_use, "EUN"))

# Remove EU27 countries (including EUN)
data1_no_eu <- data1 %>% 
  filter(!iso3 %in% c(eu27_key$iso3_use, "EUN", "WLD"))

# Extract EUN data
data1_eun <- data1 %>% 
  filter(iso3=="EUN")

# Duplicate EUN data for each E27 country
##############################################

# Duplicate EUN data for each E27 country
data1_eu_use <- purrr::map_df(1:nrow(eu27_key), function(x) {
  
  iso_do <- eu27_key$iso3_use[x]
  country_do <- eu27_key$country_use[x]
  
  cdata <- data1_eun %>% 
    mutate(iso3=iso_do,
           country=country_do)
  
})

# Merge EU-expanded and non-EU data
data2 <- bind_rows(data1_no_eu, data1_eu_use) %>% 
  arrange(country, food, nutrient, year)

# Country key for export
cntry_key_out <- data2 %>% 
  select(iso3, country) %>% 
  unique() %>% 
  arrange(iso3) %>% 
  # Mark whether in EU27
  mutate(eu27=iso3%in%eu27_key$iso3)

# Export data
###########################################

# Export keys
write.csv(nutr_key, file=file.path(outputdir, "COSIMO_nutrient_key.csv"), row.names=F)
write.csv(food_key, file=file.path(outputdir, "COSIMO_food_product_key.csv"), row.names=F)
write.csv(cntry_key_out, file=file.path(outputdir, "COSIMO_country_key.csv"), row.names=F)

# Export data
saveRDS(data2, file=file.path(outputdir, "COSIMO_2010_2030_nutr_by_scenario_cntry_food.rds"))


# Visualize global trends
################################################################################

# Global stats
gstats <- data2 %>% 
  drop_na() %>% 
  filter(food=="Total food") %>% 
  group_by(nutrient, nutrient_units, year) %>% 
  summarize(value_lo=mean(value_lo, na.rm=T),
            value_hi=mean(value_hi, na.rm=T)) %>% 
  gather(key="scenario", value="intake", 4:ncol(.)) %>% 
  mutate(scenario=recode(scenario, "value_lo"="Base", "value_hi"="High"),
         nutrient_label=paste0(nutrient, " (", nutrient_units, ")"))

# Plot
g <- ggplot(gstats, aes(x=year, y=intake, color=scenario)) +
  geom_line() +
  facet_wrap(~nutrient_label, ncol=4, scales="free_y") +
  labs(x="", y="Mean intake") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        strip.text=element_text(size=8),
        plot.title=element_text(size=10),
        # Gridlines
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # Legend
        legend.position="bottom")
g

# Export
ggsave(g, filename=file.path(plotdir, "COSIMO_nutrient_check.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



