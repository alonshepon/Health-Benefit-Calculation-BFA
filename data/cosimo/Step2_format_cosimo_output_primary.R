

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "data/cosimo/raw"
outputdir <- "data/cosimo/processed"
plotdir <- "data/cosimo/figures"

# Read data
data_lo_orig <- read.csv(file.path(inputdir, "BaseNutrients.csv"), as.is=T)
data_hi_orig <- read.csv(file.path(inputdir, "ScenarioNutrients.csv"), as.is=T)

# Read country key
eu27_key <- read.csv(file.path(outputdir, "COSIMO_AGLINK_2020_country_key.csv"), as.is=T) %>% 
  filter(group_code=="EUN") %>% 
  select(iso3, country, iso3_use, country_use)


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
  # Remove columns
  select(-c(X, X.3)) %>% 
  # Rename columns
  rename(country=countries, iso3=X.1, food=products, food_code=X.2, nutrient=elements, nutrient_code=ele_codes, food_id=product_codes, country_id=country_codes) %>% 
  # Rearrange columns
  select(country_id, iso3, country, food_id, food_code, food, nutrient_code, nutrient, everything()) %>% 
  # Gather
  gather(key="year", value="value_lo", 9:ncol(.)) %>% 
  # Format year
  mutate(year=gsub("X_", "", year) %>% as.numeric(),
         value_lo=ifelse(value_lo=="#N/A", NA, value_lo) %>% as.numeric())

# Format nutrient
data_hi <- data_hi_orig %>%
  # Remove columns
  select(-c(X, X.3)) %>% 
  # Rename columns
  rename(country=countries, iso3=X.1, food=products, food_code=X.2, nutrient=elements, nutrient_code=ele_codes, food_id=product_codes, country_id=country_codes) %>% 
  # Rearrange columns
  select(country_id, iso3, country, food_id, food_code, food, nutrient_code, nutrient, everything()) %>% 
  # Gather
  gather(key="year", value="value_hi", 9:ncol(.)) %>% 
  # Format year
  mutate(year=gsub("X_", "", year) %>% as.numeric(),
         value_hi=ifelse(value_hi=="#N/A", NA, value_hi) %>% as.numeric())

# Merge data
data_orig <- data_lo %>% 
  left_join(data_hi) %>% 
  # Compute differences
  mutate(value_diff=value_hi-value_lo,
         value_pdiff=(value_hi-value_lo)/value_lo*100) %>% 
  # Fix nutrient values and units
  mutate(nutrient_orig=nutrient,
         nutrient=recode(nutrient_orig, 
                         "Calcium, Ca [mg/p/d]"="Calcium",        
                         "Energy [Kcal/p/d]"="Energy",                
                         "Iron, Fe [mg/p/d]"="Iron",                
                         "Monounsaturated fatty acids, t"="Monounsaturated fatty acids",  
                         "Omega3 fatty acids [g/p/d]"="Omega-3 fatty acids",      
                         "Polyunsaturated fatty acids, t"="Polyunsaturated fatty acids",   
                         "Protein [g/p/d]"="Protein",                  
                         "Saturated Fatty acids, total ["="Saturated fatty acids",  
                         "Total lipid [g/p/d]"="Total lipids",              
                         "Vitamin A, [IU/p/g]"="Vitamin A",              
                         "Vitamin A, RAE [mg/p/d retinol"="Vitamin A, RAE",   
                         "Vitamin B-12 [ug/p/d]"="Vitamin B-12",          
                         "Zinc, Zn [mg/p/d]"="Zinc"),
         nutrient_units=recode(nutrient_orig, 
                               "Calcium, Ca [mg/p/d]"="mg/p/d",        
                               "Energy [Kcal/p/d]"="Kcal/p/d",                
                               "Iron, Fe [mg/p/d]"="mg/p/d",                
                               "Monounsaturated fatty acids, t"="g/p/d",  
                               "Omega3 fatty acids [g/p/d]"="g/p/d",      
                               "Polyunsaturated fatty acids, t"="g/p/d",   
                               "Protein [g/p/d]"="g/p/d",                  
                               "Saturated Fatty acids, total ["="g/p/d",  
                               "Total lipid [g/p/d]"="g/p/d",              
                               "Vitamin A, [IU/p/g]"="IU/p/d",              
                               "Vitamin A, RAE [mg/p/d retinol"="mg/p/d",   
                               "Vitamin B-12 [ug/p/d]"="ug/p/d",          
                               "Zinc, Zn [mg/p/d]"="mg/p/d")) %>% 
  select(-nutrient_orig) %>% 
  # Fix country
  mutate(iso3=ifelse(iso3=="", "EUN", iso3)) %>% 
  # Arrange
  select(country_id:nutrient, nutrient_units, everything()) %>% 
  arrange(country, food, nutrient, year)

# Inspect data
str(data_orig)
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
  select(food_id, food_code, food) %>% 
  unique() %>% 
  arrange(food_id)

# Country key
cntry_key <- data_orig %>% 
  select(country_id, iso3, country) %>% 
  unique() %>% 
  arrange(country_id) %>% 
  mutate(country_use=countrycode(iso3, "iso3c", "country.name"),
         country_use=ifelse(is.na(country_use), country, country_use),
         iso3_use=countrycode(country_use, "country.name", "iso3c"),
         iso3_use=ifelse(is.na(iso3_use), iso3, iso3_use),
         iso3_use=ifelse(country%in%c("USSR", "Ethiopia PDR"), iso3, iso3_use),
         country_use=ifelse(country%in%c("USSR", "Ethiopia PDR"), country, country_use)) %>% 
  select(country_id, iso3, country, iso3_use, country_use) %>% 
  # Mark countries in EU27
  mutate(eu27=iso3_use %in% eu27_key$iso3_use)

sum(cntry_key$eu27)

# Any duplictaed
freeR::which_duplicated(cntry_key$iso3_use)
freeR::which_duplicated(cntry_key$country_use)

# Czechoslovakia is not in the COSIMO output -- no need to duplicate it
eu27_key$country[!eu27_key$iso3 %in% cntry_key$iso3]


# Add corrected country names
################################################################################

# Add corrected country to data
data1 <- data_orig %>% 
  select(-c(country, iso3)) %>% 
  left_join(cntry_key %>% select(country_id, iso3_use, country_use)) %>% 
  rename(country=country_use, iso3=iso3_use) %>% 
  select(country_id, iso3, country, everything())

# Inspect data
str(data1)
freeR::complete(data1)


# Fix E27 countries
################################################################################

# Examine EU27 countries
data1_eu <- data1 %>% 
  filter(iso3 %in% c(eu27_key$iso3_use, "EUN"))

# Remove EU27 countries (including EUN)
data1_no_eu <- data1 %>% 
  filter(!iso3 %in% c(eu27_key$iso3_use, "EUN"))

# Extract EUN data
data1_eun <- data1 %>% 
  filter(iso3=="EUN")

# Duplicate EUN data for each E27 country
##############################################

# EU27 countries
e27_do <- cntry_key %>%
  # Reduce to EU countries
  filter(eu27==T) %>% 
  select(country_id, iso3_use, country_use)

# Duplicate EUN data for each E27 country
data1_eu_use <- purrr::map_df(1:nrow(e27_do), function(x) {
  
  id_do <- e27_do$country_id[x]
  iso_do <- e27_do$iso3_use[x]
  country_do <- e27_do$country_use[x]
  
  cdata <- data1_eun %>% 
    mutate(country_id=id_do,
           iso3=iso_do,
           country=country_do)
  
})

# Merge EU-expanded and non-EU data
data2 <- bind_rows(data1_no_eu, data1_eu_use) %>% 
  arrange(country, food, nutrient, year)

cntry_key_out <- data2 %>% 
  select(country_id, iso3, country) %>% 
  unique() %>% 
  arrange(country_id)

# Export data
###########################################

# Export keys
write.csv(nutr_key, file=file.path(outputdir, "COSIMO_nutrient_key.csv"), row.names=F)
write.csv(food_key, file=file.path(outputdir, "COSIMO_food_product_key.csv"), row.names=F)
write.csv(cntry_key_out, file=file.path(outputdir, "COSIMO_country_key.csv"), row.names=F)

# Export data
saveRDS(data2, file=file.path(outputdir, "COSIMO_2010_2030_nutr_by_scenario_cntry_food.rds"))

