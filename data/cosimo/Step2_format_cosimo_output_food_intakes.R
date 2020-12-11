

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
food_lo_orig <- read.csv(file.path(inputdir, "FoodConsumptionBase.csv"), as.is=T)
food_hi_orig  <- read.csv(file.path(inputdir, "FoodConsumptionScenario.csv"), as.is=T)

# Read country key
cntry_key_cosimo <- read.csv(file.path(outputdir, "COSIMO_country_key.csv"), as.is=T)


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

# Format low road
food_lo <- food_lo_orig %>% 
  rename(code_long=X, country=countries, food=products, nutrient=elements, country_id=country_codes, iso3=X.1,
         food_id=product_codes, food_code=X.2, nutrient_id=ele_codes, code_end=X.3) %>% 
  # Gather year
  gather(key="year", value="value_lo", 11:ncol(.)) %>% 
  # Format year
  mutate(year=year %>% gsub("X_", "", .) %>% as.numeric()) %>% 
  # Arrange
  select(country_id, iso3, country, 
         food_id, food_code, food, 
         nutrient_id, nutrient,
         year, value_lo)

# Format high road
food_hi <- food_hi_orig %>% 
  rename(code_long=X, country=countries, food=products, nutrient=elements, country_id=country_codes, iso3=X.1,
         food_id=product_codes, food_code=X.2, nutrient_id=ele_codes, code_end=X.3) %>% 
  # Gather year
  gather(key="year", value="value_hi", 11:ncol(.)) %>% 
  # Format year
  mutate(year=year %>% gsub("X_", "", .) %>% as.numeric()) %>% 
  # Arrange
  select(country_id, iso3, country, 
         food_id, food_code, food, 
         nutrient_id, nutrient,
         year, value_hi)

# Merge high and low road
food_merge <- left_join(food_lo, food_hi) %>% 
  # Add percentage
  mutate(value_diff=value_hi-value_lo,
         value_diff_perc=(value_hi-value_lo)/value_lo*100) %>% 
  # Arrange
  arrange(country, food, nutrient, year) 

  
# Build keys
################################################################################

# Food key
food_key <- food_merge %>% 
  select(food_id, food_code, food) %>% 
  unique() %>% 
  arrange(food_id)

# Country key
cntry_key <- food_merge %>% 
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
  mutate(eu27=iso3_use %in% cntry_key_cosimo$iso3[cntry_key_cosimo$eu_27==T])

# None of the EU27 countries are in here
sum(cntry_key$eu27)

# Any duplictaed
freeR::which_duplicated(cntry_key$iso3_use)
freeR::which_duplicated(cntry_key$country_use)


# Fix E27 countries
################################################################################

# Remove EU27 countries (including EUN)
food_no_eu <- food_merge %>% 
  filter(!iso3 %in% c(cntry_key_cosimo$iso3[cntry_key_cosimo$eu_27==T], "EUN"))

# Extract EUN data
food_eu <- food_merge %>% 
  filter(iso3=="EUN")


# Duplicate EUN data for each E27 country
##############################################

# EU27 countries
e27_do <- cntry_key_cosimo %>%
  filter(eu27) %>% 
  select(country_id, iso3, country)

# Duplicate EUN data for each E27 country
food_eu_use <- purrr::map_df(1:nrow(e27_do), function(x) {
  
  id_do <- e27_do$country_id[x]
  iso_do <- e27_do$iso3[x]
  country_do <- e27_do$country[x]
  
  cdata <- food_eu %>% 
    mutate(country_id=id_do,
           iso3=iso_do,
           country=country_do)
  
})

# Merge EU-expanded and non-EU data
food <- bind_rows(food_no_eu, food_eu_use) %>% 
  arrange(country, food, year)


# Export
################################################################################

# Export data
saveRDS(food, file=file.path(outputdir, "COSIMO_2010_2030_food_by_scenario_cntry.Rds"))




