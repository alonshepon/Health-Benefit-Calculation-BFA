

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
nutr_diff_orig <- read.csv(file.path(inputdir, "NutrientPercentageDifference.csv"), as.is=T)
food_lo_orig <- read.csv(file.path(inputdir, "FoodConsumptionBase.csv"), as.is=T)
food_hi_orig  <- read.csv(file.path(inputdir, "FoodConsumptionScenario.csv"), as.is=T)
nutr_abs_orig <- read.csv(file.path(inputdir, "NutrientsScen.csv"), as.is=T, skip=1)


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

# Format nutrient absolute values
################################################################################

# Format data
nutr_abs <- nutr_abs_orig %>% 
  # Gather columns
  gather(key="year", value="value", 5:ncol(.)) %>% 
  # Format year
  mutate(year=year %>% gsub("X|A", "", .) %>% as.numeric()) %>% 
  # Remove useless columns
  select(-c(Location, Type, Comment)) %>%
  # Rename
  rename(long_code=OUTPUT.0) %>% 
  # Extract code info
  mutate(iso3 = strsplit_extract(x=long_code, split="_", which=1),
         food_code = strsplit_extract(x=long_code, split="_", which=2),
         code3 = strsplit_extract(x=long_code, split="_", which=3),
         code4 = strsplit_extract(x=code3, split="..", which=1),
         code5 = strsplit_extract(x=code3, split="..", which=2),
         code6 = strsplit_extract(x=code3, split="..", which=3)) %>% 
  # Select
  select(-c(code3, code4, code6)) %>% 
  rename(nutrient_code=code5) %>% 
  select(long_code, iso3, food_code, nutrient_code, year, value, everything()) %>% 
  arrange(iso3, food_code, nutrient_code, year) %>% 
  # Remove empty rows
  filter(!is.na(year))

# Inspect data
str(nutr_abs)
sort(unique(nutr_abs$iso3))
sort(unique(nutr_abs$food_code))
sort(unique(nutr_abs$nutrient_code))

# Export formatted data
saveRDS(nutr_abs, file=file.path(outputdir, "COSIMO_2010_2030_abs_nutr_diff_by_food.Rds"))


# Format nutrition differences
################################################################################

# Format low road
food_lo <- food_lo_orig %>% 
  rename(code_long=X, country_orig=countries, food=products, nutrient=elements, country_id=country_codes, country_iso3=X.1,
         food_id=product_codes, food_code=X.2, nutrient_id=ele_codes, code_end=X.3) %>% 
  # Gather year
  gather(key="year", value="value_lo", 11:ncol(.)) %>% 
  # Format year
  mutate(year=year %>% gsub("X_", "", .) %>% as.numeric()) %>% 
  # Arrange
  select(country_id, country_iso3, country_orig, 
         food_id, food_code, food, 
         nutrient_id, nutrient,
         year, value_lo)

# Format high road
food_hi <- food_hi_orig %>% 
  rename(code_long=X, country_orig=countries, food=products, nutrient=elements, country_id=country_codes, country_iso3=X.1,
         food_id=product_codes, food_code=X.2, nutrient_id=ele_codes, code_end=X.3) %>% 
  # Gather year
  gather(key="year", value="value_hi", 11:ncol(.)) %>% 
  # Format year
  mutate(year=year %>% gsub("X_", "", .) %>% as.numeric()) %>% 
  # Arrange
  select(country_id, country_iso3, country_orig, 
         food_id, food_code, food, 
         nutrient_id, nutrient,
         year, value_hi)

# Merge high and low road
food <- left_join(food_lo, food_hi) %>% 
  # Add percentage
  mutate(value_diff=value_hi-value_lo,
         value_diff_perc=(value_hi-value_lo)/value_lo*100) %>% 
  # Arrange
  arrange(country_orig, food, nutrient, year)

# Country key
country_key1 <- food %>% 
  select(country_id, country_iso3, country_orig) %>% 
  unique() %>% 
  arrange(country_id) %>% 
  mutate(country=countrycode(country_iso3, "iso3c", "country.name")) %>% 
  mutate(country=ifelse(is.na(country), country_orig, country))

# Food key --- looks good
food_key1 <- food %>% 
  select(food_id, food_code, food) %>% 
  unique() %>% 
  arrange(food_id)

# Nutrient key --- looks good
nutr_key1 <- food %>% 
  select(nutrient_id, nutrient) %>% 
  unique() %>% 
  arrange(nutrient_id)

# Add corrected county to data
food_out <- food %>% 
  # Add corrected country name
  select(-country_orig) %>% 
  left_join(country_key %>% select(country_iso3, country)) %>% 
  # Arrange
  select(country_id, country_iso3, country, 
         food_id, food_code, food, 
         nutrient_id, nutrient,
         year, everything()) %>% 
  arrange(country, food, nutrient, year)


# Export data
saveRDS(food_out, file=file.path(outputdir, "COSIMO_2010_2030_food_consumption_differences.Rds"))


# Format nutrition differences
################################################################################

# Format data
nutr_diff <- nutr_diff_orig %>% 
  # Rename columns
  rename(code_long=X, country_orig=countries, food=products, nutrient_orig=elements, country_id=country_codes, country_iso3=X.1,
         food_id=product_codes, food_code=X.2, nutrient_id=ele_codes, code_end=X.3) %>% 
  # Gather year
  gather(key="year", value="perc_diff", 11:ncol(.)) %>% 
  # Format year
  mutate(year=year %>% gsub("X_", "", .) %>% as.numeric()) %>% 
  # Format percent difference
  mutate(perc_diff=perc_diff %>% gsub("#DIV/0!|#N/A|%", "", .) %>% as.numeric()) %>% 
  # Fix missing ISO3
  mutate(country_iso3=ifelse(country_iso3=="" & country_orig=="EU 27", "EUN", country_iso3)) %>% 
  # Format nutrient
  mutate(nutrient=recode(nutrient_orig, 
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
                               "Monounsaturated fatty acids, t"="unknown",  
                               "Omega3 fatty acids [g/p/d]"="g/p/d",      
                               "Polyunsaturated fatty acids, t"="unknown",   
                               "Protein [g/p/d]"="g/p/d",                  
                               "Saturated Fatty acids, total ["="unknown",  
                               "Total lipid [g/p/d]"="g/p/d",              
                               "Vitamin A, [IU/p/g]"="IU/p/g",              
                               "Vitamin A, RAE [mg/p/d retinol"="mg/p/d",   
                               "Vitamin B-12 [ug/p/d]"="ug/p/d",          
                               "Zinc, Zn [mg/p/d]"="mg/p/d")) %>% 
  # Arrange
  select(country_id, country_iso3, country_orig, 
         food_id, food_code, food, 
         nutrient_id, nutrient, nutrient_units,
         year, perc_diff)


# Inspect
freeR::complete(nutr_diff)
range(nutr_diff$year)
range(nutr_diff$perc_diff, na.rm=T)

# Country key
country_key <- nutr_diff %>% 
  select(country_id, country_iso3, country_orig) %>% 
  unique() %>% 
  arrange(country_id) %>% 
  mutate(country=countrycode(country_iso3, "iso3c", "country.name")) %>% 
  mutate(country=ifelse(is.na(country), country_orig, country))

# Food key
food_key <- nutr_diff %>% 
  select(food_id, food_code, food) %>% 
  unique() %>% 
  arrange(food_id)

# Nutrient key
nutr_key <- nutr_diff %>% 
  select(nutrient_id, nutrient, nutrient_units) %>% 
  unique() %>% 
  arrange(nutrient_id)

# Final data
nutr_diff_out <- nutr_diff %>% 
  # Add corrected country name
  select(-country_orig) %>% 
  left_join(country_key %>% select(country_iso3, country)) %>% 
  # Arrange
  select(country_id, country_iso3, country, 
         food_id, food_code, food, 
         nutrient_id, nutrient, nutrient_units,
         year, perc_diff) %>% 
  arrange(country, food, nutrient, year)


# Export data
saveRDS(nutr_diff_out, file=file.path(outputdir, "COSIMO_2010_2030_perc_nutr_diff_by_food.Rds"))

