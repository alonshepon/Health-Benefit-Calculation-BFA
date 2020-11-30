

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/nutrition/subnational_distributions/all_intakes" # On Chris Free's computer
outputdir <- "data/intakes/processed"
plotdir <- "data/intake/figures"


# Merge data
################################################################################

# Files
intake_files <- list.files(inputdir)

# File key
file_key <- tibble(filename=intake_files) %>% 
  # Add sex
  mutate(sex=ifelse(grepl("_m_",filename), "men", 
                          ifelse(grepl("_w_", filename), "women", "unknown"))) %>% 
  # Add country
  mutate(country=ifelse(grepl("mexico", filename), "Mexico", ""),
         country=ifelse(grepl("usa", filename), "United States", country),
         country=ifelse(grepl("zambia", filename), "Zambia", country),
         country=ifelse(grepl("uganda", filename), "Uganda", country)) %>% 
  # Add nutrient
  mutate(nutrient=gsub(".csv|_m_||_w_|_h_w_|mexico|usa|zambia|uganda", "", filename)) %>% 
  mutate(nutrient=recode(nutrient,
                         "b12"="Vitamin B-12",
                         "calc"="Calcium",
                         "iron"="Iron",
                         "omega_3"="Omega-3 fatty acids",
                         "processed_meat"="Processed meat",
                         "red_meat"="Red meat",
                         "vita"="Vitamin A",
                         "zinc"="Zinc"))

# Loop through intake files
data_orig <- purrr::map_df(intake_files, function(x){
  
  # Read data and add filename
  sdata <- read.csv(file.path(inputdir, x), as.is=T)
  sdata1 <- sdata %>% 
    mutate(filename=x)
  
})

# Format original data
data <- data_orig %>% 
  rename(id=X, age_yr=age, intake=HI) %>% 
  left_join(file_key) %>% 
  select(filename, country, nutrient, sex, age_yr, id, intake) %>% 
  arrange(country, nutrient, sex, age_yr, id)

# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outputdir, "habitual_nutrient_intakes_by_age_sex_4countries.Rds"))

