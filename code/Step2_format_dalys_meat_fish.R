
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories (outside repository) -- Alon's computer
# ihmedir1 <- "d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/Data/IHME/" 
# ihmedir3 <- "d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/Data/IHME3/" 

# Directories (outside repository) -- Chris's computer
ihmedir1 <- "/Users/cfree/Dropbox/BFA Data/IHME" 
ihmedir3 <- "/Users/cfree/Dropbox/BFA Data/IHME3" 

# Directories (in repository)
outputdir <- "output"
plotdir <- "figures"
codedir <- "code"

# Read country codes
country_codes_orig <- readxl::read_excel(file.path(ihmedir1, "definitions", "IHME_GBD_2017_GBD_LOCATIONS_HIERARCHY_Y2018M11D18.XLSX"))

# Read HDI info
hdi_orig <- readxl::read_excel(file.path(ihmedir1, "definitions", "human-development-index.xlsx"))

# Read SDI info
sdi_orig <- readxl::read_excel(file.path(ihmedir1, "definitions", "IHME_GBD_2019_SDI_1990_2019_Y2020M10D15.XLSX"))


# Build data
################################################################################

# IHME CSVs to merge
ihme_csvs <- list.files(file.path(ihmedir3, ""), pattern=".csv")

# Merge IHME CSVs
data1 <- purrr::map_df(ihme_csvs, function(x){
  fdata <- read.csv(file.path(ihmedir3, "", x), as.is=T)
})

# Prepare country codes for merge
country_codes_2col <- country_codes_orig %>% 
  select(location_id, location_name)
country_codes_level3  <- country_codes_orig %>%
  filter(level==3)

# Prepare HDI data for merge
hdi <- hdi_orig %>% 
  filter(Year==2017)

# Prepare SDI data for merge
# Duplicated rows for "Georgia" (manually fixed) and "North Africa and Middle East" and "South Asia" (fixed with unique())
sdi <- sdi_orig %>% 
  # Unique
  unique() %>% 
  # Columns of interest
  select(Location, "2017") %>% 
  # Rename columns
  rename(location=Location, sdi="2017") %>% 
  # Add category
  mutate(sdi_group=cut(sdi, breaks=c(0,0.35,0.75,1), labels = c("low","middle","high"))) %>% 
  # Rename "Georgia" the state
  mutate(row_id=1:n(),
         location=ifelse(location=="Georgia" & row_id==105, "Georgia, USA", location)) %>% 
  select(-row_id)

# Add SDI/HDI to data
data2 <- data1 %>% 
  # Add location name
  left_join(country_codes_2col, by=c("location"="location_id")) %>% 
  # Add HDI
  left_join(hdi, by=c("location_name"="Entity")) %>% 
  # Add SDI
  left_join(sdi, by=c("location_name"="location"))


# Export data
################################################################################

# Export data  -- Chris's computer
saveRDS(data2, file = file.path(outputdir, "my_data.rds"))
saveRDS(country_codes_orig, file = file.path(outputdir, "countries_code.rds"))
saveRDS(country_codes_level3, file = file.path(outputdir, "countries_level3.rds"))

# Export data  -- Alon's computer
# saveRDS(data2, file = file.path(ihmedir, "my_data.rds"))
# saveRDS(country_codes_orig, file = file.path(ihmedir, "countries_code.rds"))
# saveRDS(country_codes_level3, file = file.path(ihmedir, "countries_level3.rds"))

