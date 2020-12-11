

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(readxl)
library(tidyverse)

# Directories (outside repository)
ihmedir <- "/Users/cfree/Dropbox/Health Benefits calculations/Data/IHME" # Chris Free's computer

# Directories (in repository)
outputdir <- "output"
plotdir <- "figures"
codedir <- "code"

# Read data
sdi_orig <- readxl::read_excel(file.path(ihmedir, "definitions", "IHME_GBD_2019_SDI_1990_2019_Y2020M10D15.XLSX"))
hdi_orig <- readxl::read_excel(file.path(ihmedir, "definitions", "human-development-index.xlsx"))
country_key_orig <- readxl::read_excel(file.path(ihmedir, "definitions", "IHME_GBD_2017_GBD_LOCATIONS_HIERARCHY_Y2018M11D18.XLSX"))


# Build data
################################################################################

# Files to merge
my_files <- list.files(file.path(ihmedir, "excels/meat"), pattern="*.csv")

# Merge files
data1 <- purrr::map_df(my_files, function(x){
  fdata <- read.csv(file.path(ihmedir, "excels/meat", x))
})

# Format country info for merge
country_key <- country_key_orig %>% 
  select(location_id, location_name)

# Format 2017 HDI data
hdi <- hdi_orig %>% 
  janitor::clean_names("snake") %>% 
  rename(hdi=human_development_index_undp) %>% 
  filter(year==2017) %>%
  select(entity, hdi)

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

# Add 2017 HDI/SDI data to IHME data
data2 <- data1 %>% 
  # Add country id
  left_join(country_key, by=c("location"="location_id")) %>% 
  # Add HDI data
  left_join(hdi, by=c("location_name"="entity")) %>% 
  # Add SDI data
  left_join(sdi, by=c("location_name"="location"))


# Export data
################################################################################

# Export data
saveRDS(data2, file = file.path(outputdir, "my_meat_data.rds"))


