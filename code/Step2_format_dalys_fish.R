
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories (outside repository)
ihmedir <- "/Users/cfree/Dropbox/Health Benefits calculations/Data/IHME" # Chris Free's computer

# Directories (in repository)
outputdir <- "output"
plotdir <- "figures"
codedir <- "code"

# Read country codes
country_codes_orig <- readxl::read_excel(file.path(ihmedir, "definitions", "IHME_GBD_2017_GBD_LOCATIONS_HIERARCHY_Y2018M11D18.XLSX"))

# Read HDI info
hdi_orig <- readxl::read_excel(file.path(ihmedir, "definitions", "human-development-index.xlsx"))

# Read SDI info
sdi_orig <- readxl::read_excel(file.path(ihmedir, "definitions", "IHME_GBD_2019_SDI_1990_2019_Y2020M10D15.XLSX"))

# THERE ARE PROBLEMS HERE - MULTIPLE MATCHES OF SDI

# Build data
################################################################################

# IHME CSVs to merge
ihme_csvs <- list.files(file.path(ihmedir, "excels"), pattern=".csv")

# Merge IHME CSVs
data1 <- purrr::map_df(ihme_csvs, function(x){
  fdata <- read.csv(file.path(ihmedir, "excels", x), as.is=T)
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
sdi <- sdi_orig %>% 
  # Columns of interest
  select(Location, "2017") %>% 
  # Rename columns
  rename(location=Location, sdi="2017") %>% 
  # Add category
  mutate(sdi_group=cut(sdi, breaks=c(0,0.35,0.75,1), labels = c("low","middle","high")))

# Add SDI/HDI to data
data2 <- data1 %>% 
  # Add location name
  left_join(country_codes_2col, by=c("location"="location_id")) %>% 
  # Add HDI
  left_join(hdi, by=c("location_name"="Entity")) %>% 
  # Add SDI (THIS ADDS COLUMNS - PROBEMATIC!!!!!!!!!!!!!!!!!!!!!!!!!!!)
  left_join(sdi, by=c("location_name"="location"))


# Export data
################################################################################

# Export data
saveRDS(data2, file = file.path(outputdir, "my_data.rds"))
saveRDS(country_codes_orig, file = file.path(outputdir, "countries_code.rds"))
saveRDS(country_codes_level3, file = file.path(outputdir, "countries_level3.rds"))

