

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "data/cosimo/processed"
ihmedir1 <- "/Users/cfree/Dropbox/BFA Data/IHME" 

# Read COSIMO country key
cntry_key_orig <- read.csv(file.path(datadir, "COSIMO_country_key.csv"), as.is=T)

# Read SDI info
sdi_orig <- readxl::read_excel(file.path(ihmedir1, "definitions", "IHME_GBD_2019_SDI_1990_2019_Y2020M10D15.XLSX"))
gbd_cntry_orig <- readxl::read_excel(file.path(ihmedir1, "definitions", "IHME_GBD_2017_GBD_LOCATIONS_HIERARCHY_Y2018M11D18.XLSX"))

# Read HDI info
hdi_orig <- readxl::read_excel(file.path(ihmedir1, "definitions", "human-development-index.xlsx"))


# Read data
################################################################################

# Formtat HDI
hdi <- hdi_orig %>% 
  janitor::clean_names() %>% 
  filter(year==2017) %>% 
  rename(hdi=human_development_index_undp, iso3=code) %>% 
  select(iso3, hdi)

# Format SDI
sdi <- sdi_orig %>% 
  # Reduce to 2017 SDI values (b/c that's what Alon uses)
  select(Location, "2017") %>% 
  rename(location=Location, sdi="2017") %>% 
  # Add location meta-data
  left_join(gbd_cntry_orig, by=c("location"="location_name")) %>%
  # Reduce to countries
  filter(level==3) %>%
  # Add ISO3
  mutate(iso3=countrycode(location, "country.name", "iso3c")) %>%
  select(iso3, location, sdi) %>% 
  # Remove Georgia the state
  filter(!(location=="Georgia" & sdi==0.838))

# Countries to fill
low_sdi_missing <- c("Congo - Brazzaville", "Swaziland", "Congo - Kinshasa", "Côte d’Ivoire", "Ethiopia PDR", "São Tomé & Príncipe", "São Tomé & Príncipe", "Western Sahara", "Eswatini")
mid_sdi_missing <- c("Netherlands Antilles", "St. Kitts & Nevis", "New Caledonia", "French Polynesia", "Tuvalu")
hi_sdi_missing <- c("United States", "United Kingdom", "Taiwan", "Belgium-Luxembourg", "Hong Kong SAR China", "Czechoslovakia", "North Macedonia",
                    "Macau SAR China", "Serbia and Montenegro", "Russia", "USSR", "Yugoslav SFR", "Macedonia", "Macao SAR China", "San Marino", "Palau")

# Add SDI to COSIMO countries
cntry_key <- cntry_key_orig %>% 
  left_join(sdi, by="iso3") %>% 
  select(-location) %>% 
  # Add SDI category
  mutate(sdi_group=cut(sdi, breaks=c(0,0.35,0.75,1), labels = c("low","middle","high")),
         sdi_group=as.character(sdi_group)) %>% 
  # Fill in missing categories
  mutate(sdi_group=ifelse(is.na(sdi) & country %in% low_sdi_missing, "low", sdi_group),
         sdi_group=ifelse(is.na(sdi) & country %in% mid_sdi_missing, "middle", sdi_group),
         sdi_group=ifelse(is.na(sdi) & country %in% hi_sdi_missing, "high", sdi_group)) %>% 
  # Add HDI
  left_join(hdi)

# Missing
sort(unique(cntry_key$country[is.na(cntry_key$sdi_group)]))
  
# Export data
################################################################################

# Export
saveRDS(cntry_key, file=file.path(datadir, "COSIMO_country_key_with_SDI_HDI_info.Rds"))


  
  
  
  
  
  
  


  