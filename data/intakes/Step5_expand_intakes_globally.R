

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "data/intakes/processed"
outputdir <- "data/intakes/output"
plotdir <- "data/intakes/figures"

# Read COSIMO countries
cntry_key_orig <- read.csv("data/cosimo/processed/COSIMO_country_key.csv", as.is = T)
  
# Read data
dists <- readRDS(file.path(outputdir, "intake_distributions_expanded_13countries.Rds"))

# Get world
world <- rnaturalearth::ne_countries("large", returnclass="sf")
world_df <- world %>% 
  sf::st_drop_geometry()


# Assign each COSIMO country to a nutrient intake group
################################################################################

# Build intake group
cntry_key <- cntry_key_orig %>% 
  # Add UN subregion
  left_join(world_df %>% select(gu_a3, subregion), by=c("iso3"="gu_a3")) %>% 
  rename(subregion_un=subregion) %>% 
  # Fill missing subregions
  mutate(subregion_un=ifelse(is.na(subregion_un), country, subregion_un),
         subregion_un=recode(subregion_un, 
                              "Belgium-Luxembourg"="Western Europe",
                              "Czechoslovakia"="Eastern Europe",
                              "Ethiopia PDR"="Eastern Africa",
                              "Netherlands Antilles"="Caribbean",
                              "Serbia and Montenegro"="Southern Europe",
                              "USSR"="Eastern Europe",
                              "Yugoslav SFR"="Southern Europe",
                              "Palestinian Territories"="Western Asia",
                              "EU 27"="Western Europe",
                              "Western Sahara"="Northern Africa")) %>% 
  # Classify intake group based on UN subregion
  mutate(intake_group=recode(subregion_un,
                             "Antarctica"="N/A", 
                             "Australia and New Zealand"="United States", 
                             "Caribbean"="Mexico", 
                             "Central America"="Mexico",  
                             "Central Asia"="China",  
                             "Eastern Africa"="Uganda & Zambia", 
                             "Eastern Asia"="China", 
                             "Eastern Europe"="Italy, Romania, Bulgaria", 
                             "Melanesia"="Laos & Philippines",  
                             "Polynesia"="Laos & Philippines",
                             "Micronesia"="Laos & Philippines",
                             "Middle Africa"="Uganda & Zambia", 
                             "Northern Africa"="Italy, Romania, Bulgaria",  
                             "Northern America"="United States", 
                             "Northern Europe"="Italy, Romania, Bulgaria",  
                             "Seven seas (open ocean)"="N/A",  
                             "South America"="Bolivia", 
                             "South-Eastern Asia"="Laos & Philippines", 
                             "Southern Africa"="Uganda & Zambia",  
                             "Southern Asia"="China",  
                             "Southern Europe"="Italy, Romania, Bulgaria",  
                             "Western Africa"="Burkina Faso",  
                             "Western Asia"="Italy, Romania, Bulgaria", 
                             "Western Europe"="Italy, Romania, Bulgaria")) %>% 
  # Make a few manual corrections
  mutate(intake_group=ifelse(country=="Sudan", "Uganda & Zambia", intake_group))


# Build expanded distribution key
################################################################################

# Build expanded distribution key
dist_key_exp <- purrr::map_df(1:nrow(cntry_key), function(x){
  
  # Country do
  iso3_do <- cntry_key$iso3[x]
  name_do <- cntry_key$country[x]
  group_do <- cntry_key$intake_group[x]
  
  # Get distributions from intake group and add meta-data
  cdist <- dists %>% 
    # Reduce to intake group
    filter(country_final==group_do) %>% 
    rename(intake_group=country_final) %>% 
    # Add meta-data
    mutate(country_iso3=iso3_do,
           country_name=name_do) %>% 
    # Arrange columns
    select(country_iso3, country_name, intake_group, everything())
  
})

# Inspect
freeR::complete(dist_key_exp)
n_distinct(dist_key_exp$country_iso3)

# Export key
saveRDS(dist_key_exp, file=file.path(outputdir, "intake_distributions_for_all_cosimo_countries.Rds"))




