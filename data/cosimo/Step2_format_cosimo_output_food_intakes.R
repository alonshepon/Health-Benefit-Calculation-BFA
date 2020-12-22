

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
food_lo_orig <- readxl::read_excel(file.path(inputdir, "FoodConsNutriBaseScen.xlsx"), sheet=3)
food_hi_orig <- readxl::read_excel(file.path(inputdir, "FoodConsNutriBaseScen.xlsx"), sheet=4)

# Read country key
eu27_key <- read.csv(file.path(outputdir, "COSIMO_AGLINK_2020_country_key.csv"), as.is=T) %>% 
  filter(group_code=="EUN") %>% 
  select(iso3, country, iso3_use, country_use) %>% 
  filter(country!="Czechoslovakia")

# Note:
# Units are reported to be kg/p/day but are actually kg/p/year
# I convert them to g/p/day

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
  rename(code_long="...1", country=countries, food=products, nutrient=elements, iso3="...5",
         food_code="...6", code_end="...7") %>% 
  # Gather year
  gather(key="year", value="value_lo", 8:ncol(.)) %>% 
  # Format year
  mutate(year=year %>% gsub("_", "", .) %>% as.numeric()) %>% 
  # Arrange
  select(iso3, country, 
         food_code, food, 
         year, value_lo)

# Format high road
food_hi <- food_hi_orig %>% 
  rename(code_long="...1", country=countries, food=products, nutrient=elements, iso3="...5",
         food_code="...6", code_end="...7") %>% 
  # Gather year
  gather(key="year", value="value_hi", 8:ncol(.)) %>% 
  # Format year
  mutate(year=year %>% gsub("_", "", .) %>% as.numeric()) %>% 
  # Arrange
  select(iso3, country, 
         food_code, food, 
         year, value_hi)

# There is a super annoying bug where I have to split the data in half to join then merge after

# Countries
countries <- sort(unique(food_lo$country))
countries_a <- countries[1:83]
countries_b <- countries[1:83]

# Merge high and low road
food_merge_a <- food_lo %>% filter(country %in% countries_a) %>% 
  left_join(food_hi %>% filter(country %in% countries_a)) %>% 
  # Add percentage
  mutate(value_diff=value_hi-value_lo,
         value_diff_perc=(value_hi-value_lo)/value_lo*100) %>% 
  # Arrange
  arrange(country, food, year)

food_merge_b <- food_lo %>% filter(country %in% countries_b) %>% 
  left_join(food_hi %>% filter(country %in% countries_b)) %>% 
  # Add percentage
  mutate(value_diff=value_hi-value_lo,
         value_diff_perc=(value_hi-value_lo)/value_lo*100) %>% 
  # Arrange
  arrange(country, food, year)

# Merge
food_merge <- bind_rows(food_merge_a, food_merge_b)

# Inspect
freeR::complete(food_merge)
  
# Build keys
################################################################################

# Food key
food_key <- food_merge %>% 
  select(food_code, food) %>% 
  unique() %>% 
  arrange(food_code)

# Country key
cntry_key <- food_merge %>% 
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
  filter(!iso3 %in% c(eu27_key$iso3_use, "EUN"))

# Extract EUN data
food_eu <- food_merge %>% 
  filter(iso3=="EUN")


# Duplicate EUN data for each E27 country
##############################################

# Duplicate EUN data for each E27 country
food_eu_use <- purrr::map_df(1:nrow(eu27_key), function(x) {
  
  iso_do <- eu27_key$iso3_use[x]
  country_do <- eu27_key$country_use[x]
  
  cdata <- food_eu %>% 
    mutate(iso3=iso_do,
           country=country_do)
  
})

# Merge EU-expanded and non-EU data
food <- bind_rows(food_no_eu, food_eu_use) %>% 
  # Add corrected country
  mutate(country=countrycode(iso3, "iso3c", "country.name")) %>% 
  arrange(country, food, year) %>% 
  #Convert values from kg/p/year to g/p/day
  mutate(value_lo=value_lo*1000/365,
         value_hi=value_hi*1000/365,
         value_diff=value_diff*1000/365)

# Export
################################################################################

# Export data
saveRDS(food, file=file.path(outputdir, "COSIMO_2010_2030_food_by_scenario_cntry.Rds"))


# Visualize global trends
################################################################################

# Global stats
gstats <- food %>% 
  group_by(food, year) %>% 
  summarize(value_lo=mean(value_lo, na.rm=T),
            value_hi=mean(value_hi, na.rm=T)) %>% 
  gather(key="scenario", value="intake", 3:ncol(.)) %>% 
  mutate(scenario=recode(scenario, "value_lo"="Base", "value_hi"="High"))

# Plot
g <- ggplot(gstats, aes(x=year, y=intake, color=scenario)) +
  geom_line() +
  facet_wrap(~food, ncol=4, scales="free_y") +
  labs(x="", y="Mean intake") +
  theme_bw() +
  theme(legend.position = "bottom")
g



