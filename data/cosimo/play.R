



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
food_orig <- read.csv(file.path(inputdir, "FoodConsBaseRev.csv"), as.is=T, skip=1)
nutrients_orig <- read.csv(file.path(inputdir, "NutrientsBaseRev.csv"), as.is=T, skip=1)

# Format food data
################################################################################

# Format food
food <- food_orig %>% 
  # Gather years
  gather(key="year", value="intake", 8:ncol(.)) %>% 
  # Format years
  mutate(year=gsub("X|A", "", year) %>% as.numeric()) %>% 
  # Rename columns
  rename(iso3=X.1, food=products, food_code=X.2, garbage=X.3, country=countries) %>% 
  # Eliminate useless columns
  select(-c(garbage, X, elements)) %>% 
  # Arrange
  select(iso3, country, food_code, food, year, intake, everything())

# Inspect data
str(food)
freeR::complete(food)
range(food$year)
table(food$food_code)
table(food$food)

# Global stats
gstats <- food %>% 
  group_by(food_code, food, year) %>% 
  summarize(intake=mean(intake, na.rm=T))

# Plot
g <- ggplot(gstats, aes(x=year, y=intake)) +
  facet_wrap(~food, ncol=4, scales="free_y") +
  geom_line() +
  labs(x="Year", y="Mean intake (kg / person / year)") +
  theme_bw()
g
  
# Format nutrient data
################################################################################

# Format data
nutrients <- nutrients_orig %>% 
  # Gather years
  gather(key="year", value="intake", 8:ncol(.)) %>% 
  # Format years
  mutate(year=gsub("X|A", "", year) %>% as.numeric()) %>% 
  # Rename columns
  rename(long_code=OUTPUT.0, country=countries, iso3=X, food_code=X.1, nutrient_code=X.2, food=products,
         nutrient_orig=elements) %>% 
  # Rearrange
  select(iso3, country, food_code, food, nutrient_code, nutrient_orig, year, intake)
  
# Inspect data
str(nutrients)
freeR::complete(nutrients)
range(nutrients$year)
table(nutrients$food_code)
table(nutrients$food)
table(nutrients$nutrient_orig)

# Global stats
gstats <- nutrients %>% 
  filter(food!="Total food") %>%
  group_by(iso3, country, nutrient_code, nutrient_orig, year) %>% 
  summarize(intake=sum(intake, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(nutrient_code, nutrient_orig, year) %>% 
  summarize(intake=mean(intake, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(nutrient_orig=gsub(" \\[", "\n[", nutrient_orig))

# Plot
g <- ggplot(gstats, aes(x=year, y=intake)) +
  facet_wrap(~nutrient_orig, ncol=4, scales="free_y") +
  geom_line() +
  labs(x="Year", y="Mean intake") +
  theme_bw()
g

  


