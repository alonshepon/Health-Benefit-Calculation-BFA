

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
                          ifelse(grepl("_w_ad_", filename), "women (adult)", 
                                 ifelse(grepl("_w_", filename), "women", 
                                        ifelse(grepl("_c_", filename), "children", "unknown"))))) %>% 
  # Add country
  mutate(country=ifelse(grepl("mexico", filename), "Mexico", ""),
         country=ifelse(grepl("usa", filename), "United States", country),
         country=ifelse(grepl("zambia", filename), "Zambia", country),
         country=ifelse(grepl("uganda", filename), "Uganda", country),
         country=ifelse(grepl("china", filename), "China", country),
         country=ifelse(grepl("lao", filename), "Laos", country),
         country=ifelse(grepl("phil", filename), "Philippines", country),
         country=ifelse(grepl("burkina", filename), "Burkina Faso", country),
         country=ifelse(grepl("italy", filename), "Italy", country)) %>% 
  # Add nutrient
  mutate(nutrient=gsub(".csv|_m_||_w_|_h_w_|_c_|_w_ad_|mexico|usa|zambia|uganda|phil|lao|china|italy|burkina", "", filename)) %>% 
  mutate(nutrient=recode(nutrient,
                         "b12"="Vitamin B-12",
                         "calc"="Calcium",
                         "iron"="Iron",
                         "omega_3"="Omega-3 fatty acids",
                         "processed_meat"="Processed meat",
                         "red_meat"="Red meat",
                         "vita"="Vitamin A",
                         "zinc"="Zinc")) %>% 
  # Add nutrient units
  mutate(nutrient_units=recode(nutrient, 
                               "Calcium"="mg/p/d",             
                               "Iron"="mg/p/d",                
                               "Omega-3 fatty acids"="g/p/d", # g of Eicosapentaenoic acid + Docosahexaenoic acid/person/day
                               "Processed meat"="g/p/d",       
                               "Red meat"="g/p/d",            
                               "Vitamin A"="µg/p/d",  # µg RAE/person/day         
                               "Vitamin B-12"="µg/p/d",        
                               "Zinc"="mg/p/d"))

# Loop through intake files
data_orig <- purrr::map_df(intake_files, function(x){
  
  # Read data and add filename
  sdata <- read.csv(file.path(inputdir, x), as.is=T)
  sdata1 <- sdata %>% 
    mutate(filename=x)
  
})

# Age group key
#5 1-4 years 
#6 5-9 years 
#7 10-14 years
#8 15-19 years
#9 20-24 years
#10 25-29 years
#11 30-34 years
#12 35-39 years
#13 40-44 years 
#14 45-49 years 
#15 50-54 years 
#16 55-59 years 
#17 60-64 years 
#18 65-69 years 
#19 70-74 years 
#20 75-79 years
#30 80-84 years
#31 85-89 years
#32 90-94 years
#33 95-99 years

# Age groups
age_group_lo <- c(0, seq(5,100,5))
age_group_hi <- c(seq(4,99,5), Inf)
age_groups <- paste(age_group_lo, age_group_hi, sep="-")
age_group_breaks <- c(-Inf,age_group_hi)
age_group_key <- tibble(age_yr=0:100,
                        age_group=cut(x=0:100, breaks=age_group_breaks, labels=age_groups)) %>% 
  mutate(age_group=recode(age_group, 
                          "100-Inf"="100+"),
         age_group_id=recode(age_group,
                              "0-4"="5",
                              "5-9"="6",
                              "10-14"="7",
                              "15-19"="8",
                              "20-24"="9",
                              "25-29"="10",
                              "30-34"="11",
                              "35-39"="12",
                              "40-44"="13",
                              "45-49"="14",
                              "50-54"="15",
                              "55-59"="16",
                              "60-64"="17",
                              "65-69"="18",
                              "70-74"="19",
                              "75-79"="20",
                              "80-84"="30",
                              "85-89"="31",
                              "90-94"="32",
                              "95-99"="33",
                              "100+"="34"))

# Format original data
data <- data_orig %>% 
  # Rename columns
  rename(id=X, age_yr=age, intake=HI) %>% 
  # Add country/nutrient/sex information
  left_join(file_key) %>% 
  # Add age group
  mutate(age_group=cut(age_yr, 
                       breaks=age_group_breaks,
                       labels=age_groups)) %>% 
  mutate(age_group=recode(age_group, "100-Inf"="100+")) %>% 
  left_join(age_group_key) %>% 
  # Arrange columns
  select(filename, country, nutrient, nutrient_units, sex, age_group_id, age_group, age_yr, id, intake) %>% 
  arrange(country, nutrient, sex, age_yr, id) %>% 
  # Remove un-needed data
  filter(nutrient!="Processed meat" & sex!="women (adult)")

# Inspect
freeR::complete(data)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outputdir, "habitual_nutrient_intakes_by_age_sex_9countries.Rds"))

