
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories (outside repository)
genusdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/nutrition/nutrient_endowment/data/genus/processed/" # Chris Free's computer

# Directories (inside repository)
inputdir <- "data/cosimo/raw"
outputdir <- "data/cosimo/processed"
plotdir <- "data/cosimo/figures"

# Read COSIMO output
cosimo_orig <- readRDS(file.path(outputdir, "COSIMO_2010_2030_nutr_by_scenario_cntry_food.rds"))

# Read GENUS dataset
genus_orig <- readRDS(file.path(genusdir, "genus_nutrient_supplies_w_fort_by_age_sex_2011.Rds"))


# Map GENUS coverage
################################################################################

# GENUS coverage?
genus_isos <- genus_orig %>% 
  group_by(iso3_use) %>% 
  summarize(data_yn=sum(!is.na(value_med))>0)

# Get world
world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")

# Add data
genus_isos_sf <- world %>% 
  left_join(genus_isos, by=c("gu_a3"="iso3_use"))

# Plot world
g <- ggplot(genus_isos_sf) +
  geom_sf(mapping=aes(fill=data_yn)) +
  geom_sf_text(data=genus_isos_sf, mapping=aes(color=data_yn, label=iso3_use)) +
  theme_bw()
g


# Format GENUS for merge
################################################################################

# Expand GENUS to separate children into male/female
genus_c_f <- genus_orig %>% 
  filter(sex=="Children") %>% 
  mutate(sex="Females")
genus_c_m <- genus_orig %>% 
  filter(sex=="Children") %>% 
  mutate(sex="Males")
genus_adults <- genus_orig %>% 
  filter(sex!="Children")
genus_exp <- bind_rows(genus_c_f, genus_c_m, genus_adults) %>% 
  arrange(country_use, nutrient, sex, age_range)


# Build key for fraction of nutrients
################################################################################

# COSIMO ISO3s
cosimo_isos <- sort(unique(cosimo_orig$iso3))
cosimo_nutrs <- sort(unique(cosimo_orig$nutrient))

# GENUS nutrients
sort(unique(genus_exp$nutrient))
genus_nutr_use <- c("Calcium", # Calcium
                    "Calories", # Energy
                    "Iron", # Iron
                    "Monounsaturated fatty acids", # Monounsaturated fatty acids
                    # Omega-3 fatty acids not available in GENUS
                    "Polyunsaturated fatty acids", # Polyunsaturated fatty acids
                    "Protein", # Protein
                    "Saturated fatty acids", # Saturated fatty acids
                    "Fat", # Total lipids
                    "Vitamin A", # Vitamin A, RAE
                    "Vitamin B-12", # for Vitamin B6? B^is not available in GENUS
                    "Zinc") # Zinc

# Build scalar key
intake_scalars <- genus_exp %>% 
  # Compute scalar distribution of intake within country-sex-age groups
  group_by(iso3_use, country_use, nutrient, units_long, units_short) %>% 
  mutate(scalar=value_med/mean(value_med)) %>% 
  ungroup() %>% 
  # Reduce to useful data
  filter(!is.na(scalar)) %>% 
  # Simplify key
  select(iso3_use, nutrient, sex, age_range, scalar) %>% 
  # Rename for COSIMO output consistency
  rename(iso3=iso3_use, age_group=age_range)


# Build key for fraction of nutrients
################################################################################

# Build key
data <- cosimo_orig %>% 
  # Remove comparison columns
  select(-c(value_diff, value_pdiff)) %>% 
  # Gather columns
  gather(key="scenario", value="value", 11:ncol(.)) %>% 
  mutate(scenario=recode(scenario, 
                         "value_lo"="Base",
                         "value_hi"="High road")) %>% 
  # Reduce to total diet
  filter(food=="Total Diet") %>% 
  select(-c(food_id, food_code, food)) %>% 
  # Add GENUS nutrient name for scalar matching
  mutate(nutrient_genus=recode(nutrient, 
                               "Energy"="Calories",
                               "Total lipids"="Fat",
                               "Vitamin A, RAE"="Vitamin A",
                               "Vitamin B-12"="Vitamin B6",
                               "Omega-3 fatty acids"="Polyunsaturated fatty acids")) %>% 
  # Add GENUS ISO3s for scalar matching
  mutate(iso3_genus=recode(iso3,
                           "AFG"="PAK", # Afghanistan             Pakistan
                           "ANT"="USA", # Netherlands Antilles    
                           "BDI"="RWA", # Burundi                 Rwanda
                           "BLX"="BEL", # Belgium-Luxembourg      Belgium
                           "BMU"="USA", # Bermuda                 USA
                           "COD"="COG",  # Congo - Kinshasa       Congo
                           "COM"="MDG",  # Comoros                Madagascar
                           "CZ2"="USA",  # Czechoslovakia
                           "DMA"="USA",  # Dominica
                           "ET2"="USA",  # Ethiopia PDR
                           "EUN"="USA",  # EU 27
                           "GAB"="USA",  # Gabon
                           "HKG"="CHN",  # Hong Kong China
                           "KHM"="USA",  # Cambodia
                           "KIR"="USA",  # Kiribati
                           "KNA"="USA",  # St. Kitts & Nevis
                           "LBR"="USA",  # Liberia
                           "LSO"="USA",  # Lesotho
                           "MAC"="USA",  # Macau SAR China
                           "MMR"="USA",  # Myanmar
                           "OMN"="USA",  # Oman
                           "PNG"="USA",  # Papua New Guinea
                           "PRK"="USA",  # North Korea
                           "SLB"="USA",  # Solomon Islands
                           "SLE"="USA",  # Sierra Leone
                           "SOM"="USA",  # Somalia
                           "SRM"="USA",  # Serbia & Montenegro
                           "STP"="USA",  # Sao Tome & Principe
                           "SYC"="USA",  # Seychelles
                           "TCD"="USA",  # Chad
                           "TGO"="USA",  # Togo
                           "TKM"="USA",  # Turkmenistan
                           "TLS"="USA",  # Timor-Leste
                           "TWN"="USA",  # Taiwan
                           "UGA"="USA",  # Uganda
                           "USR"="RUS",  # USSR
                           "VNM"="USA",  # Vietname
                           "VUT"="USA",  # Vanuatu
                           "WSM"="USA",  # Samoa
                           "YUG"="USA",  # Yugoslavia
                           "ZMB"="USA")) %>%  # Zambia
  # Add fractions
  left_join(intake_scalars, by=c("iso3_genus"="iso3", "nutrient_genus"="nutrient")) %>% 
  # Calculate sex-age means
  rename(mean_cntry=value) %>% 
  mutate(mean_group=mean_cntry * scalar)

# Which combos missing data
check <- data %>% 
  group_by(iso3, country) %>% 
  summarize(n=sum(!is.na(scalar))) %>% 
  filter(n==0)





