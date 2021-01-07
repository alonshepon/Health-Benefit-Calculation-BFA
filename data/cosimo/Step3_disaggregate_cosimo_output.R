
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

# Read COSIMO-GENUS I3O matching key
cosimo_genus_iso_key <- readxl::read_excel("tables/TableS2_cosimo_genus_countries.xlsx", skip=1) %>% 
  setNames(c("iso3_cosimo", "country_cosimo", "iso3_genus", "country_genus")) %>% 
  select(iso3_cosimo, iso3_genus)

# Read SPADE-derived scalars (for omega-3s and Vitamin B-12)
spade_scalars <- readRDS(file.path("data/intakes/output/intake_distribution_age_sex_scalars.Rds")) %>% 
  rename(iso3=country_iso3) %>% 
  mutate(sex=recode(sex, "men"="Males", "women"="Females"),
         age_group=as.character(age_group), 
         nutrient=recode(nutrient, "Vitamin A"="Vitamin A, RAE"))


# Map GENUS coverage
################################################################################

# Only if you want to
if(F){

  # GENUS coverage?
  genus_isos <- genus_orig %>% 
    group_by(iso3_use, country_use) %>% 
    summarize(data_yn=sum(!is.na(value_med))>0) %>% 
    ungroup()
  
  # COSIMO not in GENUS
  cosimo_isos <- sort(unique(cosimo_orig$iso3))
  cosimo_isos[!cosimo_isos%in%genus_isos$iso3_use]
  
  # Get world
  world <- rnaturalearth::ne_countries(scale="large", returnclass = "sf")
  
  # Add data
  genus_isos_sf <- world %>% 
    left_join(genus_isos, by=c("gu_a3"="iso3_use"))
  
  # Plot world
  g <- ggplot(genus_isos_sf) +
    geom_sf(mapping=aes(fill=data_yn), lwd=0.2) +
    ggrepel::geom_text_repel(data=genus_isos_sf, 
                             mapping=aes(label=gu_a3, color=data_yn, geometry=geometry), 
                             stat="sf_coordinates",
                             size=2) +
    labs(x="", y="") +
    scale_color_manual(name="Data availability", values = c("darkred", "blue"), na.value="black") +
    theme_bw() +
    theme(legend.position = "bottom")
  g
  
  # Export map
  ggsave(g, filename=file.path(plotdir, "GENUS_coverage_map.png"), 
         width=15, height=8.5, units="in", dpi=200)

}


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

# Expands scalar key to include 80+ components
################################################################################

# Not 80+
intake_scalars_no80 <- intake_scalars %>% 
  filter(age_group!="80+")

# 80+
intake_scalars80 <- intake_scalars %>% 
  filter(age_group=="80+")

# Expand 80+ data
plus_groups <- c("80-84", "85-89", "90-94", "95-99")
intake_scalars80_exp <- purrr::map_df(plus_groups, function(x){
  edata <-intake_scalars80 %>% mutate(age_group=x)
})

# Merge pre-80 and expanded 80+
intake_scalars_exp <- bind_rows(intake_scalars_no80, intake_scalars80_exp) %>% 
  arrange(iso3, nutrient, sex, age_group, scalar)


# COSIMO data to use
################################################################################

# COSIMO omega
cosimo_omega <- cosimo_orig %>% 
  # Reduce to total diet
  filter(food=="Fish" & nutrient=="Omega-3 fatty acids")
range(cosimo_omega$value_lo, na.rm = T)
range(cosimo_omega$value_hi, na.rm = T)

# COSIMO not-omega
cosimo_not_omega <- cosimo_orig %>% 
  # Reduce to total diet
  filter(food=="Total food" & nutrient!="Omega-3 fatty acids")

# Merge
cosimo_use <- bind_rows(cosimo_omega, cosimo_not_omega)

# Build key for fraction of nutrients
################################################################################

# Build country-sex-age key
cntry_sex_age_key <- expand.grid(iso3=sort(unique(cosimo_orig$iso3)),
                                 sex=c("Males", "Females"),
                                 age_group=sort(unique(spade_scalars$age_group))) %>% 
  arrange(iso3, sex, age_group)

# Build key
data <- cosimo_use %>% 
  # Remove comparison columns
  select(-c(value_diff, value_pdiff)) %>% 
  # Gather columns
  gather(key="scenario", value="value", 9:ncol(.)) %>% 
  mutate(scenario=recode(scenario, 
                         "value_lo"="Base",
                         "value_hi"="High road")) %>% 
  select(-c(food_code, food)) %>% 
  # Remove missing values
  filter(!is.na(value)) %>% 
  # Add GENUS nutrient name for scalar matching
  mutate(nutrient_genus=recode(nutrient, 
                               "Energy"="Calories",
                               "Total lipids"="Fat",
                               "Vitamin A, RAE"="Vitamin A")) %>% 
  # Add GENUS ISO3s for scalar matching
  left_join(cosimo_genus_iso_key, by=c("iso3"="iso3_cosimo")) %>% 
  mutate(iso3_genus=ifelse(is.na(iso3_genus), iso3, iso3_genus)) %>% 
  # Add sex/age group
  left_join(cntry_sex_age_key, by="iso3") %>% 
  # Add GENUS-derived scalars
  left_join(intake_scalars_exp, by=c("iso3_genus"="iso3", "nutrient_genus"="nutrient", "sex", "age_group")) %>% 
  rename(scalar_genus=scalar) %>% 
  # Add SPADE-derived scalars
  left_join(spade_scalars %>% select(iso3, nutrient, sex, age_group, scalar)) %>%
  rename(scalar_spade=scalar) %>% 
  # Select scalar
  mutate(scalar=ifelse(nutrient %in% c("Omega-3 fatty acids", "Vitamin B-12") | is.na(scalar_genus), scalar_spade, scalar_genus)) %>% 
  # Calculate sex-age means
  rename(mean_cntry=value) %>% 
  mutate(mean_group=mean_cntry * scalar) %>% 
  # Arrange output
  select(scenario, year, iso3, country,
         nutrient_code, nutrient, nutrient_units, sex, age_group,
         nutrient_genus, iso3_genus, scalar_genus, scalar_spade, scalar, 
         mean_cntry, mean_group, everything())

# Inspect data
# The only rows missing values are 2018-2030 values for countries that don't exist anymore
freeR::complete(data)

# Inspect missing
check <- data %>% 
  filter(is.na(scalar))
range(check$year)
sort(unique(check$nutrient))
sort(unique(check$country))



# Add the distribution fits
################################################################################

# Read fits
dists <- readRDS("data/intakes/output/intake_distributions_for_all_cosimo_countries.Rds") %>% 
  mutate(nutrient=recode(nutrient, "Vitamin A"="Vitamin A, RAE")) %>% 
  mutate(age_group=as.character(age_group))

# Add distribution fits
data1 <- data %>% 
  # Recode sex for merge
  # mutate(sex=recode(sex, "Females"="women", "Males"="men")) %>% 
  # Add distribution fits
  left_join(dists, by=c("iso3"="country_iso3", "nutrient", "sex", "age_group")) %>% 
  # Add means and differences
  mutate(g_mean=g_shape/g_rate,
         g_mean_diff=mean_group-g_mean) %>% 
  mutate(ln_mean=exp(ln_meanlog + ln_sdlog^2/2),
         ln_mean_diff=mean_group-ln_mean)

# Inspect
freeR::complete(data1)

# Check
# The only rows missing values shoudl be those for uninteresting nutrients
check2 <- data1 %>% 
  filter(is.na(best_dist))
sort(unique(check2$nutrient))

# Export
################################################################################

# Export
saveRDS(data1, file=file.path(outputdir, "COSIMO_2010_2030_country_nutrient_age_sex_means_and_distributions.Rds"))




