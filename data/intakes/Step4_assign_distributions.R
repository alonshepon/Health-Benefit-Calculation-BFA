

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

# Read data
dists <- read.csv(file=file.path(outputdir, "habitual_nutrient_intakes_by_age_sex_13countries_distribution_fits.csv"), as.is=T) %>% 
  mutate(dist_id=paste(country_final, nutrient, sex, age_group, sep="-")) %>% 
  select(dist_id, everything()) %>% 
  filter(sex!="children")

# Age group coverage
################################################################################

# Age groups
sort(unique(dists$age_group))
age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
                "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99")

# Problem distributions
prob_dists <- c(paste("Burkina Faso-Calcium-men", age_groups, sep="-"),
                paste("Burkina Faso-Iron-men", age_groups, sep="-"),
                paste("Burkina Faso-Zinc-men", age_groups, sep="-"),
                paste("Burkina Faso-Zinc-women", age_groups, sep="-"),
                paste("Bolivia-Vitamin B-12-women", age_groups, sep="-"))

# Coverage
coverage <- dists %>% 
  # Remove ones without fits
  filter(!is.na(best_dist)) %>% 
  # Remove ones with poor distributions
  filter(!dist_id %in% prob_dists) %>% 
  # Map coverage
  group_by(dist_id, country_final, nutrient, sex, age_group) %>% 
  summarize(n=n()) %>% 
  # Factor age group
  mutate(age_group=factor(age_group, levels=age_groups))

# Plot coverage
g <- ggplot(coverage, aes(x=age_group, y=nutrient)) +
  facet_grid(country_final~sex) +
  geom_tile() +
  # Labels
  labs(x="Age (yr)", y="") +
  scale_fill_discrete(name="Data available?") +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Build key
################################################################################

# Read 9 country key
dist_key9 <- readxl::read_excel(file.path(outputdir, "dist_id_key_9countries.xlsx")) %>% 
  mutate(age_group=recode(age_group, 
                          "43960"="5-9", 
                          "44118"="10-14"))

# Expose gaps
dist_key <- dists %>% 
  # Create full list of distributions
  select(country_final, nutrient, sex, age_group) %>% 
  unique() %>% 
  complete(country_final, nutrient, sex, age_group) %>% 
  # Add key
  mutate(dist_id=paste(country_final, nutrient, sex, age_group, sep="-")) %>% 
  # Mark available/borrowed
  left_join(coverage) %>% 
  mutate(type=ifelse(!is.na(n), "available", "borrowed")) %>% 
  # Add distribution key to use from 9 country key
  left_join(dist_key9 %>% select(dist_id, dist_id_use, type2)) %>% 
  # Fill in gaps based on available data from new countries
  mutate(type2=ifelse(!is.na(n), "available", type2),
         dist_id_use=ifelse(type=="available", dist_id, dist_id_use)) %>% 
  # Arrange columns
  select(-n) %>% 
  select(country_final:type, type2, everything())

# Export for manual editting
write.csv(dist_key, file=file.path(outputdir, "dist_id_key.csv"), row.names=F)

# Read edited key
dist_key2 <- readxl::read_excel(file.path(outputdir, "dist_id_key.xlsx")) %>% 
  mutate(age_group=recode(age_group, 
                          "44325"="5-9", 
                          "44483"="10-14"),
         age_group=factor(age_group, levels=age_groups)) %>% 
  mutate(type2=recode(type2, "available"="Available"),
         sex=recode(sex, "men"="Males", "women"="Females"),
         country_label=recode(country_final, 
                              "Laos & Philippines"="Laos &\nPhilippines",
                              "Uganda & Zambia"="Uganda &\nZambia",
                              "Italy, Romania, Bulgaria"="Italy,\nRomania,\nBulgaria"))

# Plot
g <- ggplot(dist_key2 , aes(x=age_group, y=nutrient, fill=type2)) +
  facet_grid(country_label~sex) +
  geom_tile() +
  # Labels
  labs(x="Age (yr)", y="") +
  scale_fill_discrete(name="Data source") +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size=7),
        axis.title=element_text(size=8),
        legend.text = element_text(size=7),
        legend.title=element_text(size=8),
        strip.text = element_text(size=8))
g

# Plot key
ggsave(g, filename=file.path(plotdir,"FigSX_intake_age_sex_key_map.png"), 
       width=8.5, height=8.5, units="in", dpi=300)


# Stats for manuscript
################################################################################

table(dist_key$type)
sum(dist_key$type=="borrowed") / nrow(dist_key) *100

# Add distributions
################################################################################

# Add distributions
dist_key3 <- dist_key2 %>% 
  select(-country_label) %>% 
  left_join(dists %>% select(dist_id, best_dist:ln_ks), by=c("dist_id_use"="dist_id"))

# Inspect data
freeR::complete(dist_key3)

# Export 
saveRDS(dist_key3, file=file.path(outputdir, "intake_distributions_expanded_13countries.Rds"))




