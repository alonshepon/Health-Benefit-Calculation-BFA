
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "data/cosimo/processed"
outputdir <- "data/intakes/output"
plotdir <- "data/intakes/figures"

# Read country key
key_orig <- read.csv(file.path(inputdir, "COSIMO_country_key.csv"), as.is=T)

# Get world
world <- rnaturalearth::ne_countries("small", returnclass="sf")

# Intake data
intake_cntrys <- c("China", "Laos", "Mexico", "Philippines", "Uganda", "United States of America", "Zambia", "Italy", "Burkina Faso", "Bulgaria", "Bolivia", "Romania", "Bangladesh")
world_intake <- world %>% 
  filter(geounit %in% intake_cntrys) 
world_intake_centroid <- world_intake %>% 
  sf::st_centroid()

# Extract French Guiana
fguiana <-world %>% 
  sf::st_cast(to="POLYGON") %>% 
  filter(gu_a3=="FRA") %>% 
  mutate(id=1:n()) %>% 
  select(id) %>% 
  filter(id==1)


 # Read data
################################################################################

sf1 <- world %>% 
  left_join(key_orig, by=c("gu_a3"="iso3"))

# Continent
g <- ggplot(sf1) +
  geom_sf(mapping=aes(fill=continent), color="grey30", lwd=0.2) +
  # Labels
  labs(title="Continent") +
  # Legend
  scale_fill_discrete(name="Continent") +
  # Theme
  theme_bw()
g

# U.N. region
g <- ggplot(sf1) +
  geom_sf(mapping=aes(fill=region_un), color="grey30", lwd=0.2) +
  # Labels
  labs(title="U.N. region") +
  # Legend
  scale_fill_discrete(name="U.N. region") +
  # Theme
  theme_bw()
g

# World bank region
g <- ggplot(sf1) +
  geom_sf(mapping=aes(fill=region_wb), color="grey30", lwd=0.2) +
  # Add points for intake countries
  geom_sf(data=world_intake_centroid, color="black", size=2) +
  # Labels
  labs(title="World Bank regions") +
  # Legend
  scale_fill_discrete(name="World Bank region") +
  # Theme
  theme_bw()
g

# U.N. subregion
g <- ggplot(sf1) +
  geom_sf(mapping=aes(fill=subregion), color="grey30", lwd=0.2) +
  # Add points for intake countries
  geom_sf(data=world_intake_centroid, color="black", size=2) +
  # Labels
  labs(title="U.N. subregions") +
  # Legend
  scale_fill_discrete(name="", guide=  guide_legend(ncol=5) ) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        strip.text=element_text(size=8),
        plot.title=element_text(size=10),
        # Gridlines
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = "bottom")
g

# Export plot
# ggsave(g, filename=file.path(plotdir, "FigSX_intake_data_un_subregion.png"),
#        width=6.5, height=4, units="in", dpi=600)



# Build intake map
################################################################################

# # World key
# intake_key_df <- world %>% 
#   sf::st_drop_geometry() %>% 
#   select(geounit, gu_a3, subregion) %>% 
#   rename(country=geounit, iso3=gu_a3) %>% 
#   # Classify based on UN subregion
#   mutate(intake_group=recode(subregion,
#                              "Antarctica"="N/A", 
#                              "Australia and New Zealand"="United States", 
#                              "Caribbean"="Mexico", 
#                              "Central America"="Mexico",  
#                              "Central Asia"="China",  
#                              "Eastern Africa"="Uganda & Zambia", 
#                              "Eastern Asia"="China", 
#                              "Eastern Europe"="Italy", 
#                              "Melanesia"="Laos & Philippines",  
#                              "Middle Africa"="Uganda & Zambia", 
#                              "Northern Africa"="Italy",  
#                              "Northern America"="United States", 
#                              "Northern Europe"="Italy",  
#                              "Seven seas (open ocean)"="N/A",  
#                              "South America"="Mexico", 
#                              "South-Eastern Asia"="Laos & Philippines", 
#                              "Southern Africa"="Uganda & Zambia",  
#                              "Southern Asia"="China",  
#                              "Southern Europe"="Italy",  
#                              "Western Africa"="Burkina Faso",  
#                              "Western Asia"="Italy", 
#                              "Western Europe"="Italy")) %>% 
#   # Make a few manual corrections
#   mutate(intake_group=ifelse(country=="Sudan", "Uganda & Zambia", intake_group))

# World key
intake_key_df <- world %>% 
  sf::st_drop_geometry() %>% 
  select(geounit, gu_a3, subregion) %>% 
  rename(country=geounit, iso3=gu_a3) %>% 
  # Classify based on UN subregion
  mutate(intake_group=recode(subregion,
                             "Antarctica"="N/A", 
                             "Australia and New Zealand"="United States", 
                             "Caribbean"="Mexico", 
                             "Central America"="Mexico",  
                             "Central Asia"="China",  
                             "Eastern Africa"="Uganda & Zambia", 
                             "Eastern Asia"="China", 
                             "Eastern Europe"="Italy, Romania, Bulgaria", 
                             "Melanesia"="Laos & Philippines",  
                             "Middle Africa"="Uganda & Zambia", 
                             "Northern Africa"="Italy, Romania, Bulgaria",  
                             "Northern America"="United States", 
                             "Northern Europe"="Italy, Romania, Bulgaria",  
                             "Seven seas (open ocean)"="N/A",  
                             "South America"="Bolivia", 
                             "South-Eastern Asia"="Laos & Philippines", 
                             "Southern Africa"="Uganda & Zambia",  
                             "Southern Asia"="Bangladesh",  
                             "Southern Europe"="Italy, Romania, Bulgaria",  
                             "Western Africa"="Burkina Faso",  
                             "Western Asia"="Italy, Romania, Bulgaria", 
                             "Western Europe"="Italy, Romania, Bulgaria")) %>% 
  # Make a few manual corrections
  mutate(intake_group=ifelse(country=="Sudan", "Uganda & Zambia", intake_group)) %>% 
  # Remove NA
  filter(intake_group!="N/A")

# Add to world and map
intake_key_sf <- world %>% 
  select(gu_a3) %>% 
  rename(iso3=gu_a3) %>% 
  left_join(intake_key_df) %>% 
  filter(!is.na(intake_group))

# Build UN subregion key
unsub <- world %>% 
  sf::st_buffer(.00001) %>% 
  group_by(subregion) %>% 
  summarize() %>% 
  sf::st_make_valid()

# Plot map
g <- ggplot(intake_key_sf) +
  geom_sf(mapping=aes(fill=intake_group), color="grey30", lwd=0.2) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.2, color="grey30", fill="white") +
  # Plot UN subregions
  geom_sf(data=unsub, fill=NA, color="black", lwd=0.3) +
  # Add points for intake countries
  geom_sf(data=world_intake_centroid, color="black", size=2) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Labels
  labs(title="Intake group") +
  # Legend
  scale_fill_discrete(name="Intake group", ) +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        strip.text=element_text(size=8),
        plot.title=element_text(size=10),
        # Gridlines
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g

ggsave(g, filename=file.path(plotdir, "FigSX_intake_data_map_assignment.png"), 
       width=6.5, height=3, units="in", dpi=600)


