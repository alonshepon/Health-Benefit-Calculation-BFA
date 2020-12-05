
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
key_orig <- read.csv(file.path(inputdir, "COSIMO_2020_country_key.csv"), as.is=T)

# Get world
world <- rnaturalearth::ne_countries("small", returnclass="sf")

# Intake data
intake_cntrys <- c("China", "Laos", "Mexico", "Philippines", "Uganda", "United States of America", "Zambia", "Italy")
world_intake <- world %>% 
  filter(geounit %in% intake_cntrys)
world_intake_centroid <- world_intake %>% 
  sf::st_centroid()


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
ggsave(g, filename=file.path(plotdir, "FigureS3_intake_data_un_subregion.png"), 
       width=6.5, height=4, units="in", dpi=600)


