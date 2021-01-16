

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
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outputdir, "COSIMO_2010_2030_food_by_scenario_cntry.Rds"))

# World
world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")


# Extract French Guiana
fguiana <-world %>% 
  sf::st_cast(to="POLYGON") %>% 
  filter(gu_a3=="FRA") %>% 
  mutate(id=1:n()) %>% 
  select(id) %>% 
  filter(id==1)

# Country centroids
world_lg <- rnaturalearth::ne_countries(scale="large", returnclass = "sf") %>% 
  mutate(area_sqkm=sf::st_area(.)/(1000*1000)) %>%
  mutate(area_sqkm=as.numeric(area_sqkm)) %>% 
  sf::st_centroid() %>% 
  select(continent, subunit, su_a3, area_sqkm) %>% 
  rename(country=subunit, iso3=su_a3)

# Small nation centroids
world_tiny <- rnaturalearth::ne_countries(type="tiny_countries", returnclass = "sf") %>% 
  select(continent, subunit, su_a3) %>% 
  rename(country=subunit, iso3=su_a3) %>% 
  mutate(area_sqkm=10)

# Merge centroids
world_centers <- bind_rows(world_lg, world_tiny)

# Plot centroids
g <- ggplot(world_centers) +
  geom_sf(mapping=aes(size=area_sqkm))
g


#  Build data
################################################################################

# Food groups
sort(unique(data_orig$food))

# Fish
fish <- data_orig %>% 
  # Reduce to 2030
  filter(year==2030 & food=="Fish") %>% 
  # Simplify
  select(iso3, country, food, value_lo, value_hi, value_diff, value_diff_perc) %>% 
  # Rename
  rename(fish_lo=value_lo, 
         fish_hi=value_hi, 
         fish_diff=value_diff, 
         fish_pdiff=value_diff_perc)

# Red meat
meat <- data_orig %>% 
  # Reduce to 2030
  filter(year==2030 & food %in% c("Bovine", "Ovine", "Pork")) %>% 
  # Simplify
  select(iso3, country, food, value_lo, value_hi) %>% 
  # Summarize
  group_by(iso3, country) %>% 
  summarize(value_lo=sum(value_lo),
            value_hi=sum(value_hi)) %>% 
  ungroup() %>% 
  mutate(food="Red meat", 
         value_diff=value_hi-value_lo,
         value_diff_perc=(value_hi-value_lo)/value_lo*100) %>% 
  # Rename
  rename(meat_lo=value_lo, 
         meat_hi=value_hi, 
         meat_diff=value_diff, 
         meat_pdiff=value_diff_perc)

# Dairy
dairy <- data_orig %>% 
  # Reduce to 2030
  filter(year==2030 & food %in% c("Butter", "Milk and Dairy")) %>% 
  # Simplify
  select(iso3, country, food, value_lo, value_hi) %>% 
  # Summarize
  group_by(iso3, country) %>% 
  summarize(value_lo=sum(value_lo),
            value_hi=sum(value_hi)) %>% 
  ungroup() %>% 
  mutate(food="Dairy", 
         value_diff=value_hi-value_lo,
         value_diff_perc=(value_hi-value_lo)/value_lo*100) %>% 
  # Rename
  rename(dairy_lo=value_lo, 
         dairy_hi=value_hi, 
         dairy_diff=value_diff, 
         dairy_pdiff=value_diff_perc)

# Poultry
poultry <- data_orig %>% 
  # Reduce to 2030
  filter(year==2030 & food %in% c("Poultry")) %>% 
  # Simplify
  select(iso3, country, food, value_lo, value_hi) %>% 
  # Summarize
  group_by(iso3, country) %>% 
  summarize(value_lo=sum(value_lo),
            value_hi=sum(value_hi)) %>% 
  ungroup() %>% 
  mutate(food="Poultry", 
         value_diff=value_hi-value_lo,
         value_diff_perc=(value_hi-value_lo)/value_lo*100) %>% 
  # Rename
  rename(poultry_lo=value_lo, 
         poultry_hi=value_hi, 
         poultry_diff=value_diff, 
         poultry_pdiff=value_diff_perc)

# Eggs
eggs <- data_orig %>% 
  # Reduce to 2030
  filter(year==2030 & food %in% c("Eggs")) %>% 
  # Simplify
  select(iso3, country, food, value_lo, value_hi) %>% 
  # Summarize
  group_by(iso3, country) %>% 
  summarize(value_lo=sum(value_lo),
            value_hi=sum(value_hi)) %>% 
  ungroup() %>% 
  mutate(food="Eggs", 
         value_diff=value_hi-value_lo,
         value_diff_perc=(value_hi-value_lo)/value_lo*100) %>% 
  # Rename
  rename(eggs_lo=value_lo, 
         eggs_hi=value_hi, 
         eggs_diff=value_diff, 
         eggs_pdiff=value_diff_perc)

# Eggs
other <- data_orig %>% 
  # Reduce to 2030
  filter(year==2030 & food %in% c("Eggs", "Poultry", "Butter", "Milk and Dairy")) %>% 
  # Simplify
  select(iso3, country, food, value_lo, value_hi) %>% 
  # Summarize
  group_by(iso3, country) %>% 
  summarize(value_lo=sum(value_lo),
            value_hi=sum(value_hi)) %>% 
  ungroup() %>% 
  mutate(food="Eggs", 
         value_diff=value_hi-value_lo,
         value_diff_perc=(value_hi-value_lo)/value_lo*100) %>% 
  # Rename
  rename(other_lo=value_lo, 
         other_hi=value_hi, 
         other_diff=value_diff, 
         other_pdiff=value_diff_perc)

# Threshhold value
val <- 0.2

# Build data
data <- fish %>% 
  select(-food) %>% 
  # Add red meat
  left_join(meat %>% select(-c(country, food)), by="iso3") %>% 
  # Add poultry
  left_join(poultry %>% select(-c(country, food)), by="iso3") %>% 
  # Add eggs
  left_join(eggs %>% select(-c(country, food)), by="iso3") %>% 
  # Add dairy
  left_join(dairy %>% select(-c(country, food)), by="iso3") %>% 
  # Add poultry-eggs-dairy merged
  left_join(other %>% select(-c(country, food)), by="iso3") %>% 
  # Quantify directions
  mutate(fish_dir=ifelse(fish_pdiff > val, "up",
                         ifelse(fish_pdiff < -1*val, "down", "stable")),
         meat_dir=ifelse(meat_pdiff > val, "up",
                         ifelse(meat_pdiff < -1*val, "down", "stable")),
         other_dir=ifelse(other_pdiff > val, "up",
                         ifelse(other_pdiff < -1*val, "down", "stable")),
         dir_type=paste(meat_dir, other_dir, sep="-")) %>% 
  # Reclassify directions
  mutate(dir_type=recode(dir_type,
                         "down-up"="",
                         "down-down"="Decrease",
                         "down-stable"="Decrease",
                         "stable-down"="Decrease",
                         "stable-stable"="Stable",
                         "stable-up"="Increase",
                         "up-stable"="Increase",
                         "up-up"="Increase", 
                         "up-down"="")) %>% 
  mutate(dir_type=factor(dir_type, levels=c("Increase", "Stable", "Decrease")))

table(data$dir_type)


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_blank(),
                    axis.title=element_blank(),
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=5),
                    strip.text=element_blank(),
                    plot.title=element_text(size=7),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot fish
#############################

# Add expanded data to SF
data_sf <- world %>% 
  left_join(data, by=c("gu_a3"="iso3"))

# Spatialize tiny
sdata_pt <- world_centers %>% 
  left_join(data, by=c("iso3"="iso3")) %>% 
  arrange(area_sqkm) %>% 
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")

# Plot data
g1 <- ggplot(data_sf) +
  geom_sf(mapping=aes(fill=fish_pdiff), lwd=0.1, color="grey30") +
  # Plot small places
  geom_sf(data=sdata_pt %>% filter(!is.na(fish_pdiff)), mapping=aes(fill=fish_pdiff), shape=21, size=0.9, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Labels
  labs(title="A. Aquatic foods consumption") +
  # Legend
  scale_fill_gradient2(name="% difference\nin 2030 intake\n(high vs. base)",
                       midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               barwidth = 0.8, barheight = 2.7)) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.10,0.40))
g1


# Plot meat
#############################

# Plot data
g2 <- ggplot(data_sf) +
  geom_sf(mapping=aes(fill=meat_pdiff), lwd=0.1, color="grey30") +
  # Plot small places
  geom_sf(data=sdata_pt %>% filter(!is.na(meat_pdiff)), mapping=aes(fill=meat_pdiff), shape=21, size=0.9, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Labels
  labs(title="B. Red meat consumption") +
  # Legend
  scale_fill_gradient2(name="% difference\nin 2030 intake\n(high vs. base)", 
                       midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               barwidth = 0.8, barheight = 2.7)) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.10,0.40))
g2

# Plot poultry
#############################

# Plot data
g3 <- ggplot(data_sf) +
  geom_sf(mapping=aes(fill=poultry_pdiff), lwd=0.1, color="grey30") +
  # Plot small places
  geom_sf(data=sdata_pt %>% filter(!is.na(poultry_pdiff)), mapping=aes(fill=poultry_pdiff), shape=21, size=0.9, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Labels
  labs(title="C. Poultry consumption") +
  # Legend
  scale_fill_gradient2(name="% difference\nin 2030 intake\n(high vs. base)", 
                       midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               barwidth = 0.8, barheight = 2.7)) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.10,0.40))
g3

# Plot dairy
#############################

# Plot data
g4 <- ggplot(data_sf) +
  geom_sf(mapping=aes(fill=eggs_pdiff), lwd=0.1, color="grey30") +
  # Plot small places
  geom_sf(data=sdata_pt %>% filter(!is.na(eggs_pdiff)), mapping=aes(fill=eggs_pdiff), shape=21, size=0.9, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Labels
  labs(title="D. Egg consumption") +
  # Legend
  scale_fill_gradient2(name="% difference\nin 2030 intake\n(high vs. base)", 
                       midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               barwidth = 0.8, barheight = 2.7)) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.10,0.40))
g4

# Plot dairy
#############################

# Plot data
g5 <- ggplot(data_sf) +
  geom_sf(mapping=aes(fill=dairy_pdiff), lwd=0.1, color="grey30") +
  # Plot small places
  geom_sf(data=sdata_pt %>% filter(!is.na(dairy_pdiff)), mapping=aes(fill=dairy_pdiff), shape=21, size=0.9, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Labels
  labs(title="E. Dairy consumption") +
  # Legend
  scale_fill_gradient2(name="% difference\nin 2030 intake\n(high vs. base)", 
                       midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               barwidth = 0.8, barheight = 2.7)) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.10,0.40))
g5

# Plot difference
#############################

# Plot data
g6 <- ggplot(data_sf) +
  geom_sf(mapping=aes(fill=dir_type), lwd=0.1, color="grey30") +
  # Plot small places
  geom_sf(data=sdata_pt %>% filter(!is.na(dir_type)), mapping=aes(fill=dir_type), shape=21, size=0.9, stroke=0.3, show.legend =F) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="white") +
  # Labels
  labs(title="F. Non-aquatic animal-sourced food (ASF) consumption") +
  # Legend
  scale_fill_discrete(name="Non-Aquatic ASF\n(high vs. base)", na.translate=F) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.125,0.25), # 0.2 without title
        legend.key.size = unit(0.2, units="cm"), 
        legend.text=element_text(size=4.5))
g6


# Merge plots
#############################

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, 
                             g3, g4, 
                             g5, g6, ncol=2)

# Export
# figname <- paste0("Fig2_diet_changes_6panel", val, ".png")
figname <- paste0("Fig3_foods.png")
ggsave(g, filename=file.path(plotdir, figname), 
       width=6.5, height=4.5, units="in", dpi=600)


