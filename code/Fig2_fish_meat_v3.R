

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "data/cosimo/raw"
outputdir <- "data/cosimo/old/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(outputdir, "COSIMO_2010_2030_food_by_scenario_cntry.Rds"))

# World
world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")


#  Build data
################################################################################

# Food groups
sort(unique(data_orig$food))

# Format data
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

# Format data
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

# Format data
chicken <- data_orig %>% 
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

# Format data
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

# Threshhold value
val <- 0.25

# Build data
data <- fish %>% 
  select(-food) %>% 
  # Add red meat
  left_join(meat %>% select(-c(country, food)), by="iso3") %>% 
  # Add chicken
  left_join(chicken %>% select(-c(country, food)), by="iso3") %>% 
  # Add chicken
  left_join(eggs %>% select(-c(country, food)), by="iso3") %>% 
  # Add dairy
  left_join(dairy %>% select(-c(country, food)), by="iso3") %>% 
  # Quantify directions
  mutate(fish_dir=ifelse(fish_pdiff > val, "up",
                         ifelse(fish_pdiff < -1*val, "down", "stable")),
         meat_dir=ifelse(meat_pdiff > val, "up",
                         ifelse(meat_pdiff < -1*val, "down", "stable")),
         poultry_dir=ifelse(poultry_pdiff > val, "up",
                            ifelse(poultry_pdiff < -1*val, "down", "stable")),
         eggs_dir=ifelse(eggs_pdiff > val, "up",
                          ifelse(eggs_pdiff < -1*val, "down", "stable")),
         dairy_dir=ifelse(dairy_pdiff > val, "up",
                         ifelse(dairy_pdiff < -1*val, "down", "stable")),
         dir_type=paste(fish_dir, meat_dir, poultry_dir, dairy_dir, sep="-"))

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

# Plot data
g1 <- ggplot(data_sf) +
  geom_sf(mapping=aes(fill=fish_pdiff), lwd=0.1, color="grey30") +
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
  theme(legend.position = c(0.10,0.37))
g1


# Plot meat
#############################

# Plot data
g2 <- ggplot(data_sf) +
  geom_sf(mapping=aes(fill=meat_pdiff), lwd=0.1, color="grey30") +
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
  theme(legend.position = c(0.10,0.37))
g2

# Plot poultry
#############################

# Plot data
g3 <- ggplot(data_sf) +
  geom_sf(mapping=aes(fill=poultry_pdiff), lwd=0.1, color="grey30") +
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
  theme(legend.position = c(0.10,0.37))
g3

# Plot dairy
#############################

# Plot data
g4 <- ggplot(data_sf) +
  geom_sf(mapping=aes(fill=eggs_pdiff), lwd=0.1, color="grey30") +
  # Labels
  labs(title="C. Egg consumption") +
  # Legend
  scale_fill_gradient2(name="% difference\nin 2030 intake\n(high vs. base)", 
                       midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               barwidth = 0.8, barheight = 2.7)) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.10,0.37))
g4

# Plot dairy
#############################

# Plot data
g5 <- ggplot(data_sf) +
  geom_sf(mapping=aes(fill=dairy_pdiff), lwd=0.1, color="grey30") +
  # Labels
  labs(title="C. Dairy consumption") +
  # Legend
  scale_fill_gradient2(name="% difference\nin 2030 intake\n(high vs. base)", 
                       midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",
                               barwidth = 0.8, barheight = 2.7)) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.10,0.37))
g5

# Plot difference
#############################

# Plot data
g6 <- ggplot(data_sf) +
  geom_sf(mapping=aes(fill=dir_type), lwd=0.1, color="grey30") +
  # Labels
  labs(title="E. Consumption categories") +
  # Legend
  scale_fill_discrete(name="High vs. base\n(AF higher in all)", na.translate=F) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.16,0.22), # 0.2 without title
        legend.key.size = unit(0.2, units="cm"))
g6



# Plot scatterplot
#############################

# Plot scatterplot
g7 <- ggplot(data_sf, mapping=aes(x=fish_pdiff, y=meat_pdiff, color=continent)) +
  geom_point(size=4) +
  # Lines
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  # Labels
  labs(x="% difference in fish consumption in 2030\nbetween high and base scenarios",
       y="% difference in red meat consumption in 2030\nbetween high and base scenarios") +
  # Legend
  scale_color_discrete(name="Continent") +
  # Theme
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
g7


# Merge plots
#############################

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, 
                             g3, g4, 
                             g5, g6, ncol=2)

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_fish_meat_poultry_eggs_dairy.png"), 
       width=6.5, height=4.5, units="in", dpi=600)


