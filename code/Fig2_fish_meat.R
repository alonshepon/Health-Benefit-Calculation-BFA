

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

# Build data
data <- fish %>% 
  select(-food) %>% 
  left_join(meat %>% select(-c(country, food)), by="iso3") %>% 
  mutate(dir_type=paste(ifelse(fish_pdiff>0, "up", "down"), ifelse(meat_pdiff>0, "up", "down"), sep="-"),
         dir_type=recode(dir_type, 
                         "NA-down"="other",
                         "NA-up"="other",
                         "up-down"="Fish higher, meat lower",
                         "up-up"="Both higher"))

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
  labs(title="A. Fish consumption") +
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


# Plot difference
#############################

# Plot data
g3 <- ggplot(data_sf) +
  geom_sf(mapping=aes(fill=dir_type), lwd=0.1, color="grey30") +
  # Labels
  labs(title="C. Fish and red meat consumption") +
  # Legend
  scale_fill_manual(name="High vs. base", na.translate=F, values=c(RColorBrewer::brewer.pal(9, "RdBu")[7], 
                                                      RColorBrewer::brewer.pal(9, "RdBu")[2])) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.15,0.15),
        legend.key.size = unit(0.2, units="cm"))
g3

# Plot scatterplot
#############################

# Plot scatterplot
g4 <- ggplot(data_sf, mapping=aes(x=fish_pdiff, y=meat_pdiff, color=continent)) +
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
g4


# Merge plots
#############################

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=3)

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_fish_meat_diff.png"), 
       width=3.5, height=4.75, units="in", dpi=600)


