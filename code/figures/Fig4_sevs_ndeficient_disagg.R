

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outputdir <- "output"
plotdir <- "figures"

# Read problem country key
prob_key <- read.csv("data/countries_with_bug.csv", as.is=T)

# Read data
sevs <- readRDS(file.path(outputdir, "2030_sevs_base_high_road_final_diversity_disagg.Rds")) %>% 
  mutate(sev_delta_cap=pmin(sev_delta, 2) %>% pmax(., -2)) %>% 
  # Rename Vitamin A
  mutate(nutrient=recode(nutrient, 
                         # "Vitamin A, RAE"="Vitamin A",
                         "Omega-3 fatty acids"="DHA+EPA",
                         "Vitamin B-12"="Vitamin B12")) %>% 
  # Eliminate problem countries
  filter(!iso3 %in% prob_key$iso)

# Read data
data_orig <- readRDS(file=file.path(outputdir, "2030_ndeficient_base_high_diversity_disagg.Rds")) %>% 
  mutate(nutrient=recode(nutrient, 
                         "Omega-3 fatty acids"="DHA+EPA",
                         "Vitamin B-12"="Vitamin B12")) %>% 
  # Eliminate problem countries
  filter(!iso3 %in% prob_key$iso)

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


# Build data
################################################################################

# Format ndeficient
###################################

# Map stats
stats1 <- data_orig %>% 
  group_by(nutrient, iso3, country) %>% 
  summarize(npeople=sum(ndeficient_diff, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(npeople_cap=pmax(npeople, -1*1e6) %>% pmin(., 1*1e6))

hist(stats1$npeople/1e6, breaks=seq(-35,5, 0.5))
abline(v=-1)

# Raster stats
stats2 <- data_orig %>% 
  # Calculate npeople by nutrient-sex-age
  group_by(nutrient, sex, age_group) %>% 
  summarize(npeople=sum(ndeficient_diff, na.rm=T)) %>% 
  ungroup() %>% 
  # Complete set to fill NAs
  complete(nutrient, sex, age_group) %>% 
  # Recode nutrients
  mutate(nutrient=recode(nutrient, "Vitamin A, RAE"="Vitamin A")) %>% 
  mutate(nutrient=factor(nutrient,
                         levels=c("Vitamin A", "Calcium", "Zinc", "Iron", "Vitamin B12", "DHA+EPA"))) %>%
  # Remove 100 group
  filter(age_group!="100+")


# Format SEVs
###################################

# Nutrient order
nutrients <- c("DHA+EPA", "Vitamin B12", "Iron", "Zinc", "Calcium", "Vitamin A, RAE")

# Calculate country-level means
c_avgs <- sevs %>% 
  # Remove NAs
  filter(!is.na(sev_delta)) %>%
  # Calculate country means
  group_by(nutrient, iso3, country) %>% 
  summarize(sev_delta_avg=mean(sev_delta, na.rm=T)) %>% 
  ungroup() %>% 
  # Order nutrients
  mutate(nutrient=factor(nutrient, levels=nutrients)) %>% 
  # Add caps
  mutate(cap=recode(nutrient, 
                    "DHA+EPA"=-5, 
                    "Vitamin B12"=-1,
                    "Iron"=-0.75,
                    "Zinc"=-0.6,
                    "Calcium"=-1,
                    "Vitamin A, RAE"=-0.5) %>% as.numeric(.),
         sev_delta_avg_cap=pmax(sev_delta_avg, cap))

# Inspect
g <- ggplot(c_avgs, aes(x=sev_delta_avg_cap)) +
  facet_wrap(~nutrient, scales="free_x") +
  geom_histogram() +
  theme_bw()
g

# Set breaks and labels
breaks_list <- list("DHA+EPA"=seq(-5, 0, 1),
                    "Vitamin B12"=seq(-1, 0, 0.25),
                    "Iron"=seq(-0.75, 0, 0.25),
                    "Zinc"=seq(-0.6, 0.2, 0.2),
                    "Calcium"=seq(-1, 0.5, 0.5),
                    "Vitamin A, RAE"=seq(-0.5, 1.5, 0.5))

labels_list <- list("DHA+EPA"=c("≤ -5", "-4", "-3", "-2", "-1", "0"),
                    "Vitamin B12"=c("≤ -1.0", "-0.75", "-0.50", "-0.25", "0.0"),
                    "Iron"=c("≤ -0.75", "-0.50", "-0.25", "0.00"),
                    "Zinc"=c("≤ -0.6", "-0.4", "-0.2", "0.0", "0.2"),
                    "Calcium"=c("≤ -1.0", "-0.5", "0.0", "0.5"),
                    "Vitamin A, RAE"=c("-0.5", "0.0", "0.5", "1.0", "1.5"))




# Plotting functions
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
                    # Legend
                    legend.position=c(0.11,0.35),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Function to plot map: nutrient <- "Calcium"
plot_map <- function(nutrient){
  
  # Format data
  nutr_do <- nutrient
  c_avgs_sf <- world %>% 
    left_join(c_avgs %>% filter(nutrient==nutr_do), by=c("gu_a3"="iso3"))
  
  # Spatialize tiny
  sdata_pt <- world_centers %>% 
    left_join(c_avgs %>% filter(nutrient==nutr_do), by=c("iso3"="iso3")) %>% 
    # Reduce to ones with data
    filter(!is.na(sev_delta_avg_cap)) %>% 
    arrange(area_sqkm) %>% 
    # Reduce to small
    filter(area_sqkm<=2.5*10^4 & continent!="Europe")
  
  # Get breaks and labels
  breaks <- breaks_list[[nutr_do]]
  labels <- labels_list[[nutr_do]]
  
  # Build title
  nutr_do_use <- nutr_do
  if(nutr_do=="Vitamin A, RAE"){
    nutr_do_use <- "Vitamin A"
  }
  if(nutr_do=="Vitamin B12"){
    nutr_do_use <- expression("Vitamin B"["12"])
  }
  
  # Plot
  g <- ggplot(c_avgs_sf) +
    geom_sf(mapping=aes(fill=sev_delta_avg_cap), lwd=0.1) +
    # Plot small places
    geom_sf(data=sdata_pt, mapping=aes(fill=sev_delta_avg_cap), shape=21, size=0.9, stroke=0.3) +
    # Plot French Guiana
    geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    labs(title=nutr_do_use) +
    scale_fill_gradient2(name="ΔSEVs (%)\n(high - base)", 
                         breaks=breaks, labels=labels,
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
    # Theme
    theme_bw() + base_theme +
    theme(legend.position=c(0.08,0.37),
          axis.text = element_blank(),
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)))
  g
  
}


# Plot #1
################################################################################

# Build maps
g1 <- plot_map("DHA+EPA")
g2 <- plot_map("Vitamin B12")
g3 <- plot_map("Iron")
g4 <- plot_map("Zinc")
g5 <- plot_map("Calcium")
g6 <- plot_map("Vitamin A, RAE")

# Merge maps
g <- gridExtra::grid.arrange(g1, g2,
                             g3, g4,
                             g5, g6, ncol=2)


# Plot #2
################################################################################

# Plot raster
g7 <- ggplot(stats2, aes(x=age_group, y=nutrient, fill=npeople/1e6)) +
  facet_wrap(~sex) +
  geom_raster() +
  # Labels
  labs(x="Age group", y="") +
  scale_fill_gradient2(name="ΔMillions of people\nwith inadequate intakes\n(high - base)",
                       midpoint=0, low="navy", mid="white", high="darkred", na.value="grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black",  barwidth = 1, barheight = 4)) +
  # Y-axis values
  scale_y_discrete(labels=c("Vitamin A", "Calcium", "Zinc", "Iron", expression("Vitamin B"["12"]), "DHA+EPA")) +
  # Theme
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=5),
        axis.title=element_text(size=7),
        legend.text=element_text(size=5),
        legend.title=element_text(size=6),
        strip.text=element_text(size=6),
        plot.title=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "right")
g7

# Merge maps
plot2 <- gridExtra::grid.arrange(g, g7, nrow=2, heights=c(0.75,0.25))

# Export
ggsave(plot2, filename=file.path(plotdir, "Fig4_sevs_ndeficient_disagg.pdf"), 
       width=6.5, height=6.2, units="in", dpi=600)
