

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
plotdir <- "data/cosimo/figures"

# Read data
data_orig <- readRDS(file.path(outputdir, "COSIMO_2010_2030_perc_nutr_diff_by_food.Rds"))

# Read country key
cntry_key <- read.csv(file.path(outputdir, "COSIMO_2020_country_key.csv"), as.is=T)

# Build E27 country key
e27_key <- cntry_key %>% 
  filter(group_name=="E27") %>% 
  mutate(country_use=countrycode::countrycode(iso3, "iso3c", "country.name")) %>% 
  mutate(country_use=ifelse(is.na(country_use), country, country_use)) %>% 
  select(iso3, country_use) %>%
  rename(country=country_use)

# World
world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")


#  Build data
################################################################################

# 1) Build EU, non-EU, and merge

# Build data
data_clean <- data_orig %>% 
  # Reduce to totals in 2030
  filter(food_code=="TOT" & year==2030) %>% 
  # Add nutrient label
  mutate(nutrient_label=paste0(nutrient, " (", nutrient_units, ")"))

# Build non-EU data
data1 <- data_clean %>% 
  filter(country_iso3!="EUN")

# Build EU data
data2 <- purrr::map_df(1:nrow(e27_key), function(x) {
  
  # Country
  iso3_do <- e27_key$iso3[x]
  country_do <- e27_key$country[x]
  
  # Country data
  cdata <- data_clean %>% 
    filter(country_iso3=="EUN") %>% 
    mutate(country_iso3=iso3_do,
           country=country_do, 
           country_id=NA)
  
})

# Merge EU and non-EU data
data <- bind_rows(data1, data2)

# Expand data to match map countries
data_expanded <- world %>% 
  sf::st_drop_geometry() %>% 
  # Reduce
  select(gu_a3) %>% 
  # Add nutrient info
  left_join(data, by=c("gu_a3"="country_iso3")) %>% 
  # Expand
  complete(gu_a3, nutrient_label) %>% 
  # Remove missing data
  filter(!is.na(nutrient_label))


# Plot data (all)
################################################################################

# Add expanded data to SF
data_all_sf <- world %>% 
  left_join(data_expanded, by=c("gu_a3"))

# Plot data
g <- ggplot(data_all_sf) +
  facet_wrap(~nutrient_label, ncol=4) +
  geom_sf(mapping=aes(fill=perc_diff), lwd=0.1, color="grey30") +
  # Legend
  scale_fill_gradient2(name="% difference in 2030\nunder high and low roads", midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text=element_blank(),
        axis.title=element_blank(),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        strip.text=element_text(size=6),
        plot.title=element_text(size=8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_perc_nutr_diff_all.png"), 
       width=10.5, height=6, units="in", dpi=600)


# Plot data (subset)
################################################################################

# Data plot
nutrients_plot <- c("Iron (mg/p/d)", "Zinc (mg/p/d)", "Calcium (mg/p/d)", 
                    "Vitamin A (IU/p/g)", "Vitamin B-12 (ug/p/d)", "Omega-3 fatty acids (g/p/d)")
data_plot <- data_expanded %>% 
  filter(nutrient_label %in% nutrients_plot)

# Add expanded data to SF
data_plot_sf <- world %>% 
  left_join(data_plot, by=c("gu_a3"))

# Plot data
g <- ggplot(data_plot_sf) +
  facet_wrap(~nutrient_label, ncol=3) +
  geom_sf(mapping=aes(fill=perc_diff), lwd=0.1, color="grey30") +
  # Legend
  scale_fill_gradient2(name="% difference in 2030\nunder high and low roads", midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text=element_blank(),
        axis.title=element_blank(),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        strip.text=element_text(size=6),
        plot.title=element_text(size=8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_perc_nutr_diff_use.png"), 
       width=6.5, height=3, units="in", dpi=600)

