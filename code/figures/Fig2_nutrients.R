

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outputdir <- "data/cosimo_nutr_disagg/processed"
plotdir <- "figures"

# Read problem country key
prob_key <- read.csv("data/countries_with_bug.csv", as.is=T)

# Read data
data_orig <- readRDS(file.path(outputdir, "COSIMO_nutrient_by_scenario_cntry_with_dissagg.Rds")) %>% 
  # Eliminate problem countries
  filter(!iso3 %in% prob_key$iso) %>% 
  # Recode nutrients
  mutate(nutrient=recode(nutrient, 
                         "Vitamin B-12"="Vitamin B12",
                         "Omega-3 fatty acids"="DHA+EPA fatty acids")) %>% 
  # Change nutrient units
  mutate(nutrient_units=gsub("/p", "", nutrient_units))

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

# Inspect
################################################################################

# 2030 values
val2030 <- data_orig %>% 
  filter(year==2030)

# Plot 2030 values
g <- ggplot(val2030, aes(y=intake_orig)) +
  facet_wrap(~nutrient, scale="free_y") +
  geom_boxplot()
g

# Summary for omegas
summary(val2030$intake_orig[val2030$nutrient=="DHA+EPA fatty acids"])


# Manuscript stats
################################################################################

# Calculate stats
stats <- data_orig %>% 
  # Calculate percent difference
  mutate(intake_pdiff=(intake-intake_orig)/intake_orig*100) %>% 
  # Reduce to base and 2030
  filter(scenario=="Base" & year==2030 & is.finite(intake_pdiff)) %>%
  # Calculate global stats
  group_by(nutrient, nutrient_units) %>% 
  summarize(pdiff_avg=mean(intake_pdiff, na.rm=T),
            pdiff_med=median(intake_pdiff, na.rm=T),
            pdiff_min=min(intake_pdiff, na.rm=T),
            pdiff_max=max(intake_pdiff, na.rm=T))


# Determine caps
################################################################################

# Format data
data <- data_orig %>%
  # Reduce to 2030
  filter(year==2030) %>% 
  # Calculate stats
  select(-c(intake_orig, intake_diff)) %>% 
  mutate(scenario=recode(scenario, "High road"="High")) %>% 
  spread(key="scenario", value="intake") %>% 
  mutate(intake_diff=High-Base,
         intake_pdiff=(High-Base)/Base*100) %>% 
  # Add cap
  mutate(cap=recode(nutrient,
                    "Calcium"=40,
                    "Iron"=0.4,
                    "DHA+EPA fatty acids"=0.15,
                    "Protein"=3,
                    "Vitamin A, RAE"=10,
                    "Vitamin B12"=0.75,
                    "Zinc"=0.2) %>% as.numeric(),
         intake_diff_cap=pmin(intake_diff, cap))

# Inspect distribution
g <- ggplot(data, aes(x=intake_diff_cap)) +
  facet_wrap(~nutrient, ncol=4, scales="free") +
  geom_histogram() +
  theme_bw()
g

# Set breaks and labels
breaks_list <- list("Calcium"=seq(0, 40, 10),
                    "Iron"=seq(0, 0.4, 0.1),
                    "DHA+EPA fatty acids"=seq(0, 0.15, 0.05),
                    "Vitamin A, RAE"=seq(-10, 5, 5),
                    "Vitamin B12"=seq(0, 0.75, 0.25),
                    "Zinc"=seq(-0.05, 0.2, 0.05))

labels_list <- list("Calcium"=c("0", "10", "20", "30", "≥40"),
                      "Iron"=c("0.0", "0.1", "0.2", "0.3", "≥0.4"),
                      "DHA+EPA fatty acids"=c("0", "0.05", "0.10", "≥0.15"),
                      "Vitamin A, RAE"=c("-10", "-5", "0", "5"),
                      "Vitamin B12"=c("0.00", "0.25", "0.50", "≥0.75"),
                      "Zinc"=c("-0.05", "0.00", "0.05", "0.10", "0.15", "≥0.20"))


# Plot data
################################################################################

nutrient <- "Vitamin A, RAE"

# Function to plot data
plot_map <- function(nutrient){
  
  # Subset data
  nutr_do <- nutrient
  sdata <- data %>% 
    filter(nutrient==nutr_do)

  # Spatialize
  sdata_sf <- world %>% 
    left_join(sdata, by=c("gu_a3"="iso3"))
  
  # Spatialize tiny
  sdata_pt <- world_centers %>% 
    left_join(sdata, by=c("iso3"="iso3")) %>% 
    # Reduce to ones with data
    filter(!is.na(intake_diff)) %>% 
    arrange(area_sqkm) %>% 
    # Reduce to small
    filter(area_sqkm<=2.5*10^4 & continent!="Europe")
  
  # Build label
  nutr_do_use <- nutr_do
  nutrient_label <- paste0(nutr_do_use, " (", unique(sdata$nutrient_units), ")")
  if(nutr_do=="Vitamin A, RAE"){
    nutr_do_use <- "Vitamin A"
    nutrient_label <- paste0(nutr_do_use, " (", unique(sdata$nutrient_units), ")")
  }
  if(nutr_do=="Vitamin B12"){
    nutrient_label <- expression("Vitamin B"["12"]*" (ug/d)")
  }
  
  # Get breaks and labels
  breaks <- breaks_list[[nutr_do]]
  labels <- labels_list[[nutr_do]]
  
  # Plot
  g <- ggplot(sdata_sf) +
    geom_sf(mapping=aes(fill=intake_diff_cap), lwd=0.1, color="grey30") +
    # Plot small places
    geom_sf(data=sdata_pt, mapping=aes(fill=intake_diff_cap), shape=21, size=0.9, stroke=0.3) +
    # Plot French Guiana
    geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
    # Labels
    labs(title = nutrient_label) +
    # Legend
    scale_fill_gradient2(name="ΔDaily per capita\nnutrient intake\n(high - base)", 
                         breaks=breaks, labels=labels,
                         # midpoint=0, low="#FFD947", high="#364F6B", mid="white", na.value = "grey80") + # grey gold
                         # midpoint=0, low="#FC5185", high="#3FC1C9", mid="white", na.value = "grey80") + # blue pink
                         # midpoint=0, low="#FC5185", high="#A9D158", mid="white", na.value = "grey80") + # green pink
                         midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") + # RED BLUE
    # scale_fill_gradient2(name="% difference\nin 2030 intakes\n(high vs. base)", midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.5, barheight = 1.7)) +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Theme
    theme_bw() +
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          legend.text=element_text(size=4),
          legend.title=element_text(size=4),
          strip.text=element_text(size=6),
          plot.title=element_text(size=8),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          legend.position =  c(0.11,0.33),
          legend.background = element_rect(fill=alpha('blue', 0)))
  g
  
}

# Plot boxplot
plot_boxplot <- function(nutrient){
  
  # Subset data
  nutr_do <- nutrient
  sdata <- data_orig %>% 
    filter(year==2030 & nutrient==nutr_do) %>% 
    # Cap values
    mutate(ymin=quantile(intake_diff, 0.05),
           ymax=quantile(intake_diff, 0.95),
           intake_diff_cap=pmin(intake_diff, ymax) %>% pmax(., ymin)) %>% 
    mutate(scenario=recode(scenario, "High road"="High"))
  
  # Plot data
  g <- ggplot(sdata, aes(y=intake_diff_cap, x=scenario)) +
    geom_boxplot(outlier.size=0.3, lwd=0.2, fill="grey85", color="grey30") +
    # Labels
    labs(x="Scenario", y="ΔDaily per capita nutrient intake\n(with - without diversity disaggregation)", title=" ") +
    # Horizontal reference line
    geom_hline(yintercept=0, lwd=0.3) +
    # Theme
    theme_bw() +
    theme(axis.text=element_text(size=4.5),
          axis.title=element_text(size=5),
          axis.title.x=element_blank(),
          legend.text=element_blank(),
          legend.title=element_blank(),
          strip.text=element_blank(),
          plot.title=element_text(size=10),
          # Gridlines
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"), 
          axis.text.y = element_text(angle = 90, hjust = 0.5))
  g
  
  
}


# Maps
map1 <- plot_map(nutrient="DHA+EPA fatty acids")
map2 <- plot_map(nutrient="Vitamin B12")
map3 <- plot_map(nutrient="Iron")
map4 <- plot_map(nutrient="Zinc")
map5 <- plot_map(nutrient="Calcium")
map6 <- plot_map(nutrient="Vitamin A, RAE")

# Boxplots
box1 <- plot_boxplot(nutrient="DHA+EPA fatty acids")
box2 <- plot_boxplot(nutrient="Vitamin B12")
box3 <- plot_boxplot(nutrient="Iron")
box4 <- plot_boxplot(nutrient="Zinc")
box5 <- plot_boxplot(nutrient="Calcium")
box6 <- plot_boxplot(nutrient="Vitamin A, RAE")


# Merge 
g <- gridExtra::grid.arrange(map1, box1, map2, box2,
                             map3, box3, map4, box4,
                             map5, box5, map6, box6, 
                             ncol=4,
                             widths=c(0.38,0.12, 0.38, 0.12))

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_nutrients.png"), 
       width=6.5, height=4, units="in", dpi=600)



