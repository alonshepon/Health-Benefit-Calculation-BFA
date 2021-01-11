

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

# Read data
data_orig <- readRDS(file.path(outputdir, "COSIMO_nutrient_by_scenario_cntry_with_dissagg.Rds"))

# World
world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")


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
summary(val2030$intake_orig[val2030$nutrient=="Omega-3 fatty acids"])


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
  spread(key="scenario", value="intake") %>% 
  mutate(intake_diff=High-Base,
         intake_pdiff=(High-Base)/Base*100) %>% 
  # Add cap
  mutate(cap=recode(nutrient,
                    "Calcium"=100,
                    "Iron"=0.75,
                    "Omega-3 fatty acids"=1,
                    "Protein"=4,
                    "Vitamin A"=10,
                    "Vitamin B12"=3,
                    "Zinc"=1.5) %>% as.numeric(),
         intake_diff_cap=pmin(intake_diff, cap))

# Inspect distribution
g <- ggplot(data, aes(x=intake_diff_cap)) +
  facet_wrap(~nutrient, ncol=4, scales="free") +
  geom_histogram() +
  theme_bw()
g

# Set breaks and labels
breaks_list <- list("Calcium"=seq(-50,100,50),
                    "Iron"=seq(0, 0.75, 0.25),
                    "Omega-3 fatty acids"=seq(0,1,0.25),
                    "Vitamin A"=seq(-10,10,5),
                    "Vitamin B12"=seq(0,3,1),
                    "Zinc"=seq(0,1.5,0.5))

labels_list <- list("Calcium"=c("-50", "0", "50", "≥100"),
                      "Iron"=c("0.00", "0.25", "0.50", "≥0.75"),
                      "Omega-3 fatty acids"=c("0.00", "0.25", "0.50", "0.75", "≥1.00"),
                      "Vitamin A"=c("-10", "-5", "0", "5", "≥10"),
                      "Vitamin B12"=c("0", "1", "2", "≥3"),
                      "Zinc"=c("0.0", "0.5", "1.0", "≥1.5"))


# Plot data
################################################################################

nutrient <- "Vitamin A"

# Function to plot data
plot_map <- function(nutrient){
  
  # Subset data
  nutr_do <- nutrient
  sdata <- data %>% 
    filter(nutrient==nutr_do)

  # Spatialize
  sdata_sf <- world %>% 
    left_join(sdata, by=c("gu_a3"="iso3"))
  
  # Build label
  nutrient_label <- paste0(nutr_do, " (", unique(sdata$units), ")")
  
  # Get breaks and labels
  breaks <- breaks_list[[nutr_do]]
  labels <- labels_list[[nutr_do]]
  
  # Plot
  g <- ggplot(sdata_sf) +
    geom_sf(mapping=aes(fill=intake_diff_cap), lwd=0.1, color="grey30") +
    # Labels
    labs(title = nutrient_label) +
    # Legend
    scale_fill_gradient2(name="ΔDaily per capita\nnutrient intake\n(high - base)", 
                         breaks=breaks, labels=labels,
                         midpoint=0, low="darkred", high="navy", mid="white", na.value = "grey80") +
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
           intake_diff_cap=pmin(intake_diff, ymax) %>% pmax(., ymin))
  
  # Plot data
  g <- ggplot(sdata, aes(y=intake_diff_cap, x=scenario)) +
    geom_boxplot(outlier.size=0.3, lwd=0.2, fill="grey85", color="grey30") +
    # Labels
    labs(x="Scenario", y="ΔDaily per capita nutrient intake\n(disaggregation - GND values)", title=" ") +
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
map1 <- plot_map(nutrient="Omega-3 fatty acids")
map2 <- plot_map(nutrient="Vitamin B12")
map3 <- plot_map(nutrient="Iron")
map4 <- plot_map(nutrient="Zinc")
map5 <- plot_map(nutrient="Calcium")
map6 <- plot_map(nutrient="Vitamin A")

# Boxplots
box1 <- plot_boxplot(nutrient="Omega-3 fatty acids")
box2 <- plot_boxplot(nutrient="Vitamin B12")
box3 <- plot_boxplot(nutrient="Iron")
box4 <- plot_boxplot(nutrient="Zinc")
box5 <- plot_boxplot(nutrient="Calcium")
box6 <- plot_boxplot(nutrient="Vitamin A")


# Merge 
g <- gridExtra::grid.arrange(map1, box1, map2, box2,
                             map3, box3, map4, box4,
                             map5, box5, map6, box6, 
                             ncol=4,
                             widths=c(0.38,0.12, 0.38, 0.12))

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig3_nutrients.png"), 
       width=6.5, height=4, units="in", dpi=600)



