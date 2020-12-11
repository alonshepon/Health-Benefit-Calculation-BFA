


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
dists <- read.csv(file.path(outputdir, "habitual_nutrient_intakes_by_age_sex_9countries_distribution_fits.csv"), as.is=T) %>% 
  filter(!is.na(best_dist) & sex!="children")

# Read range key
range_key <- read.csv(file=file.path(inputdir, "habitual_nutrient_intake_ranges.csv"), as.is=T) %>% 
  mutate(cutoff)

# Cutoff key 
cutoff_key <- tibble(nutrient=c("Calcium", "Iron", "Omega-3 fatty acids", "Red meat", "Vitamin A", "Vitamin B-12", "Zinc"),
                     cutoff= c(2000, 80, 0.3, 300, 3000, 20, 50))

# Add cutoff to dist
dists <- dists %>% 
  left_join(cutoff_key)


# Build data
################################################################################

# Build data: 
i <- 1
data <- purrr::map_df(1:nrow(dists), function(i){
  
  # Distribution to do
  dist_do <- dists$best_dist[i]
  country_do <- dists$country[i]
  nutrient_do <- dists$nutrient[i]
  sex_do <- dists$sex[i]
  age_group_do <- dists$age_group[i]
  
  # Simulation range
  # xmin <- range_key %>% filter(nutrient==nutrient_do) %>% pull(min)
  # xmax <- range_key %>% filter(nutrient==nutrient_do) %>% pull(max)
  xmin <- 0
  xmax <- dists$cutoff[i]
  
  # If gamma
  if(dist_do=="gamma"){
    
    # Extract parameters
    shape <- dists$g_shape[i]
    rate <- dists$g_rate[i]
    
    # Build curve
    x <- seq(xmin, xmax, length.out = 1000)
    y <- dgamma(x, shape=shape, rate=rate)
    # plot(y ~ x)
    
    # Build data frame
    df <- tibble(country=country_do,
                 nutrient=nutrient_do,
                 sex=sex_do, 
                 age_group=age_group_do,
                 intake=x,
                 density=y)
    
  }
  
  # If log-normal
  if(dist_do=="log-normal"){
    
    # Extract parameters
    meanlog <- dists$ln_meanlog[i]
    sdlog <- dists$ln_sdlog[i]
    
    # Build curve
    x <- seq(xmin, xmax, length.out = 200)
    y <- dlnorm(x, meanlog=meanlog, sdlog=sdlog)
    # plot(y ~ x)
    
    # Build data frame
    df <- tibble(country=country_do,
                 nutrient=nutrient_do,
                 sex=sex_do, 
                 age_group=age_group_do,
                 intake=x,
                 density=y)
    
  }
  
  # Return
  df
  
})

# Age groups
sort(unique(data$age_group))
age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
                "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99")

# Factor age groups
data <- data %>% 
  mutate(age_group1=factor(age_group, levels=age_groups))


# Plot distributions
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
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


# Nutrients
nutrients <- sort(unique(dists$nutrient))

# Loop though nutrients
for(i in 1:length(nutrients)){
  
  # Subset data
  nutrient_do <- nutrients[i]
  sdata <- data %>% 
    filter(nutrient==nutrient_do)
  
  # X-axis cutoff
  # cutoff <- cutoff_key

  # Plot distributions
  g <- ggplot(sdata, aes(x=intake, y=density, color=age_group1)) +
    facet_grid(country ~ sex, scales="free_y") +
    geom_line() +
    # xlim(0, cutoff) +
    # Labels
    labs(x="Habitual intake", y="Density", title=nutrient_do) +
    # Legend
    scale_color_discrete(name="Age group") +
    # Theme
    theme_bw() + my_theme
  g
  
  # Export plot
  outfile <- paste0("FigS2", letters[i], "_", tolower(nutrient_do) %>% gsub(" ", "_", .), ".png")
  ggsave(g, filename=file.path(plotdir, outfile), 
         width=6.5, height=6.5, units="in", dpi=600)
  
}


