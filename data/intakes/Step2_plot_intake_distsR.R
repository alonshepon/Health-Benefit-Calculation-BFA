

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
data <- readRDS(file.path(inputdir, "habitual_nutrient_intakes_by_age_sex_4countries.Rds"))


# Plot data coverage
################################################################################

# Data coverage
coverage <- data %>% 
  group_by(country, nutrient, sex, age_yr) %>% 
  summarize(n=n()) %>% 
  mutate(data_yn=n>0)

# Plot coverage
g <- ggplot(coverage, aes(x=age_yr, y=nutrient, fill=data_yn)) +
  facet_grid(country~sex) +
  geom_tile() +
  # Labels
  labs(x="Age (yr)", y="", title="Intake data coverage") +
  scale_fill_discrete(name="Data available?") +
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
        axis.line = element_line(colour = "black"),
        # Legend
        legend.position="bottom")
g

# Export plot
ggsave(g, filename=file.path(plotdir,"intake_data_coverage.png"), 
       width=6.5, height=4, units="in", dpi=600)


# Plot intake distribtuions
################################################################################

# Key
key <- data %>% 
  select(country, nutrient) %>% 
  unique() %>% 
  arrange(country, nutrient)

# Setup theme
my_theme <-  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   axis.text=element_text(size=6),
                   axis.text.y=element_blank(),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom")

# Loop through and plot
for(i in 1:nrow(key)){
  
  # Subset data
  country_do <- key$country[i]
  nutrient_do <- key$ nutrient[i]
  sdata <- data %>% 
    filter(country==country_do, nutrient==nutrient_do)
  print(paste(i, country_do, nutrient_do))
  
  # Plot data
  plot_title <- paste0(country_do, ": ", nutrient_do, " intake")
  g <- ggplot(sdata, aes(x=intake, fill=sex)) +
    facet_wrap(~age_group, ncol=4, scales="free_y") +
    geom_density(alpha=0.6, lwd=0.2) +
    # Labels
    labs(x="Habitual intake", y="Density", title=plot_title) +
    # Legends
    scale_fill_discrete(name="") +
    # Theme
    theme_bw() + my_theme
  g
  
  # Export plot
  fig_name <- paste0(country_do, "_", nutrient_do, ".pdf") %>% tolower()
  ggsave(g, filename=file.path(plotdir, fig_name), 
         width=8.5, height=11, units="in", dpi=600)
  
}
