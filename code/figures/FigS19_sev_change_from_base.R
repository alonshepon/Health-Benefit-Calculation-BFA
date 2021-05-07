

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
cosimodir <- "data/cosimo_nutr_disagg/processed"

# Read problem country key
prob_key <- read.csv("data/countries_with_bug.csv", as.is=T)

# Read data
sevs_orig <- readRDS(file.path(outputdir, "2030_sevs_base_high_road_final_diversity_disagg.Rds"))
nutr_orig <- readRDS(file.path(cosimodir, "COSIMO_nutrient_by_scenario_cntry_with_dissagg.Rds"))

# Calculate average EARs
ears <- nutriR::dris %>% 
  filter(dri_type=="Estimated Average Requirement (EAR)") %>% 
  group_by(nutrient, nutrient_units) %>% 
  summarize(ear=mean(value, na.rm=T)) %>% 
  mutate(nutrient=recode(nutrient, 
                         "Vitamin A"="Vitamin A, RAE"))


# Build data
################################################################################

# Read SEVS data
sevs <- sevs_orig %>% 
  # Eliminate problem countries
  filter(!iso3 %in% prob_key$iso) %>% 
  # Recode nutrients
  mutate(nutrient=recode(nutrient, 
                         # "Vitamin A, RAE"="Vitamin A",
                         "Omega-3 fatty acids"="DHA+EPA fatty acids",
                         "Vitamin B-12"="Vitamin B12")) %>% 
  # Calculate average by country
  group_by(nutrient, country, iso3) %>% 
  summarize(across(sev_base:sev_delta, .fns=mean)) %>% 
  ungroup()
    
# Read nutrients data
nutrs <- nutr_orig %>% 
  # Eliminate problem countries
  filter(!iso3 %in% prob_key$iso) %>% 
  # Recode nutrients
  mutate(nutrient=recode(nutrient, 
                         "Vitamin B-12"="Vitamin B12",
                         "Omega-3 fatty acids"="DHA+EPA fatty acids")) %>% 
  # Change nutrient units
  mutate(nutrient_units=gsub("/p", "", nutrient_units)) %>% 
  # Reduce to 2030
  filter(year==2030) %>% 
  # Simplify and spread
  select(nutrient, nutrient_units, country, iso3, scenario, intake) %>% 
  spread(key="scenario", value="intake") %>% 
  # Rename
  rename(intake_base="Base", intake_high="High road") %>% 
  mutate(intake_diff=intake_high-intake_base) %>% 
  # Standardize intake difference to nutrient scale
  group_by(nutrient) %>% 
  mutate(intake_diff_std=scale(intake_diff)) %>% 
  ungroup()

# Merge
data <- sevs %>% 
  left_join(nutrs) %>% 
  mutate(label=paste0(nutrient, " (", nutrient_units, ")"))


# Build data
################################################################################

# Plotting function
# nutrient <- "Iron"
make_plot <- function(nutrient){
  
  # Nutrient
  nutr_do <- nutrient
  ear <- ears$ear[ears$nutrient==nutr_do]
  
  # Plot
  g <- ggplot(data %>% filter(nutrient==nutr_do), aes(x=intake_diff, y=intake_base, fill=sev_delta, size=sev_base)) +
    facet_wrap(~label, scales="free") +
    geom_point(pch=21, stroke=0.2) +
    # Lines
    geom_hline(yintercept=ear, linetype="dashed") +
    geom_vline(xintercept=0) +
    # Labels
    labs(x="ΔNutrient intake\n(high - base)", y="Nutrient intake\n(base scenario)") +
    # Legend
    scale_size_continuous(name="SEV (base)") +
    scale_fill_gradient2(name="ΔSEV (high - base)", midpoint=0, low="navy", mid="white", high="darkred") +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", order=1)) +
    # Theme
    theme_bw() +
    theme(axis.text=element_text(size=5),
          axis.title=element_text(size=6),
          legend.text=element_text(size=5),
          legend.title=element_text(size=6),
          strip.text=element_text(size=6),
          plot.title=element_blank(),
          # Gridlines
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          # Legend
          legend.position="right",
          legend.key.size = unit(0.3, "cm"),
          legend.margin = margin(-2,0,0,0))
  print(g)
  
  # Export
  return(g)
  
}

# Plot data
g1 <- make_plot("DHA+EPA fatty acids")
g2 <- make_plot("Vitamin B12")
g3 <- make_plot("Iron")
g4 <- make_plot("Zinc")
g5 <- make_plot("Calcium")
g6 <- make_plot("Vitamin A, RAE")

# Merge maps
g <- gridExtra::grid.arrange(g1, g2,
                             g3, g4,
                             g5, g6, ncol=2)

# Export
ggsave(g, filename=file.path(plotdir, "FigS19_sev_outcomes_based_on_base.png"), 
       width=6.5, height=6.5, units="in", dpi=600)




# Other strategies
################################################################################

# # Plot
# g <- ggplot(data, aes(x=intake_diff, y=sev_delta, size=intake_base, fill=sev_base)) +
#   facet_wrap(~nutrient, scales="free") +
#   geom_point(pch=21, stroke=0.2) +
#   # Lines
#   geom_hline(yintercept=0) +
#   geom_vline(xintercept=0) +
#   # Labels
#   labs(x="ΔNutrient intake\n(high - base)", y="ΔSEVS\n(high - base)") +
#   # Legend
#   scale_size_continuous(name="Intake (base)") +
#   scale_fill_gradientn(name="SEV (base)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
#   guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
#   # Theme
#   theme_bw()
# g
# 
# # Plot
# g <- ggplot(data %>% filter(nutrient=="Calcium"), aes(x=sev_base, y=scale(intake_base), fill=sev_delta)) +
#   facet_wrap(~nutrient, scales="free") +
#   geom_point(pch=21, stroke=0.2) +
#   # Lines
#   # geom_hline(yintercept=0) +
#   # geom_vline(xintercept=0) +
#   # Labels
#   labs(x="Scaled intake\n(base scenario)", y="Mean deficiency\n(base scenario)") +
#   # Legend
#   # scale_size_continuous(name="Intake difference (scaled)") +
#   scale_fill_gradient2(name="ΔSEV (high - base)(more blue = more improvements", midpoint=0, low="navy", mid="white", high="darkred") +
#   guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
#   # Theme
#   theme_bw()
# g
