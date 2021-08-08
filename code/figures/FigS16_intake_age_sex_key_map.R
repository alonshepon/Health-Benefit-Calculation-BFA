

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
plotdir <- "figures"



age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", 
                "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99")


# Read edited key
dist_key2 <- readxl::read_excel(file.path(outputdir, "dist_id_key.xlsx")) %>% 
  mutate(age_group=recode(age_group, 
                          "44325"="5-9", 
                          "44483"="10-14"),
         age_group=factor(age_group, levels=age_groups)) %>% 
  mutate(type2=recode(type2, "available"="Available"),
         sex=recode(sex, "men"="Males", "women"="Females"),
         country_label=recode(country_final, 
                              "Laos & Philippines"="Laos &\nPhilippines",
                              "Uganda & Zambia"="Uganda &\nZambia",
                              "Italy, Romania, Bulgaria"="Italy,\nRomania,\nBulgaria")) %>% 
  # Format omegas
  mutate(nutrient_plot=recode(nutrient, "Omega-3 fatty acids"="DHA+EPA")) %>% 
  # Format sources
  mutate(type2=recode(type2, 
                      "From men"="From males", 
                      "From women"="From females"))

# Plot
g <- ggplot(dist_key2 , aes(x=age_group, y=nutrient_plot, fill=type2)) +
  facet_grid(country_label~sex) +
  geom_tile() +
  # Labels
  labs(x="Age (yr)", y="") +
  scale_fill_discrete(name="Data source") +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size=7),
        axis.title=element_text(size=8),
        legend.text = element_text(size=7),
        legend.title=element_text(size=8),
        strip.text = element_text(size=8))
g

# Plot key
ggsave(g, filename=file.path(plotdir,"FigS16_intake_age_sex_key_map.png"), 
       width=8.5, height=8.5, units="in", dpi=300)

