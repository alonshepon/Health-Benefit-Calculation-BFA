

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
data <- readRDS(file.path(inputdir, "habitual_nutrient_intakes_by_age_sex_9countries.Rds"))


# Build key for filling sex/age/countries
################################################################################

# Build base key
key1 <- data %>% 
  group_by(country, sex, age_group, nutrient) %>% 
  summarize(n=n()) %>% 
  ungroup()

# Format base key
key2 <- key1 %>% 
  mutate(country=recode(country, 
                        "Uganda"="Uganda & Zambia",
                        "Zambia"="Uganda & Zambia",
                        "Laos"="Laos & Philippines",
                        "Philippines"="Laos & Philippines")) %>% 
  group_by(country, sex, age_group, nutrient) %>% 
  summarize(source=ifelse(sum(n)>0, "Original", "other")) %>% 
  ungroup() %>% 
  # Fill in other sources
  complete(country, sex, age_group, nutrient) %>% 
  mutate(source=ifelse(country=="United States" & is.na(source), paste("US", sex, "85-89", sep="-"), source),
         # Mexico
         source=ifelse(country=="Mexico" & sex=="men" & is.na(source), paste("Mexico", sex, "90-94", sep="-"), source),
         source=ifelse(country=="Mexico" & sex=="women" & is.na(source), paste("Mexico", sex, "95-99", sep="-"), source),
         # Italy
         source=ifelse(country=="Italy" & nutrient %in% c("Zinc", "Processed meat"), "US", source),
         source=ifelse(country=="Italy" & sex=="men" & is.na(source), paste("Italy", sex, "90-94", sep="-"), source),
         source=ifelse(country=="Italy" & sex=="women" & is.na(source), paste("Italy", sex, "95-99", sep="-"), source), 
         # Laos & Philippines
         source=ifelse(country=="Laos & Philippines" & nutrient=="Vitamin A" & sex=="men", "L&P-women", source),
         source=ifelse(country=="Laos & Philippines" & nutrient=="Processed meat" & sex=="men", "L&P-men-80-84", source),
         source=ifelse(country=="Laos & Philippines" & nutrient=="Vitamin A" & sex=="women" & age_group %in% c("0-4", "5-9", "10-14"), "L&P-women-15-19", source),
         source=ifelse(country=="Laos & Philippines" & nutrient=="Vitamin A" & sex=="women" & is.na(source), "L&P-women-45-49", source),
         source=ifelse(country=="Laos & Philippines" & sex=="men" & is.na(source), "L&P-men-85-89", source),
         source=ifelse(country=="Laos & Philippines" & sex=="women" & is.na(source), "L&P-women-80-84", source),
         # Uganda & Zambia
         source=ifelse(country=="Uganda & Zambia" & sex=="men", "U&Z-women", source),
         source=ifelse(country=="Uganda & Zambia" & sex=="women" & nutrient=="Processed meat", "US", source),
         source=ifelse(country=="Uganda & Zambia" & sex=="women" & age_group %in% c("0-4", "5-9", "10-14") & is.na(source), "U&Z-women-15-19", source),
         source=ifelse(country=="Uganda & Zambia" & sex=="women" & nutrient %in% c("Vitamin B-12", "Iron") & is.na(source), "U&Z-women-70-74", source),
         source=ifelse(country=="Uganda & Zambia" & sex=="women" & is.na(source), "U&Z-women-65-69", source),
         # China
         source=ifelse(country=="China" & nutrient %in% c("Processed meat", "Vitamin B-12"), "US", source),
         source=ifelse(country=="China" & age_group %in% c("0-4", "5-9", "10-14") & is.na(source), paste("China", sex, "15-19", sep="-"), source),
         source=ifelse(country=="China" & is.na(source), paste("China", sex, "90-94", sep="-"), source),
         # Burkina Faso
         source=ifelse(country=="Burkina Faso" & nutrient=="Processed meat", "US", source),
         source=ifelse(country=="Burkina Faso" & sex=="men" & is.na(source), "BF-women", source),
         source=ifelse(country=="Burkina Faso" & sex=="women" & age_group %in% c("5-9", "10-14") & is.na(source), "BF-women-15-19", source),
         source=ifelse(country=="Burkina Faso" & is.na(source), "BF-women-55-59", source))

# Plot key
g <- ggplot(key2, aes(x=age_group, y=nutrient, fill=source)) +
  # lemon::facet_rep_grid(country~sex, repeat.tick.labels = "x") +
  facet_grid(country~sex) +
  geom_tile() +
  # Labels
  labs(x="Age group (yr)", y="") +
  scale_fill_discrete(name="Data source", guide=guide_legend(ncol=1)) +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        strip.text=element_text(size=8),
        plot.title=element_blank(),
        # Gridlines
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g

# Export plot
ggsave(g, filename=file.path(plotdir,"intake_sex_age_gap_fill.png"), 
       width=6.5, height=8.5, units="in", dpi=600)

