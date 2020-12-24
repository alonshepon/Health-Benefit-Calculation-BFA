


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "data/cosimo_nutr_disagg/raw"
outputdir <- "data/cosimo_nutr_disagg/processed"
plotdir <- "data/cosimo_nutr_disagg"

# Read data
data1_orig <- read.csv(file.path(inputdir, "Disaggregated_NutrientScen.csv"), as.is=T)
data2_orig <- read.csv(file.path(inputdir, "Disaggregated_NutrientScen_grouped.csv"), as.is=T)

# Build EU27 key
eu27_key <- read.csv("data/cosimo/processed/COSIMO_AGLINK_2020_country_key.csv", as.is=T) %>% 
  filter(group_code=="EUN") %>% 
  select(iso3, country, iso3_use, country_use) %>% 
  filter(country!="Czechoslovakia")

# Format data
################################################################################

# Format data
data <- data2_orig %>% 
  # Rename columns
  rename(iso3=iso3c, intake_orig=nutrient_supply, intake=nutrient_supply_dis) %>% 
  # Add country column
  mutate(country=countrycode(iso3, "iso3c", "country.name")) %>% 
  # Fill missing countries
  mutate(country=ifelse(is.na(country), iso3, country),
         country=recode(country, 
                        "ANT"="Netherlands Antilles", 
                        "BLX"="Belgium-Luxembourg",
                        "CZ2"="Czechoslovakia",
                        "ET2"="Ethiopia PDR",
                        "EUN"="EU 27",
                        "SRM"="Serbia and Montenegro",
                        "USR"="USSR",
                        "YUG"="Yugoslav SFR")) %>% 
  # Format scenario
  mutate(scenario=recode(scenario, "base"="Base", "high"="High")) %>% 
  # Arrange
  select(iso3, country, nutrient, units, scenario, year, intake_orig, intake, everything()) %>% 
  arrange(country, nutrient, scenario, year) %>% 
  # Calculate intake difference
  mutate(intake_diff=intake-intake_orig)

# Inspect data
str(data)
freeR::complete(data)
range(data$year)
table(data$nutrient)


# Explode EU data
################################################################################

# Non-EU data
data_non_eu <- data %>% 
  filter(!iso3 %in% c("EUN", "WLD"))

# EU data
data_eun <- data %>% 
  filter(iso3 %in% "EUN")

# Duplicate EUN data for each E27 country
data_eun_use <- purrr::map_df(1:nrow(eu27_key), function(x) {
  
  iso_do <- eu27_key$iso3_use[x]
  country_do <- eu27_key$country_use[x]
  
  cdata <- data_eun %>% 
    mutate(iso3=iso_do,
           country=country_do)
  
})

# Merge EU-expanded and non-EU data
data_out <- bind_rows(data_non_eu, data_eun_use) %>% 
  arrange(country,nutrient, scenario, year)


# Read data
################################################################################

# Export data
saveRDS(data, file=file.path(outputdir, "COSIMO_nutrient_by_scenario_cntry_with_dissagg.Rds"))




# Check time series
################################################################################

data_use <- data %>% 
  filter(data!="WLD")

gstats <- data_use %>% 
  group_by(nutrient, scenario, year) %>% 
  summarize(intake=mean(intake_orig, na.rm=T)) %>% 
  ungroup()

g <- ggplot(gstats, aes(x=year, y=intake, color=scenario)) +
  geom_line() +
  facet_wrap(~nutrient, ncol=4, scales="free_y") +
  labs(x="", y="Mean intake") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=6),
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


# Plot data
################################################################################

# Data to plot
sdata <- data %>% 
  # Reduce to 2030
  filter(year==2030) %>% 
  # Cap values
  group_by(nutrient) %>% 
  mutate(ymin=quantile(intake_diff, 0.05),
         ymax=quantile(intake_diff, 0.95),
         intake_diff_cap=pmin(intake_diff, ymax) %>% pmax(., ymin)) %>%  
  ungroup() %>% 
  # Remove protein
  filter(nutrient!="Protein") %>% 
  # Add label
  mutate(nutrient_label=paste0(nutrient, "\n(", units, ")"))
  
# Plot data
g <- ggplot(sdata, aes(y=intake_diff_cap, x=scenario)) +
  facet_wrap(~nutrient_label, scale="free_y") +
  geom_boxplot(outlier.size=0.3, lwd=0.3, fill="grey80") +
  # Labels
  labs(x="Scenario", y="ΔDaily per capita nutrient intake\n(disaggregation - GND values)") +
  # Horizontal reference line
  geom_hline(yintercept=0, lwd=0.3) +
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
        axis.line = element_line(colour = "black"))
g

# Export
ggsave(g, filename=file.path(plotdir, "delta_nutr_intakes_w_diversity.png"), 
       width=6.5, height=4, units="in", dpi=600)







