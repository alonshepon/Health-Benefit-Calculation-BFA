

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

# Read data
data <- readRDS(file.path(inputdir, "habitual_nutrient_intakes_by_age_sex_13countries.Rds"))


# Calculate nutrient mins/maxs
################################################################################

# Intake ranges
range_key <- data %>% 
  group_by(nutrient) %>% 
  summarize(min=min(intake),
            max=max(intake),
            q90=quantile(intake, probs=0.9))

# Export
write.csv(range_key, file=file.path(inputdir, "habitual_nutrient_intake_ranges.csv"), row.names = F)

# Plot data coverage
################################################################################

# Data coverage
coverage <- data %>% 
  group_by(country, nutrient, nutrient_units, sex, age_yr) %>% 
  summarize(n=n()) %>% 
  mutate(data_yn=n>0, 
         sex=stringr::str_to_title(sex),
         nutrient_label=paste0(nutrient, " (", nutrient_units, ")")) %>% 
  # Remove children
  filter(sex!="Children") %>% 
  mutate(sex=recode(sex, 
                    "Men"="Males",
                    "Women"="Females")) %>% 
  # Format omegas
  mutate(nutrient=recode(nutrient, "Omega-3 fatty acids"="DHA+EPA"))

# Plot coverage
g <- ggplot(coverage, aes(x=age_yr, y=nutrient, fill=data_yn)) +
  facet_grid(country~sex) +
  geom_tile() +
  # Labels
  labs(x="Age (yr)", y="") +
  scale_fill_discrete(name="Data available?") +
  # Theme
  theme_bw() + 
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        strip.text=element_text(size=6),
        plot.title=element_blank(),
        # Gridlines
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        # Legend
        legend.position="none")
g

# Export plot
ggsave(g, filename=file.path(plotdir,"FigS8_intake_data_coverage.png"), 
       width=6.5, height=8.5, units="in", dpi=600)