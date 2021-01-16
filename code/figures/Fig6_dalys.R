

# Read data
################################################################################

# Turn off scientific notation
options(scipen=999)

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
outputdir <- "output"
plotdir <- "figures"

# Read data
dalys <- readRDS(file.path(outputdir, "2030_dalys_base_high_road_summarized.Rds"))

# World
world <- rnaturalearth::ne_countries("small", returnclass = "sf")


# Format data
################################################################################

# Build data
cdata <- dalys %>% 
  group_by(country, iso3) %>% 
  summarize(dalys_base=sum(DALY2030_br),
            dalys_high=sum(DALY2030_hr),
            dalys_diff=dalys_high-dalys_base,
            dalys_pdiff=dalys_diff/dalys_base*100) %>% 
  ungroup()


# Plot distribution of DALYs
hist(cdata$dalys_pdiff)

# Add to world
cdata_sf <- world %>% 
  left_join(cdata, by=c("gu_a3"="iso3"))


# Plot data
################################################################################

# Base theme
base_theme <-  theme(axis.text=element_text(size=6),
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

# Plot data
g1 <- ggplot(cdata_sf) +
  geom_sf(mapping=aes(fill=dalys_pdiff), lwd=0.2) +
  # Legend
  scale_fill_gradient2(name="% difference\nin 2030 DALYs\n(high vs. base)", low="navy", high="darkred", mid="white") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() +  base_theme +
  theme(axis.text=element_blank(),
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.position = c(0.08,0.37))
g1


# Export
ggsave(g1, filename=file.path(plotdir, "Fig5_dalys.png"), 
       width=6.5, height=2.75, units="in", dpi=600)



