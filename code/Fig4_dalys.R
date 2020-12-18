

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

# Read data
# saveRDS(file.path(outputdir, "2030_dalys_base_high_road_cleaned.Rds"))
dalys <- readRDS(file.path(outputdir, "2030_dalys_base_high_road_summarized.Rds")) %>% 
  mutate(dalys_diff=DALY2030_hr-DALY2030_br) %>% 
  # Cap DALY difference
  mutate(dalys_diff_cap=pmin(dalys_diff, 10), 
         dalys_diff_cap=pmax(dalys_diff_cap, -10)) %>% 
  # Remove <25 years
  filter(age_group_id>=10)

# World
world <- rnaturalearth::ne_countries("small", returnclass = "sf")


# Format data
################################################################################

# Build data
cdata <- dalys %>% 
  group_by(country, iso3) %>% 
  summarize(dalys_base=sum(DALY2030_br),
            dalys_high=sum(DALY2030_hr),
            dalys_diff=dalys_high-dalys_base) %>% 
  ungroup() %>% 
  # Remove a crazy one
  filter(country!="Taiwan") %>% 
  # Cap DALY difference
  mutate(dalys_diff_cap=pmin(dalys_diff, 100), 
         dalys_diff_cap=pmax(dalys_diff_cap, -100))


# Plot distribution of DALYs
hist(cdata$dalys_diff, seq(-25000,5000,10), xlim=c(-1000,1000))
abline(v=c(-100,100),lty=2)

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
  geom_sf(mapping=aes(fill=dalys_diff_cap), lwd=0.2) +
  # Legend
  scale_fill_gradient2(name="ΔTotal DALYs in 2030\n(high - base)", low="navy", high="darkred", mid="white",
                       breaks=seq(-100,100,50), labels=c("Less than -100 years", "-50", "0", "50", "More than 100 years")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Theme
  theme_bw() +  base_theme +
  theme(axis.text=element_blank(),
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.position = c(0.1,0.33))
g1

# Plot data
g2 <- ggplot(dalys, aes(x=age_group, y=dalys_diff_cap, fill=sex)) +
  geom_boxplot(lwd=0.2, outlier.size=0.5, color="grey30") +
  # Zero line
  geom_hline(yintercept=0, color="black", lwd=0.2) +
  # Labels
  labs(x="Age group", y="ΔTotal DALYs in 2030\n(high - base)") +
  scale_fill_discrete(name="Sex") +
  scale_y_continuous(breaks=seq(-10,10,5), labels=c("≤ -10", "-5", "0", "5", "≥ 10")) +
  # Theme
  theme_bw() + base_theme + 
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

# Plot data
g3 <- ggplot(dalys, aes(y=dalys_diff_cap, fill=sex)) +
  geom_density(alpha=0.3, lwd=0.2) +
  # Zero line
  geom_hline(yintercept=0, color="black", lwd=0.2) +
  # Labels
  labs(x="Density\n ") +
  scale_y_continuous(breaks=seq(-10,10,5), labels=c("≤ -10", "-5", "0", "5", "≥ 10")) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y=element_blank(),
        legend.position = "none")
g3

# Merge plots
# g <- gridExtra::grid.arrange(g1, g2, nrow=2)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=matrix(c(1,1,2,3), nrow=2, byrow = T), widths=c(0.8,0.2))


# Export
ggsave(g, filename=file.path(plotdir, "Fig4_dalys.png"), 
       width=6.5, height=5.5, units="in", dpi=600)



