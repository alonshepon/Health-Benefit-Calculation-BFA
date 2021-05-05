

# Clear workspace
rm(list = ls())

# Read data
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "figures"
tabledir <- "tables"
datadir <- "data"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "cfree1604.xlsx"))

# Read data
################################################################################

# Format data
data <- data_orig %>% 
  gather(key="year", value="prod_mt", 5:ncol(.)) %>% 
  mutate(prod_mt=prod_mt*1000,
         year=as.numeric(year))

# Stats
stats <- data %>% 
  group_by(scenario, year) %>% 
  summarize(prod_mt=sum(prod_mt))

# Theme
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

# Total results
g1 <- ggplot(stats, aes(x=year, y=prod_mt/1e6, linetype=scenario)) + 
  geom_line() +
  # Labels
  labs(x="Year", y="Overall production\n(millions of mt)") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g1

# Sector results
g2 <- ggplot(data, aes(x=year, y=prod_mt/1e6, color=environment, linetype=scenario)) + 
  facet_wrap(~sector) +
  geom_line() +
  # Labels
  labs(x="Year", y="Production by sector\n(millions of mt)") +
  scale_linetype_discrete(name="Scenario") +
  scale_color_discrete(name="Source") +
  # Theme
  theme_bw() + my_theme
g2

# Megre
g <- gridExtra::grid.arrange(g1, g2, nrow=2, heights=c(0.45, 0.55))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigSX_production_scenarios.png"), 
       width=6.5, height=4.5, units="in", dpi=600)



