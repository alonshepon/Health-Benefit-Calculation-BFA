

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
tabledir <- "tables"

# Read data
data_orig <- readRDS(file=file.path(outputdir, "2030_ndeficient_base_high_diversity_disagg.Rds"))



# Build data
################################################################################

# Build stats
stats <- data_orig %>% 
  group_by(nutrient) %>% 
  summarize(n_better=sum(ndeficient_diff[ndeficient_diff<0], na.rm=T), 
            n_worse=sum(ndeficient_diff[ndeficient_diff>0], na.rm=T),
            n_net=sum(ndeficient_diff, na.rm=T)) %>% 
  ungroup()

# Export
write.csv(stats, file=file.path(tabledir, "TableSX_npeople_deficient.csv"), row.names=F)
