

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
  summarize(prevented=sum(ndeficient_diff[ndeficient_diff<0], na.rm=T)/1e6, 
            new=sum(ndeficient_diff[ndeficient_diff>0], na.rm=T)/1e6,
            net_change=sum(ndeficient_diff, na.rm=T)/1e6) %>% 
  ungroup()

# Export
write.csv(stats, file=file.path(tabledir, "TableSX_npeople_deficient.csv"), row.names=F)
