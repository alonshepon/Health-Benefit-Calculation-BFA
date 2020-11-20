
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories (outside repository)
datadir <- "/Users/cfree/Dropbox/Health Benefits calculations/Data/population/" # Chris Free's computer

# Directories (in repository)
outputdir <- "output"
plotdir <- "figures"
codedir <- "code"

# Build data
################################################################################

# Files to merge
files_merge <- list.files(datadir, pattern="*.CSV")

# Merge files
data_merged <- purrr::map_df(files_merge, function(x){
  fdata <- read.csv(file.path(datadir, x), as.is=T)
})

# Format merged data
data <- data_merged %>% 
  # Remove columns
  select(-c(age_group_name, measure_id, measure_name, metric_id, metric_name, sex_name))

# Export data
################################################################################

# Export data
saveRDS(data, file = file.path(outputdir, "population_all.rds"))


