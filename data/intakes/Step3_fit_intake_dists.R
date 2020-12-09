

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

# Combine countries
data <- data %>% 
  mutate(country_final=recode(country,
                              "Laos"="Laos & Philippines",
                              "Philippines"="Laos & Philippines",
                              "Uganda"="Uganda & Zambia",
                              "Zambia"="Uganda & Zambia"))


# Fit distributions
################################################################################

# Packages
library(fitdistrplus)

# Build key
dist_key <- data %>% 
  dplyr::select(country_final, nutrient, sex, age_group) %>% 
  unique() 

# Distributions
dists <- c("gamma", "log-normal")

# Build container
dist_fits <- dist_key %>% 
  mutate(best_dist=NA, 
         g_shape=NA,
         g_rate=NA,
         g_ks=NA,
         ln_meanlog=NA,
         ln_sdlog=NA,
         ln_ks=NA)

# Loop through and fit distributions
# for(i in 1:10){
for(i in 1:nrow(dist_fits)){
# best_dists <- purrr::map_df(1:10, function(i){
  
  # Params
  print(i)
  country_do <- dist_fits$country_final[i]
  nutrient_do <- dist_fits$nutrient[i]
  sex_do <- dist_fits$sex[i]
  age_group_do <- dist_fits$age_group[i]
  
  # Subset data
  sdata <- data %>% 
    filter(country_final==country_do & nutrient==nutrient_do & sex==sex_do & age_group==age_group_do)
  
  # Inspect skewness
  # descdist(sdata$intake)
  
  # Fit distributions
  dist_names <- c("gamma", "log-normal")
  gamma_fit <- try(fitdist(sdata$intake, distr="gamma"))
  lognorm_fit <- try(fitdist(sdata$intake, distr="lnorm"))
  
  # Plot distributions against data
  # denscomp(list(gamma_fit, lognorm_fit), legendtext = dist_names)
  
  # Compete distributions (goodness-of-fit)
  # gof_stats <- gofstat(list(gamma_fit, lognorm_fit), fitnames = dist_names)
  
  # If gamma worked
  if(!inherits(gamma_fit, "try-error")){
    gof_stats <- gofstat(gamma_fit)
    dist_fits$g_ks[i] <- gof_stats$ks
    dist_fits$g_shape[i] <- coef(gamma_fit)["shape"]
    dist_fits$g_rate[i] <- coef(gamma_fit)["rate"]
  }

  # If log-normal worked
  if(!inherits(lognorm_fit, "try-error")){
    gof_stats <- gofstat(lognorm_fit)
    dist_fits$ln_ks[i] <- gof_stats$ks
    dist_fits$ln_meanlog[i] <- coef(lognorm_fit)["meanlog"]
    dist_fits$ln_sdlog[i] <- coef(lognorm_fit)["sdlog"]
  }
  
}

# Mark best distribution
dist_fits_out <- dist_fits %>% 
  mutate(best_dist=ifelse(ln_ks < g_ks | is.na(g_ks), "log-normal", "gamma"))

# Inspect
freeR::complete(dist_fits_out)

# Export best distributions
write.csv(dist_fits_out, file=file.path(outputdir, "habitual_nutrient_intakes_by_age_sex_9countries_distribution_fits.csv"), row.names=F)



