
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(dosresmeta)
library(rms)
library(openxlsx)
library(dbplyr)
library(tidyverse)
library(MASS)
library(ggplot2)
library(fitdistrplus)
library(Hmisc)

# Directories (outside repository)
datadir1 <- "/Users/cfree/Dropbox/Health Benefits calculations/Data/IHME/definitions" # Chris Free's computer
datadir2 <- "/Users/cfree/Dropbox/Health Benefits calculations/Data/EAR/" # Chris Free's computer

# Directories (in repository)
outputdir <- "output"
plotdir <- "figures"
codedir <- "code"

# Read RR GBD 2019
omega_N_raw_2019 <- readxl::read_excel(file.path(datadir1, "omega_RR_2019.XLSX"))
EAR_requirements <- readxl::read_excel(file.path(datadir2, "EAR_requirements_GBDgroups.xlsx"))


# Helper functions
################################################################################

# Age groups
#5 1-4 years 
#6 5-9 years 
#7 10-14 years
#8 15-19 years
#9 20-24 years
#10 25-29 years
#11 30-34 years
#12 35-39 years
#13 40-44 years 
#14 45-49 years 
#15 50-54 years 
#16 55-59 years 
#17 60-64 years 
#18 65-69 years 
#19 70-74 years 
#20 75-79 years
#30 80-84 years
#31 85-89 years
#32 90-94 years
#33 95-99 years

# Build something
x  <- seq(0,1000,1)
y1 <- 1.15^((250-x)/100)   # RR of GBD based on Lancet 2017
y2 <- -0.0014*x+1.35        # RR of GBD based on linearized Mozzafarian and Rimm 2006
df <- data.frame(x,y1,y2)

# Plot something
ggplot(df, aes(x)) +
  geom_line(aes(y=y1), colour="red") +
  geom_line(aes(y=y2), colour="green")


# Helper functions
################################################################################

# Function to...
omega_n3_RR <- function(val,age,omega_N_raw_2019){
  
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    R <- 0
    return(R)
  
  # Else..    
  }else{
    
    agepaste<-paste("age",as.character(age),sep="")
    x<-omega_N_raw_2019$x*1000   #mg/d
    y<-omega_N_raw_2019[ , grepl( agepaste , names( omega_N_raw_2019 ) ) ]
    set.seed(1)
    plot(x, y)
    xx <- rcspline.eval(x, inclx=TRUE, nk=4)
    knots <- attr(xx, "knots")
    coef <- lsfit(xx, y)$coef
    options(digits=4)
    # rcspline.restate must ignore intercept
    w <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
    # could also have used coef instead of coef[-1], to include intercept
    cat(attr(w,"latex"), sep="\n")
    
    xtrans <- eval(attr(w, "function"))
    # This is an S function of a single argument
    lines(x, coef[1] + xtrans(x), type="l")
    # Plots fitted transformation
    
    xtrans <- rcsplineFunction(knots, coef)
    xtrans
    lines(x, xtrans(x), col='blue')
    return(xtrans(val))
  }
  
}

# Function to...
zinc_iron_vita_RR <- function(val, age, sex, nutrient, country_SDIgroup, EAR_requirements){
  
  #extract EAR per the nutrient, age, sex requested
  #units mg/d
  #criteria to assess iron availability
  if (nutrient=="Iron" & country_SDIgroup=="low"){nutrient<-"Iron.5%"}
  if (nutrient=="Iron" & country_SDIgroup=="middle"){nutrient<-"Iron.10%"}
  if (nutrient=="Iron" & country_SDIgroup=="high"){nutrient<-"Iron.12%"}
  
  # Something
  EAR <- EAR_requirements %>% filter(age_groups==age & sex_groups==sex)
  EAR <- EAR[ , grepl( nutrient , names( EAR_requirements ) ) ]
  EAR <- as.numeric(EAR)
  
  # Function to calculate thing
  # 10% CV for cdf justification can be found here: Risk–benefit analysis of micronutrients, Renwick, 2004, 
  # https://ec.europa.eu/food/sites/food/files/safety/docs/labelling_nutrition-supplements-responses-ilsi_annex1_en.pdf
  calc_r <- function(x){1-pnorm(x,mean=EAR, sd=EAR*0.1)}   #RR (based on the probability method: 1-cmd(normal distribution with mean equal to EAR and 10% CV))
  
  # Calculate thing
  r_val <- calc_r(val)
  
  # Return
  return(r_val)

}

