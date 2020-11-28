
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
#omega_N_raw_2019 <- readxl::read_excel(file.path(datadir1, "omega_RR_2019.XLSX"))
EAR_requirements <- readxl::read_excel(file.path(datadir2, "EAR_requirements_GBDgroups.xlsx"))
red_meat_2019 <- read.xlsx('d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/code/Health Benefit claculation BFA/Health-Benefit-Calculation-BFA/Health-Benefit-Calculation-BFA/code/meat_RR_2019.xlsx')
omega_N_raw_2019 <- read.xlsx('d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/code/Health Benefit claculation BFA/Health-Benefit-Calculation-BFA/Health-Benefit-Calculation-BFA/code/omega_RR_2019.xlsx')
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

# Function of Relative Risk of omega n-3
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
    xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
    plot(x, y)
    lines(x, xtrans(x), col='red')
    return(xtrans(val))
  }
}






# Function of Relative Risk of high red meat for various outcomes
red_meat_RR <- function(val,age,meat_outcome, red_meat_2019){
  
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    R <- 0
    return(R)}
    
    # Else..(adolescents and adults)   
  else{
    x<-c(0,50,100,150,200)   #g/d
    agepaste<-paste("age",as.character(age),sep="")   #age
    
    
    if (meat_outcome==429){ # breast cancer REI_id=429
    
    # build age specific RR
    meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Breast cancer",];
    y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
    xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
    plot(x, y)
    lines(x, xtrans(x), col='red')
    }
    
    if (meat_outcome==441){ # Colon and rectum cancer REI_id=441
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Colon and rectum cancer",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
      plot(x, y)
      lines(x, xtrans(x), col='red')
    }
    
    if (meat_outcome==493){ # Ischemic heart disease REI_id=493
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Ischemic heart disease",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
      plot(x, y)
      lines(x, xtrans(x), col='red')
    }
    
    if (meat_outcome==495){ # Ischemic stroke REI_id=495
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Ischemic stroke",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
      plot(x, y)
      lines(x, xtrans(x), col='red')
    }
    
    if (meat_outcome==496){ # Intracerebral hemorrhage REI_id=496
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Intracerebral hemorrhage",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
      plot(x, y)
      lines(x, xtrans(x), col='red')
    }
    
    if (meat_outcome==497){ # Subarachnoid hemorrhage REI_id=496 
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Subarachnoid hemorrhage",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
      plot(x, y)
      lines(x, xtrans(x), col='red')
    }
    
    if (meat_outcome==976){ # Diabetes mellitus type 2 REI_id=976 
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Diabetes mellitus type 2",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
      plot(x, y)
      lines(x, xtrans(x), col='red')
    }
    
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

