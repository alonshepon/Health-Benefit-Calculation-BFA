

# a script to build Relative risk (RR), SEV (summary exposure values) and PAF curves (functions) for omega n-3, red meat and micronutrients intakes
# Read data
################################################################################

# Clear workspace
#rm(list = ls())

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


#read data of RR values for omega n-3 and meat from GBD 2020

# Alon's computer
# omega_N_raw_2019 <- read.xlsx('d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/code/Health Benefit claculation BFA/Health-Benefit-Calculation-BFA/Health-Benefit-Calculation-BFA/code/omega_RR_2019.xlsx')
# red_meat_2019 <- read.xlsx('d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/code/Health Benefit claculation BFA/Health-Benefit-Calculation-BFA/Health-Benefit-Calculation-BFA/code/meat_RR_2019.xlsx')
# EAR_requirements <- read.xlsx('d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/Data/EAR/EAR_requirements_GBDgroups.xlsx')

# Chris
omega_N_raw_2019 <- read.xlsx('code/omega_RR_2019.xlsx')
red_meat_raw_2019 <- read.xlsx('code/meat_RR_2019.xlsx')
EAR_requirements <- read.xlsx('code/EAR_requirements_GBDgroups.xlsx')


# Read RR GBD 2019
#omega_N_raw_2019 <- readxl::read_excel(file.path(datadir1, "omega_RR_2019.XLSX"))
#EAR_requirements <- readxl::read_excel(file.path(datadir2, "EAR_requirements_GBDgroups.xlsx"))
#red_meat_2019 <- read.xlsx('d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/code/Health Benefit claculation BFA/Health-Benefit-Calculation-BFA/Health-Benefit-Calculation-BFA/code/meat_RR_2019.xlsx')

#omega_N_raw_2019 <- read.xlsx('d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/code/Health Benefit claculation BFA/Health-Benefit-Calculation-BFA/Health-Benefit-Calculation-BFA/code/omega_RR_2019.xlsx')
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

# RR of omega n-3 (Mozzafarian and Rimm) 
#x  <- seq(0,1000,1)
#y1 <- 1.15^((250-x)/100)   # RR of GBD based on Lancet 2017
#y2 <- -0.0014*x+1.35        # RR of GBD based on linearized Mozzafarian and Rimm 2006
#df <- data.frame(x,y1,y2)

#ggplot(df, aes(x)) +
#  geom_line(aes(y=y1), colour="red") +
#  geom_line(aes(y=y2), colour="green")


# Helper functions
################################################################################


# RR
#################################################################################
#----------------------------------------- Function of Relative Risk of omega n-3
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
    #plot(x, y)
    #lines(x, xtrans(x), col='red')
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
    #plot(x, y)
    #lines(x, xtrans(x), col='red')
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
      #plot(x, y)
      #lines(x, xtrans(x), col='red')
    }
    
    if (meat_outcome==495){ # Ischemic stroke REI_id=495
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Ischemic stroke",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
      #plot(x, y)
      #lines(x, xtrans(x), col='red')
    }
    
    if (meat_outcome==496){ # Intracerebral hemorrhage REI_id=496
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Intracerebral hemorrhage",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
      #plot(x, y)
      #lines(x, xtrans(x), col='red')
    }
    
    if (meat_outcome==497){ # Subarachnoid hemorrhage REI_id=496 
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Subarachnoid hemorrhage",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
      #plot(x, y)
      #lines(x, xtrans(x), col='red')
    }
    
    if (meat_outcome==976){ # Diabetes mellitus type 2 REI_id=976 
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Diabetes mellitus type 2",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
      #plot(x, y)
      #lines(x, xtrans(x), col='red')
    }
    
   return(xtrans(val))
}
}



# Function to calculate micronutrient overal risk (deficiencies)
micronutrient_RR <- function(val, age, sex, nutrient, country_SDIgroup, EAR_requirements){
  #nutrient is a char of the nutrient required, as appears in the EAR_requirement excel
  #extract EAR per the nutrient, age, sex requested
  #units mg/d
  #age, according to GBD age groups
  #sex=1 male; sex=2 female
  #criteria to assess iron availability
  if (nutrient=="Iron" & country_SDIgroup=="low"){nutrient<-"Iron.5%"}
  if (nutrient=="Iron" & country_SDIgroup=="middle"){nutrient<-"Iron.10%"}
  if (nutrient=="Iron" & country_SDIgroup=="high"){nutrient<-"Iron.12%"}
  
  #criteria to assess zinc availability
  if (nutrient=="Zinc" & country_SDIgroup=="low"){nutrient<-"Zinc.low"}
  if (nutrient=="Zinc" & country_SDIgroup=="middle"){nutrient<-"Zinc.mod"}
  if (nutrient=="Zinc" & country_SDIgroup=="high"){nutrient<-"Zinc.high"}
  

  # load EAR values based on input of nutrient and country SDI 
  EAR <- EAR_requirements %>% filter(age_groups==age & sex_groups==sex)
  EAR <- EAR[ , grepl( nutrient , names( EAR_requirements ) ) ]
  EAR <- as.numeric(EAR)
  
  # Build Risk curve based on CMD of normal distribution centered at EAR
  # 10% CV for cdf justification can be found here: Risk–benefit analysis of micronutrients, Renwick, 2004, 
  # https://ec.europa.eu/food/sites/food/files/safety/docs/labelling_nutrition-supplements-responses-ilsi_annex1_en.pdf
  calc_r <- function(x){1-pnorm(x,mean=EAR, sd=EAR*0.1)}   #RR (based on the probability method: 1-cmd(normal distribution with mean equal to EAR and 10% CV))
  
  # Calculate thing
  r_val <- calc_r(val)
  
  # Return
  return(r_val)

}

#calculate SEV (summary exposure values) for each sex-age-location group for meat and seafood consumption
#SEV = integrate(RR*Intake)/RRmax
#################################################################################


# SEV of omega n-3

omega_n3_SEV <- function(Intake,age,omega_N_raw_2019,omega_n3_RR)
{
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    SEV <- 0
    return(SEV)
    
    # Else..    
  }else{
    agepaste<-paste("age",as.character(age),sep="")
    y<-omega_N_raw_2019[ , grepl( agepaste , names( omega_N_raw_2019 ) ) ]
    RRmax<-last(y)
    integrant<-function(x){Intake(x)*omega_n3_RR(x,age,omega_N_raw_2019)}
    int<-(integrate(integrant,lower=-Inf,upper=Inf))
    SEV<-max((int$value-RRmax)/(1-RRmax)*100,0)
    return(SEV)
  }
}




# SEV of high red meat for various outcomes
red_meat_SEV <- function(Intake,age,meat_outcome, red_meat_2019,red_meat_RR){
  
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    SEV <- 0
    return(SEV)}
  
  # Else..(adolescents and adults)   
  else{
    agepaste<-paste("age",as.character(age),sep="")   #age
    
    
    if (meat_outcome==429){ # breast cancer REI_id=429
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Breast cancer",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      RRmax<-last(y)
      integrant<-function(x){Intake(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
      int<-(integrate(integrant,lower=-Inf,upper=Inf))
      SEV<-min((int$value-1)/(RRmax-1)*100,100)
      
    }
    
    if (meat_outcome==441){ # Colon and rectum cancer REI_id=441
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Colon and rectum cancer",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      RRmax<-last(y)
      integrant<-function(x){Intake(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
      int<-(integrate(integrant,lower=-Inf,upper=Inf))
      SEV<-min((int$value-1)/(RRmax-1)*100,100)
    }
    
    if (meat_outcome==493){ # Ischemic heart disease REI_id=493
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Ischemic heart disease",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      RRmax<-last(y)
      integrant<-function(x){Intake(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
      int<-(integrate(integrant,lower=-Inf,upper=Inf))
      SEV<-min((int$value-1)/(RRmax-1)*100,100)
      
    }
    
    if (meat_outcome==495){ # Ischemic stroke REI_id=495
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Ischemic stroke",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      RRmax<-last(y)
      integrant<-function(x){Intake(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
      int<-(integrate(integrant,lower=-Inf,upper=Inf))
      SEV<-min((int$value-1)/(RRmax-1)*100,100)
    }
    
    if (meat_outcome==496){ # Intracerebral hemorrhage REI_id=496
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Intracerebral hemorrhage",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      RRmax<-last(y)
      integrant<-function(x){Intake(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
      int<-(integrate(integrant,lower=-Inf,upper=Inf))
      SEV<-min((int$value-1)/(RRmax-1)*100,100)
      
    }
    
    if (meat_outcome==497){ # Subarachnoid hemorrhage REI_id=496 
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Subarachnoid hemorrhage",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      RRmax<-last(y)
      integrant<-function(x){Intake(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
      int<-(integrate(integrant,lower=-Inf,upper=Inf))
      SEV<-min((int$value-1)/(RRmax-1)*100,100)
      
    }
    
    if (meat_outcome==976){ # Diabetes mellitus type 2 REI_id=976 
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Diabetes mellitus type 2",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      RRmax<-last(y)
      integrant<-function(x){Intake(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
      int<-(integrate(integrant,lower=-Inf,upper=Inf))
      SEV<-min((int$value-1)/(RRmax-1)*100,100)
      
    }
    
    return(SEV)
  }
}






# SEV of zinc/iron/vitamin A
micronutrient_SEV <- function(Intake, age, sex, nutrient, country_SDIgroup, EAR_requirements){
  
  integrant<-function(x){micronutrient_RR(x,age,sex, nutrient, country_SDIgroup, EAR_requirements)*Intake(x)}
  res<-integrate(integrant, lower=-Inf,upper=Inf)
  SEV<-res$value*100
  # Return
  return(SEV)
  
}




###################################################################################################3
# relative risk changes of high red meat for various intake changes
red_meat_PAF <- function(Intake_br,intake_hr,age,meat_outcome, red_meat_2019,red_meat_RR,flag_meat){
  
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    PAF <- 0
    return(PAF)}
  
  # Else..(adolescents and adults)   
  else{
    agepaste<-paste("age",as.character(age),sep="")   #age
    
    
    #(meat_outcome==429 breast cancer REI_id=429
    #(meat_outcome==441 Colon and rectum cancer REI_id=441
    #(meat_outcome==493) Ischemic heart disease REI_id=493
    #(meat_outcome==495) Ischemic stroke REI_id=495
    #(meat_outcome==496) Intracerebral hemorrhage REI_id=496
    #(meat_outcome==497) Subarachnoid hemorrhage REI_id=497 
    #(meat_outcome==976) Diabetes mellitus type 2 REI_id=976 
    
    # build age specific calculation
    integrant_br<-function(x){Intake_br(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
    int_br<-(integrate(integrant_br,lower=-Inf,upper=Inf))
    integrant_hr<-function(x){Intake_hr(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
    int_hr<-(integrate(integrant_hr,lower=-Inf,upper=Inf))
 
  if(flag_meat==1){PAF<-(integrant_hr)/integrant_br  #relative change
    return(PAF)}else
      {PAF<-(integrant_hr-1)/integrant_hr
      return(PAF)}         #serve as a proper PAF
}

}

# PAF of omega n-3

omega_n3_PAF <- function(Intake_br,intake_hr,age,omega_N_raw_2019,omega_n3_RR,flag_omega)
{
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    PAF <- 0
    return(PAF)
    
    # Else..    
  }else{
    agepaste<-paste("age",as.character(age),sep="")
    y<-omega_N_raw_2019[ , grepl( agepaste , names( omega_N_raw_2019 ) ) ]
    lowest_risk<-last(y)
    
    integrant_br<-function(x){Intake_br(x)*omega_n3_RR(x,age,omega_N_raw_2019)}
    int_br<-(integrate(integrant_br,lower=-Inf,upper=Inf))
    
    integrant_hr<-function(x){Intake_hr(x)*omega_n3_RR(x,age,omega_N_raw_2019)}
    int_hr<-(integrate(integrant_hr,lower=-Inf,upper=Inf))
    if(flag_omega==1){PAF<-(integrant_hr)/integrant_br  #relative change
    return(PAF)}else
    {PAF<-(integrant_hr-lowest_risk)/integrant_hr
    return(PAF)}
     }        #serve as a proper PAF

  }






