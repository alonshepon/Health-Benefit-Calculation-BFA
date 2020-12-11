

#################################################################################
# Miscellaneous
#################################################################################

# Function to project DALYs to 2030???
r30 <- function(val, year){
  tt <- loess(val~year, span=10, control = loess.control(surface = "direct"))
  tt1 <- max(predict(tt, newdata = 2030),0)  # make sure DALYS are not negative
  return(tt1)
} 

#################################################################################
# Relative risk (RR) functions
#################################################################################

# Calculate relative risk (RR) for omega-3 fatty acids
omega_n3_RR <- function(val, age, omega_N_raw_2019){
  
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    RR <- 0

  # Else..    
  }else{
    agepaste <- paste("age",as.character(age),sep="")
    x <- omega_N_raw_2019$x*1000   #mg/d
    y <- omega_N_raw_2019[ , grepl( agepaste , names( omega_N_raw_2019 ) ) ]
    y_vec <- pull(y) # Chris added this to convert dataframe column to vector
    xtrans <- splinefun(x, y_vec, method = c("monoH.FC"),ties = mean)
    RR <- xtrans(val)
  }
  
  # Return
  return(RR)
  
}


# Calculate relative risk (RR) for high red meat for various outcomes
red_meat_RR <- function(val, age, meat_outcome, red_meat_2019){
  
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    RR <- 0
    return(RR)
    
  # Else..(adolescents and adults)   
  }else{
    
    x <- c(0,50,100,150,200)   #g/d
    agepaste <- paste("age",as.character(age),sep="")   #age
    
    # breast cancer REI_id=429
    if (meat_outcome==429){ 
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Breast cancer",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
    }
    
    # Colon and rectum cancer REI_id=441
    if (meat_outcome==441){ 
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Colon and rectum cancer",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
    }
    
    # Ischemic heart disease REI_id=493
    if (meat_outcome==493){ 
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Ischemic heart disease",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
    }
    
    # Ischemic stroke REI_id=495
    if (meat_outcome==495){ 
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Ischemic stroke",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
    }
    
    # Intracerebral hemorrhage REI_id=496
    if (meat_outcome==496){ 
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Intracerebral hemorrhage",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
    }
    
    # Subarachnoid hemorrhage REI_id=496 
    if (meat_outcome==497){ 
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Subarachnoid hemorrhage",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
    }
    
    # Diabetes mellitus type 2 REI_id=976 
    if (meat_outcome==976){ 
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Diabetes mellitus type 2",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
    }
  
    # Transform 
    RR <- xtrans(val)

  }
  
  # Return
  return(RR)
  
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
  EAR <- EAR_requirements %>% 
    filter(age_groups==age & sex_groups==sex)
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


##########################################################################################
# SEV functions
##########################################################################################

# Calculate summary exposure values (SEVs) from Vitamin A, zinc, and iron
micronutrient_SEV <- function(Intake, age, sex, nutrient, country_SDIgroup, EAR_requirements){
  
  integrant <- function(x){micronutrient_RR(x,age,sex, nutrient, country_SDIgroup, EAR_requirements)*Intake(x)}
  res <- integrate(integrant, lower=-Inf, upper=Inf)
  SEV <- res$value*100
  
  # Return
  return(SEV)
  
}

# Calculate summary exposure values (SEVs) from omega-3 fatty acids
omega_n3_SEV <- function(Intake, age, omega_N_raw_2019, omega_n3_RR){
  
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    SEV <- 0
    
  # Else..    
  }else{
    agepaste <- paste("age",as.character(age),sep="")
    y <- omega_N_raw_2019[ , grepl( agepaste , names( omega_N_raw_2019 ) ) ]
    RRmax <- last(y)
    integrant <- function(x){Intake(x)*omega_n3_RR(x, age, omega_N_raw_2019)}
    int <- (integrate(integrant, lower=-Inf, upper=Inf))
    SEV <- max((int$value-RRmax)/(1-RRmax)*100, 0)
  }
  
  # Return
  return(SEV)
  
  
}

# Calculate summary exposure values (SEVs) for red meat -- various outcomes
red_meat_SEV <- function(Intake, age, meat_outcome, red_meat_2019, red_meat_RR){
  
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



##########################################################################################
# PAF functions
##########################################################################################

# relative risk changes of high red meat for various intake changes
red_meat_PAF <- function(Intake_br, Intake_hr, age, meat_outcome, red_meat_2019, red_meat_RR, flag_meat){
  
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
    int_br_val <- int_br$value
    integrant_hr<-function(x){Intake_hr(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
    int_hr<-(integrate(integrant_hr,lower=-Inf,upper=Inf))
    int_hr_val <- int_br$value
 
    if(flag_meat==1){
      PAF<-(int_hr_val)/int_br_val  #relative change
      return(PAF)
    }else{
      PAF<-(int_hr_val-1)/int_hr_val
      return(PAF)
    }         #serve as a proper PAF
  }
}


# PAF = population attributable factor
omega_n3_PAF <- function(Intake_br, Intake_hr, age, omega_N_raw_2019, omega_n3_RR, flag_omega){
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    PAF <- 0
    return(PAF)
    
    # Else..    
  }else{
    agepaste <- paste("age", as.character(age), sep="")
    y <- omega_N_raw_2019[ , grepl( agepaste , names( omega_N_raw_2019 ) ) ]
    lowest_risk <- last(y)
    
    integrant_br <- function(x){Intake_br(x)*omega_n3_RR(x,age,omega_N_raw_2019)}
    int_br <- (integrate(integrant_br,lower=-Inf,upper=Inf)) 
    int_br_val <- int_br$value
    
    integrant_hr <- function(x){Intake_hr(x)*omega_n3_RR(x,age,omega_N_raw_2019)}
    int_hr <-(integrate(integrant_hr,lower=-Inf,upper=Inf))
    int_hr_val <- int_hr$value
    
    if(flag_omega==1){
      # PAF <- (integrant_hr)/integrant_br  # Alon's computer
      PAF <- (int_hr_val)/int_br_val  # Chris's computer
      return(PAF)
    }else{
      # PAF<- (integrant_hr-lowest_risk)/integrant_hr # Alon's computer
      PAF <- (int_hr_val-lowest_risk) / int_hr_val
      return(PAF)
    }
  }        #serve as a proper PAF

}

