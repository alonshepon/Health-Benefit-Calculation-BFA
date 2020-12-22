

#################################################################################
# RR
#################################################################################

# Function of Relative Risk of omega n-3  #input in units of mg/d
omega_n3_RR <- function(val,age,omega_N_raw_2019){
  
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    R <- 0
    return(R)
    
    # Else..    
  }else{
    
    agepaste<-paste("age",as.character(age),sep="")
    x<-omega_N_raw_2019$x  #g/d
    y<-omega_N_raw_2019[ , grepl( agepaste , names( omega_N_raw_2019 ) ) ]
    #add extra points to extend the RR function for outliers
    x<-heemod:::insert(x,pos=length(x),6); 
    y<-heemod:::insert(y,pos=length(y),last(y));
    x<-heemod:::insert(x,pos=0,-0.2); 
    y<-heemod:::insert(y,pos=0,1);
    xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
    return(xtrans(val))
  }
  
}

# Function of Relative Risk of high red meat for various outcomes #input in units of g/d
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
      #add extra points to extend the RR function for outliers
      x<-heemod:::insert(x,pos=length(x),1000); y<-heemod:::insert(y,pos=length(y),last(y));
      x<-heemod:::insert(x,pos=0,-200); y<-heemod:::insert(y,pos=0,1);
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
    }
    
    if (meat_outcome==441){ # Colon and rectum cancer REI_id=441
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Colon and rectum cancer",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      #add extra points to extend the RR function for outliers
      x<-heemod:::insert(x,pos=length(x),1000); y<-heemod:::insert(y,pos=length(y),last(y));
      x<-heemod:::insert(x,pos=0,-200); y<-heemod:::insert(y,pos=0,1);
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
    }
    
    if (meat_outcome==493){ # Ischemic heart disease REI_id=493
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Ischemic heart disease",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      #add extra points to extend the RR function for outliers
      x<-heemod:::insert(x,pos=length(x),1000); y<-heemod:::insert(y,pos=length(y),last(y));
      x<-heemod:::insert(x,pos=0,-200); y<-heemod:::insert(y,pos=0,1);
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
    }
    
    if (meat_outcome==495){ # Ischemic stroke REI_id=495
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Ischemic stroke",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      #add extra points to extend the RR function for outliers
      x<-heemod:::insert(x,pos=length(x),1000); y<-heemod:::insert(y,pos=length(y),last(y));
      x<-heemod:::insert(x,pos=0,-200); y<-heemod:::insert(y,pos=0,1);
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
    }
    
    if (meat_outcome==496){ # Intracerebral hemorrhage REI_id=496
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Intracerebral hemorrhage",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
      #add extra points to extend the RR function for outliers
      x<-heemod:::insert(x,pos=length(x),1000); y<-heemod:::insert(y,pos=length(y),last(y));
      x<-heemod:::insert(x,pos=0,-200); y<-heemod:::insert(y,pos=0,1);
    }
    
    if (meat_outcome==497){ # Subarachnoid hemorrhage REI_id=496 
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Subarachnoid hemorrhage",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      #add extra points to extend the RR function for outliers
      x<-heemod:::insert(x,pos=length(x),1000); y<-heemod:::insert(y,pos=length(y),last(y));
      x<-heemod:::insert(x,pos=0,-200); y<-heemod:::insert(y,pos=0,1);
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
    }
    
    if (meat_outcome==976){ # Diabetes mellitus type 2 REI_id=976 
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Diabetes mellitus type 2",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      #add extra points to extend the RR function for outliers
      x<-heemod:::insert(x,pos=length(x),1000); y<-heemod:::insert(y,pos=length(y),last(y));
      x<-heemod:::insert(x,pos=0,-200); y<-heemod:::insert(y,pos=0,1);
      xtrans<-splinefun(x, y,method = c("monoH.FC"),ties = mean)
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
  
  # Rename nutrient
  if(nutrient=="Vitamin A, RAE"){nutrient <- "VitA"}
  if(nutrient=="Vitamin B-12"){nutrient <- "B12"}
  
  # load EAR values based on input of nutrient and country SDI 
  EAR <- EAR_requirements %>% filter(age_groups==age & sex_groups==sex)
  EAR <- EAR[ , grepl( nutrient , names( EAR_requirements ) ) ]
  EAR <- as.numeric(EAR)
  
  # Build Risk curve based on CMD of normal distribution centered at EAR
  # 10% CV for cdf justification can be found here: Risk–benefit analysis of micronutrients, Renwick, 2004, 
  # https://ec.europa.eu/food/sites/food/files/safety/docs/labelling_nutrition-supplements-responses-ilsi_annex1_en.pdf
  if(nutrient=="B12"){
    calc_r <- function(x){1-pnorm(x,mean=EAR, sd=EAR*0.25)}   #RR (based on the probability method: 1-cmd(normal distribution with mean equal to EAR and 10% CV))
  }else{
    calc_r <- function(x){1-pnorm(x,mean=EAR, sd=EAR*0.1)}   #RR (based on the probability method: 1-cmd(normal distribution with mean equal to EAR and 10% CV))
  }
  
  # Calculate thing
  r_val <- calc_r(val)
  
  # Return
  return(r_val)
  
}

#################################################################################
# Calculate summary exposure values (SEVs)
#################################################################################

# SEV of omega n-3
omega_n3_SEV <- function(Intake,age, val_lo, val_hi, omega_N_raw_2019,omega_n3_RR){
  
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    SEV <- 0
    
    # Else..    
  }else{
    agepaste <- paste("age",as.character(age),sep="")
    y <- omega_N_raw_2019[ , grepl( agepaste , names( omega_N_raw_2019 ) ) ]
    RRmax <- last(y)
    integrant<-function(x){Intake(x)*omega_n3_RR(x,age,omega_N_raw_2019)}
    int <- (integrate(integrant, lower=val_lo, upper=val_hi))
    SEV <- ((int$value-RRmax)/(1-RRmax)*100)
  }
  
  # Prevent negative
  SEV <- pmax(SEV, 0)
  
  return(SEV)
  
}

# SEV of high red meat for various outcomes
red_meat_SEV <- function(Intake,age, val_lo, val_hi, meat_outcome, red_meat_2019,red_meat_RR){
  
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    SEV <- 0
    
    # Else..(adolescents and adults)   
  }else{
    agepaste<-paste("age",as.character(age),sep="")   #age
    
    
    if (meat_outcome==429){ # breast cancer REI_id=429
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Breast cancer",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      RRmax<-last(y)
      integrant<-function(x){Intake(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
      int<-(integrate(integrant, lower=val_lo, upper=val_hi))
      SEV<-((int$value-1)/(RRmax-1)*100)
      
    }
    
    if (meat_outcome==441){ # Colon and rectum cancer REI_id=441
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Colon and rectum cancer",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      RRmax<-last(y)
      integrant<-function(x){Intake(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
      int<-(integrate(integrant, lower=val_lo, upper=val_hi))
      SEV<-((int$value-1)/(RRmax-1)*100)
    }
    
    if (meat_outcome==493){ # Ischemic heart disease REI_id=493
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Ischemic heart disease",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      RRmax<-last(y)
      integrant<-function(x){Intake(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
      int<-(integrate(integrant, lower=val_lo, upper=val_hi))
      SEV<-((int$value-1)/(RRmax-1)*100)
      
    }
    
    if (meat_outcome==495){ # Ischemic stroke REI_id=495
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Ischemic stroke",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      RRmax<-last(y)
      integrant<-function(x){Intake(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
      int<-(integrate(integrant, lower=val_lo, upper=val_hi))
      SEV<-((int$value-1)/(RRmax-1)*100)
    }
    
    if (meat_outcome==496){ # Intracerebral hemorrhage REI_id=496
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Intracerebral hemorrhage",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      RRmax<-last(y)
      integrant<-function(x){Intake(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
      int<-(integrate(integrant, lower=val_lo, upper=val_hi))
      SEV<-((int$value-1)/(RRmax-1)*100)
      
    }
    
    if (meat_outcome==497){ # Subarachnoid hemorrhage REI_id=496 
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Subarachnoid hemorrhage",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      RRmax<-last(y)
      integrant<-function(x){Intake(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
      int<-(integrate(integrant, lower=val_lo, upper=val_hi))
      SEV<-((int$value-1)/(RRmax-1)*100)
      
    }
    
    if (meat_outcome==976){ # Diabetes mellitus type 2 REI_id=976 
      
      # build age specific RR
      meat_raw_2019_outcome<-red_meat_2019[red_meat_2019$Diet.high.in.red.meat=="Diabetes mellitus type 2",];
      y<-meat_raw_2019_outcome[ , grepl( agepaste , names( meat_raw_2019_outcome ) ) ]
      RRmax<-last(y)
      integrant<-function(x){Intake(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
      int<-(integrate(integrant, lower=val_lo, upper=val_hi))
      SEV<-((int$value-1)/(RRmax-1)*100)
      
    }
    
  }
  
  # Prevent negative
  SEV <- pmax(SEV, 0)
  
  return(SEV)
  
}

# SEV of zinc/iron/vitamin A
micronutrient_SEV <- function(Intake, age, sex, nutrient, country_SDIgroup, EAR_requirements, val_lo, val_hi){
  
  integrant<-function(x){micronutrient_RR(x,age,sex, nutrient, country_SDIgroup, EAR_requirements)*Intake(x)}
  res<-integrate(integrant, lower=val_lo, upper=val_hi)
  SEV<-res$value*100
  
  # Prevent negative
  #SEV <- pmax(SEV, 0)
  
  # Return
  return(SEV)
  
}




###################################################################################################3
# Calculate population attributable fractions (PAFs)
###################################################################################################3

red_meat_PAF <- function(Intake_br, Intake_hr, age, meat_outcome, red_meat_2019, red_meat_RR, flag_meat, 
                         val_lo_br, val_hi_br, val_lo_hr, val_hi_hr){
  
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    PAF <- 0
    
    # Else..(adolescents and adults)   
  }else{
    agepaste<-paste("age",as.character(age),sep="")   #age
    
    
    #(meat_outcome==429 breast cancer REI_id=429
    #(meat_outcome==441 Colon and rectum cancer REI_id=441
    #(meat_outcome==493) Ischemic heart disease REI_id=493
    #(meat_outcome==495) Ischemic stroke REI_id=495
    #(meat_outcome==496) Intracerebral hemorrhage REI_id=496
    #(meat_outcome==497) Subarachnoid hemorrhage REI_id=497 
    #(meat_outcome==976) Diabetes mellitus type 2 REI_id=976 
    
    # build age specific calculation
    integrant_br <- function(x){Intake_br(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
    int_br <- (integrate(integrant_br, lower=val_lo_br, upper=val_hi_br, rel.tol = 0.001))
    integrant_hr <- function(x){Intake_hr(x)*red_meat_RR(x,age,meat_outcome,red_meat_2019)}
    int_hr <- (integrate(integrant_hr, lower=val_lo_hr, upper=val_hi_hr,  rel.tol = 0.001))
    
    if(flag_meat==1){
      PAF <- (int_hr$value-int_br$value) / int_br$value  #relative change
    }else{
      PAF <- (int_br$value-1) / int_br$value
      PAF <- pmax(0, PAF)
    }        
  }
  
  return(PAF)
  
}

# PAF of omega n-3
omega_n3_PAF <- function(Intake_br, Intake_hr, age, omega_N_raw_2019, omega_n3_RR, flag_omega,
                         val_lo_br, val_hi_br, val_lo_hr, val_hi_hr){
  
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    PAF <- 0
    
    # Else..    
  }else{
    
    agepaste<-paste("age",as.character(age),sep="")
    y<-omega_N_raw_2019[ , grepl( agepaste , names( omega_N_raw_2019 ) ) ]
    lowest_risk<-last(y)
    
    integrant_br <- function(x){Intake_br(x)*omega_n3_RR(x,age,omega_N_raw_2019)}
    int_br <- (integrate(integrant_br, lower=val_lo_br, upper=val_hi_br))
    
    integrant_hr <- function(x){Intake_hr(x)*omega_n3_RR(x,age,omega_N_raw_2019)}
    int_hr <- (integrate(integrant_hr,lower=val_lo_hr, upper=val_hi_hr))
    
    if(flag_omega==1){
      PAF <- (int_hr$value-int_br$value)/int_br$value  #relative change
    }else{
      PAF <- (int_br$value-lowest_risk)/int_br$value
    }
    
  }
  
  return(PAF)
  
}


