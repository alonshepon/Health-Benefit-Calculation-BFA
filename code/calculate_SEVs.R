
#calculate SEV (summary exposure values) for each sex-age-location group for meat and seafood consumption



#step 1: create a function that calculates SEV per age-sex-location and micronutrient/meat/omega n-3
#SEV = integrate(RR*Intake)/RRmax


#read data of RR values for omega n-3 and meat from GBD 2020

omega_N_raw_2019 <- read.xlsx('d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/code/Health Benefit claculation BFA/Health-Benefit-Calculation-BFA/Health-Benefit-Calculation-BFA/code/omega_RR_2019.xlsx')
red_meat_2019 <- read.xlsx('d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/code/Health Benefit claculation BFA/Health-Benefit-Calculation-BFA/Health-Benefit-Calculation-BFA/code/meat_RR_2019.xlsx')
EAR_requirements <- read.xlsx('d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/Data/EAR/EAR_requirements_GBDgroups.xlsx')


m=9
Intake<-function(x){y=1/sqrt(2*pi*(m/5)^2)*exp(-(x-m)^2/(2*(m/5)^2))}#normal distribution for example
c<-zinc_iron_vita_SEV(Intake, 10, 1, "Iron", "low", EAR_requirements)


b<-red_meat_SEV(Intake,10,976, red_meat_2019,red_meat_RR)
plot(Intake(seq(0:500)))
integrant<-function(x){Intake(x)}
inter<-(integrate(integrant,lower=-Inf,upper=Inf))
inter$value

# SEV of omega n-3

omega_n3_SEV <- function(Intake,age,omega_N_raw_2019,omega_n3_RR)
{
  # If..
  if(age==5|age==6 | age==7| age==8| age==9){
    
    SEV <- 0
    return(R)
    
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
    
    R <- 0
    return(R)}
  
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
zinc_iron_vita_SEV <- function(Intake, age, sex, nutrient, country_SDIgroup, EAR_requirements){
 
    integrant<-function(x){zinc_iron_vita_RR(x,age,sex, nutrient, country_SDIgroup, EAR_requirements)*Intake(x)}
res<-integrate(integrant, lower=-Inf,upper=Inf)
SEV<-res$value*100
  # Return
  return(SEV)
  
}
