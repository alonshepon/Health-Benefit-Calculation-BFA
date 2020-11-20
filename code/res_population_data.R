library(openxlsx)
library(dbplyr)
library(tidyverse)
population_male<-read.xlsx("d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/Data/population/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx")
population_female<-read.xlsx("d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/Data/population/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx")

#extract population level data per age-sex-country for 2017 

population_male_cut<-population_male%>% filter(`Region,.subregion,.country.or.area.*` %in% countries_level_3$location_name) %>%select(c(3,seq(8,29))) %>%
  rename(country=`Region,.subregion,.country.or.area.*`,year=`Reference.date.(as.of.1.July)`,`5`=`0-4`,`6`=`5-9`,`7`=`10-14`,`8`=`15-19`,`9`=`20-24`,`10`=`25-29`,
         `11`=`30-34`,`12`=`35-39`,`13`=`40-44`,`14`=`45-49`,`15`=`50-54`,`16`=`55-59`,`17`=`60-64`,`18`=`65-69`,
         `19`=`70-74`,`20`=`75-79`,`30`=`80-84`,`31`=`85-89`,`32`=`90-94`,`33`=`95-99`,`34`=`100+`) %>% 
  gather(age,population,c(`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,`30`,`31`,`32`,`33`,`34`))
population_male_cut$age <- as.numeric(as.character(population_male_cut$age))
population_male_cut<-population_male_cut%>%mutate(sex=as.numeric(1)*1)

population_female_cut<-population_female%>% filter(`Region,.subregion,.country.or.area.*` %in% countries_level_3$location_name) %>%select(c(3,seq(8,29))) %>%
  rename(country=`Region,.subregion,.country.or.area.*`,year=`Reference.date.(as.of.1.July)`,`5`=`0-4`,`6`=`5-9`,`7`=`10-14`,`8`=`15-19`,`9`=`20-24`,`10`=`25-29`,
         `11`=`30-34`,`12`=`35-39`,`13`=`40-44`,`14`=`45-49`,`15`=`50-54`,`16`=`55-59`,`17`=`60-64`,`18`=`65-69`,
         `19`=`70-74`,`20`=`75-79`,`30`=`80-84`,`31`=`85-89`,`32`=`90-94`,`33`=`95-99`,`34`=`100+`) %>% 
  gather(age,population,c(`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,`30`,`31`,`32`,`33`,`34`))
population_female_cut$age <- as.numeric(as.character(population_female_cut$age))
population_female_cut<-population_male_cut%>%mutate(sex=as.numeric(1)*2)

#----merge male and female into one file
population_all <- rbind(population_female_cut, population_male_cut)
population_all<-population_all %>% rename(location_name=country)
setwd("d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/code/Health Benefit claculation BFA/Health-Benefit-Calculation-BFA")
saveRDS(population_all, file = "population_all.rds")


