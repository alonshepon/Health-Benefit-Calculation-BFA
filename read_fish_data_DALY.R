library(openxlsx)
library(tidyverse)
setwd("d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/Data/IHME/excels/")
my_files<-list.files(pattern="*.csv")
nba<-lapply(my_files,function(i){
x=read.csv(i)
x$file
x})
nba<-do.call("rbind.data.frame",nba)

##------------add country location
countries_code<-read.xlsx("d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/Data/IHME/definitions/IHME_GBD_2017_GBD_LOCATIONS_HIERARCHY_Y2018M11D18.XLSX")
countries_all_level_concise<-countries_code[,c("location_id","location_name")]
countries_level_3<-countries_code%>%filter(level==3)

##---------------HDI
countries_hdi<-read.xlsx("d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/Data/IHME/definitions/human-development-index.xlsx")
countries_hdi<-countries_hdi %>% filter(Year==2017)
##-------------SDI
countries_SDI<-read.xlsx("d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/Data/IHME/definitions/IHME_GBD_2019_SDI_1990_2019_Y2020M10D15.XLSX")
countries_SDI<-countries_SDI %>% rename("SDI"="2017") 
countries_SDI<-countries_SDI[,c("Location","SDI")]  
countries_SDI$SDI<-as.numeric(as.character(countries_SDI$SDI))  
countries_SDI<-countries_SDI %>%  mutate(group.SDI=cut((SDI), breaks=c(0,0.35,0.75,1), labels = c("low","middle","high")))


countries_all_level_concise<-merge(x=countries_all_level_concise,y=countries_hdi,by.x="location_name",by.y="Entity",all.x=TRUE)
countries_all_level_concise<-countries_all_level_concise[,c(1,2,5)]
countries_all_level_concise<-merge(x=countries_all_level_concise,y=countries_SDI,by.x="location_name",by.y="Location",all.x=TRUE)

##-----------merge data on countries and DALYs
nba<-merge(x=nba,y=countries_all_level_concise,by.x="location",by.y="location_id",all.x=TRUE)

##------------save data
setwd("d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/code/Health Benefit claculation BFA/Health-Benefit-Calculation-BFA")
saveRDS(nba, file = "my_data.rds")
saveRDS(countries_code,file="countries_code.rds")
saveRDS(countries_level_3,file="countries_level3.rds")
rm(list=ls())
