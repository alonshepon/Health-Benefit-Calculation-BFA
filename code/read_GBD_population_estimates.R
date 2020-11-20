library(openxlsx)
library(tidyverse)
setwd("d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/Data/population/")
my_files<-list.files(pattern="*.CSV")
nba<-lapply(my_files,function(i){
  x=read.csv(i)
  x$file
  x})
nba<-do.call("rbind.data.frame",nba)
population_all<-nba%>%select(-c("age_group_name","measure_id","measure_name","metric_id","metric_name","sex_name"))
##------------save data
setwd("d:/Dropbox (Personal)/Dropbox (Personal)/Nutrient Gaps/Health Benefits calculations/code/Health Benefit claculation BFA/Health-Benefit-Calculation-BFA")
saveRDS(population_all, file = "population_all.rds")