

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(cowplot)
library(latex2exp)
library(tidyverse)
library(viridis)
library(hrbrthemes)

# Directories (in repository)
outputdir <- "output"
plotdir <- "figures"
codedir <- "code"

# Read DALYs data
dalys_fish_orig <- readRDS(file.path(outputdir, "my_data.rds"))
dalys_meat_orig <- readRDS(file.path(outputdir, "my_meat_data.rds"))

# Read population data
pop_orig <- readRDS(file.path(outputdir, "population_all.rds"))

# Read country list
countries_level_3 <- readRDS(file.path(outputdir, "countries_level3.rds"))


# Build data
################################################################################

#---------age groups
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

#-----------causes
# 409 noncommunicable diseases
# 295 Communicable, maternal, neonatal, and nutritional diseases
# 294 all cause
#-------------rei
#121 low seafood
#97 zinc
#96 vitamin A
#95 iron

# Age ids
age_id <- c(seq(5,20),30,31,32,33) 

# Format population data
pop <- pop_orig %>%
  select(-c("lower","upper","location_id"))

# Format DALYs fish
omega <- dalys_fish %>% 
  filter(measure==2 & metric==1 & sex!=3 & rei==121 & location %in% countries_level_3$location_id & age %in% age_id)
# omega1<- left_join(omega, population_all, by=c("location_name" = "location_name","year"="year_id","age"="age_group_id","sex"="sex_id"),all.x=TRUE)
# omega1<-omega1%>%rename(population=val.y, DALY=val.x)

# Format DALYs meat
dalys_meat <- dalys_meat_orig %>% 
  filter(measure==2 & metric==1 & sex!=3 & rei==121 & location %in% countries_level_3$location_id & age %in% age_id)
meat<-b%>%filter(measure==2 & metric==1 & sex!=3 & rei==121 & location %in% countries_level_3$location_id & age %in% age_id)
meat1<- left_join(omega, population_all, by=c("location_name" = "location_name","year"="year_id","age"="age_group_id","sex"="sex_id"),all.x=TRUE)
meat1<-omega1%>%rename(population=val.y, DALY=val.x)

# Zinc
zinc <- a %>% 
  filter(measure==2 & metric==1 & sex!=3 & rei==97 & age %in% age_id & location %in% countries_level_3$location_id) %>%
  arrange(val)

# Iron
iron <- a %>% 
  filter(measure==2 & metric==1 & sex!=3 & rei==95 & age %in% age_id & location %in% countries_level_3$location_id) %>% 
  arrange(val)

#---linear function to predict DALY in 2030
#r30<-function(val,year){t<-lm(val~year)
#return(t$coef[1]+2030*(t$coef[2]))}

r30<-function(val,year){tt<-loess(val~year,span=10,control = loess.control(surface = "direct"))
return(max(predict(tt, newdata = 2030),0))}  #make sure DALYS are not negative


j<-omega1%>% group_by(location_name,sex, age) %>% mutate(DALY2030 = r30(DALY,year)) %>%
 filter(year==2017) %>% mutate(delta_DALY=(DALY2030/DALY))%>%
  group_by(location_name,year)%>%mutate(pop_adjust_DALY=sum(population*DALY)/sum(population))%>%
  mutate(pop_adjust_delta_DALY=sum(population*delta_DALY)/sum(population))%>%mutate(population_total=sum(population))
 #%>% mutate(delta_DALY_sym=sign(delta_DALY)*log10(1+abs(delta_DALY)*log(10)))
#delta_DALY_sym, bi symmetrical log transformation, https://pdfs.semanticscholar.org/70d5/3d9f448e6f2c10bd87a4a058be64f5af7dbc.pdf

#-------------example check
afgh_11<-omega %>%filter(measure==2 & metric==1 & sex==1 & rei==121 & location_name=='Turkey' & age==11) 
t<-lm(afgh_11$val~afgh_11$year)
t_reg<-t$coef[1]+2030*(t$coef[2])
fineX <- seq(min(afgh_11$year)-10, max(afgh_11$year)+20 , 1)
s <- fineX > max(afgh_11$year) | fineX < min(afgh_11$year)
low <- loess(afgh_11$val~afgh_11$year, span = 10, control = loess.control(surface = "direct"))
res <- predict(low, newdata = fineX)

##---------------------
####interp <- spline(afgh_11$year,afgh_11$val , xout= fineX , method = c("natural"))
#####plot(fineX,interp$y)

#plot(afgh_11$year,afgh_11$val, xlim = range(fineX), ylim = range(res))
#lines(fineX, res, col = "blue", lwd = 3)
#points(fineX[s], res[s], col = "green", cex = .6, pch = 3)

#plot(afgh_20$year,afgh_20$val)
#t$coef[1]+t$coef[2]*2030
#mutate(loess = predict(loess(y ~ x, data = data)))


#----------plot
string <- "DALY ratio, (2030/2017)"
thet <- ""# $\\Delta$"
ylab.fig2 <- TeX(paste(thet, string, sep = ""))
#reorder(location_name,-delta_DALY)
g<-ggplot(j,aes(x=fct_reorder(location_name, delta_DALY, .fun='median',.desc = TRUE),y=delta_DALY, color=group.SDI))+
  geom_boxplot(outlier.size = 0.3,alpha=0.1)+
  geom_point(aes(y=pop_adjust_delta_DALY),alpha=1,shape=19,na.rm=TRUE,size=2.5)+
  theme(axis.text.x=element_text(angle = -90, hjust = 0,size=6))+coord_cartesian(ylim = c(0, 5))+xlab("")+ylab(ylab.fig2)
g


string <- "DALY ratio, (2030/2017)"
thet <- ""# $\\Delta$"
ylab.fig2 <- TeX(paste(thet, string, sep = ""))
j1<-j%>%group_by(location)%>% summarise(population=sum(population),location_name=unique(location_name),pop_adjust_delta_DALY=unique(pop_adjust_delta_DALY),pop_adjust_DALY=unique(pop_adjust_DALY),SDI=unique(group.SDI))
g<-ggplot(j1,aes(x=log10(pop_adjust_DALY),y=pop_adjust_delta_DALY,fill=SDI,size = population))+
  geom_point(alpha=0.5,shape=21,color='black')+
  scale_size(range = c(2,15),breaks=c(1e+7,1e+8,5e+8,1e+9),name="Population")+
  #scale_color_brewer(type='dev',palette="Set1")+
  scale_fill_viridis_d(aesthetics = "colour", option="A") +
  #scale_colour_viridis_d(aesthetics = "colour")+
  theme(legend.position="bottom")+ 
  geom_text(aes(label=location_name),alpha=0.5,nudge_x = -0.18,nudge_y = 0.1,size=3,check_overlap = TRUE)+
  geom_hline(yintercept=1,size=0.5,alpha=.2)+geom_vline(xintercept=log10(median(j1$pop_adjust_DALY,na.rm=TRUE)),size=0.5,alpha=.2)+coord_cartesian(ylim = c(0, 3),xlim = c(0,6))+xlab("log10(DALY) (2017)")+ylab(ylab.fig2)+
  theme_minimal() +
  theme(legend.position="right") 
g
ggsave(g, file="./plot.pdf", width=40, height=20, units = "cm", dpi=500) 

