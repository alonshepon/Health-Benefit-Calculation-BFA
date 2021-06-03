
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
plotdir <- "figures"
tabledir <- "tables"

# Read problem country key
prob_key <- read.csv("data/countries_with_bug.csv", as.is=T)

# Read SEVs
sevs_orig <- read.csv("output/2030_sevs_base_high_road_final_diversity_disagg_no_problem_countries.csv", as.is=T)


# Build data
################################################################################

# Age groups
age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                "30-34", "35-39", "40-44", "45-49", 
                "50-54", "55-59", "60-64", "65-69",
                "70-74", "75-79", "80-84", "85-89", "90-94", "95-99")

# Format
sevs <- sevs_orig %>% 
  select(nutrient, country, sex, age_group, sev_delta) %>% 
  spread(key="sex", value="sev_delta") %>% 
  mutate(difference=Females-Males) %>% 
  mutate(age_group=factor(age_group, levels=age_groups))

# Inspect
hist(sevs$difference)

# Build key
stats3 <- sevs %>% 
  drop_na() %>% 
  group_by(nutrient, age_group) %>% 
  summarize(n=n(),
            n_f_lower=sum(Females < Males),
            p_f_lower=n_f_lower/n*100,
            sev_delta_avg_m=mean(Males),
            sev_delta_avg_f=mean(Females),
            sev_delta_m_f_diff_avg=mean(difference)) %>% # postive mean female larger, negatv
  ungroup() %>% 
  mutate(df=NA,
         tstat=NA,
         pvalue=NA)

# Perform t-test
for(i in 1:nrow(stats3)){
  
  # Subset data
  nutr_do <- stats3$nutrient[i]
  age_do <- stats3$age_group[i]
  sdata <- sevs %>% 
    filter(nutrient==nutr_do & age_group==age_do & difference!=0) 
  
  # Perform t-test
  ttest <- t.test(sdata$Females, sdata$Males, paired=T)
  
  # Record results
  stats3$tstat[i] <- ttest$statistic
  stats3$df[i] <- ttest$parameter
  stats3$pvalue[i] <- ttest$p.value
  
}

# Format
stats4 <- stats3 %>% 
  mutate(sig=ifelse(pvalue<=0.05, "Significant", "Not significant"),
         direction=ifelse(sev_delta_avg_f<sev_delta_avg_m, 
                          "Greater improvements for females", 
                          "Greater improvements for males"),
         label=paste(sig, direction, sep="-"),
         label=ifelse(grepl("Not", label), "No significant\ndifference in ΔSEVS", label),
         label=recode(label, 
                      "Significant-Greater improvements for females"="Signficantly greater\nΔSEVS for females",
                      "Significant-Greater improvements for males"="Signficantly greater\nΔSEVS for males"))

# Plot
g <- ggplot(stats4, aes(x=age_group, y=nutrient, fill=label)) +
  geom_raster() +
  # Text labels
  geom_text(mapping=aes(x=age_group, y=nutrient, label=round(p_f_lower)), 
            color="black", size=1) +
  # Labels
  labs(x="Age group (yrs)", y="") +
  # Legend
  scale_fill_manual(name="", values=c("grey80", "red", "blue")) +
  # Theme
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        strip.text=element_text(size=8),
        plot.title=element_text(size=10),
        # Gridlines
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g

ggsave(g, filename=file.path(plotdir, "FigSX_delta_sevs_males_v_females.png"), 
       width=4.5, height=2.5, units="in", dpi=600)

# Export data
write.csv(stats4, file=file.path(outputdir, "2030_delta_sev_male_v_female_comparison.csv"), row.names = F)

# Export data
write.csv(stats4, file=file.path(tabledir, "TableSX_2030_delta_sev_male_v_female_comparison.csv"), row.names = F)
