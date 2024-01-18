#Run previous code to get in data####
source('analysis/00_main.R')

library(ggpubr)
library(rstatix)
#Explore under ice temperature trends

names(DailyInterpol)
str(DailyInterpol)

# How are different parameters on Dec 21st/22nd (DOY 356) changing over time? 

DailyInterpol_winterstart <- DailyInterpol %>%
  filter(dayofyear==356)

str(DailyInterpol_winterstart)

DailyInterpol_winterstart_long <- DailyInterpol_winterstart %>%
  pivot_longer(-c(Date, year, dayofyear, weekofyear))

DailyInterpol_winterstart_long %>%
  ggplot(aes(x=year, y=value))+
  geom_point()+
  geom_smooth(method="lm", se=TRUE)+
  facet_wrap(~name, scales="free_y")

# How are different parameters on Jan 15th (DOY 15) changing over time? 

DailyInterpol_winterstart <- DailyInterpol %>%
  filter(dayofyear==15)

str(DailyInterpol_winterstart)

DailyInterpol_winterstart_long <- DailyInterpol_winterstart %>%
  pivot_longer(-c(Date, year, dayofyear, weekofyear))

DailyInterpol_winterstart_long %>%
  ggplot(aes(x=year, y=value))+
  geom_point()+
  geom_smooth(method="lm", se=TRUE)+
  facet_wrap(~name, scales="free_y")

#wtf is happening in 2018

DailyInterpol %>%
  filter(year==2018) %>%
  ggplot(aes(x=Date, y=stability_Jperm2))+
  geom_point()


DailyInterpol %>%
  filter(year==2018) %>%
  ggplot(aes(x=Date, y=stability_Jperm2))+
  geom_point()

DailyInterpol %>%
  filter(year==2018) %>%
  pivot_longer(c(Temp_0m:Temp_Bottom)) %>%
  ggplot(aes(x=Date, y=value, color=name))+
  geom_point()

#Do we have high frequency data for this time period? 
Mohonk_HF <- read.csv("data/MohonkSensor-AllData-01Apr2016to06Aug2018-ModifiedHeaders.csv") %>%
  mutate(DateTime=ymd_hm(DateTime))
str(Mohonk_HF)


Mohonk_HF %>%
  pivot_longer(-DateTime) %>%
  filter(value>0) %>%
  ggplot(aes(x=DateTime, y=value, color=name))+
  geom_point()
#No, just winter 2017-2018 for now. Maybe DCR grabbing more?


# Is the mean winter epi or hypo temp changing over time?
# Examine by month
DailyInterpol_monthly <- DailyInterpol %>%
mutate(
  Month = month(Date),
  water_year = dataRetrieval::calcWaterYear(Date)
  ) %>%
  group_by(water_year, Month) %>%
  summarise(across(stability_Jperm2:DeltaSurfaceDeep_degC, ~ mean(.x, na.rm = TRUE)))

#How does it look?
UnderIce.Monthly.SensSlopeSummary<-
  DailyInterpol_monthly %>%
  pivot_longer(-c(water_year, Month)) %>%
  filter(!name=="weekofyear") %>%
  filter(Month %in% c(12,1,2,3)) %>%
  group_by(Month, name)%>%
  dplyr::summarize(Sens_Slope=MTCC.sensSlope(x=water_year,y=value)$coefficients["Year"],
                   Sens_Intercept=MTCC.sensSlope(x=water_year,y=value)$coefficients["Intercept"],
                   Sens_pval=MTCC.sensSlope(x=water_year,y=value)$pval,
                   Sens_z_stat=MTCC.sensSlope(x=water_year,y=value)$z_stat,
                   Sens_n=MTCC.sensSlope(x=water_year,y=value)$n) %>%
  mutate(Significance=case_when(Sens_pval>=0.05 ~ " ",
                                Sens_pval<0.05 & Sens_pval >0.01 ~ "*",
                                Sens_pval<=0.01 & Sens_pval >0.001 ~ "**",
                                Sens_pval<=0.001 ~ "***")) %>%
  mutate_if(is.numeric, round, 4) %>%
  mutate(P_value_new=paste(Sens_pval,Significance,sep="")) %>%
  filter(Significance=="*")

## Looks like the hypo temp in Jan + Feb has been decreasing over time
## as well as the volume weighted mean temperature in Feb

#Plot just those 3 with statistically significant Sens slope
DailyInterpol_monthly %>%
  pivot_longer(-c(water_year, Month)) %>%
  filter(!name=="weekofyear") %>%
  filter(Month %in% c(1,2) & name=="HypoTemp_degC" | Month %in% c(2) & name=="VolumeWeightedMeanTemp_degC") %>%
  mutate(Month = factor(Month, 
                        labels=c("Jan","Feb"))) %>%
  ggplot(aes(x=water_year, y=value, color=name))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  facet_wrap(name~Month, scales="free", nrow=2)+
  theme(legend.position="none")

#Plot all 
# DailyInterpol_test %>%
#   pivot_longer(-c(water_year, Month)) %>%
#   filter(!name=="weekofyear") %>%
#   filter(Month %in% c(12,1,2,3)) %>%
#   ggplot(aes(x=water_year, y=value, color=name))+
#   geom_point()+
#   geom_smooth(method="lm",se=FALSE)+
#   facet_wrap(name~Month, scales="free")+
#   theme(legend.position="none")


ggsave( 
  "figures/UnderIce_SigSens_Temp_trends.jpg",
  width = 6,
  height = 4,
  units = "in",
  dpi = 600
)


# Is the mean winter epi or hypo temp changing over time?
# Define winter season by months
DailyInterpol_season <- DailyInterpol %>%
  mutate(
    Month = month(Date),
    season =
      ifelse(
        Month %in% c(12, 1, 2),
        "winter",
        ifelse(
          Month %in% c(3, 4, 5),
          "spring",
          ifelse(
            Month %in% c(6, 7, 8),
            "summer",
            ifelse(Month %in% c(9, 10, 11), "fall", "error")
          )
        )
      ),
    water_year = dataRetrieval::calcWaterYear(Date)
  ) %>%
  group_by(water_year, season) %>%
  summarise(across(stability_Jperm2:DeltaSurfaceDeep_degC, ~ mean(.x, na.rm = TRUE)))

#How does it look?
UnderIce.Season.SensSlopeSummary<-
  DailyInterpol_season %>%
  pivot_longer(-c(water_year, season)) %>%
  filter(!name=="weekofyear") %>%
  group_by(season, name)%>%
  dplyr::summarize(Sens_Slope=MTCC.sensSlope(x=water_year,y=value)$coefficients["Year"],
                   Sens_Intercept=MTCC.sensSlope(x=water_year,y=value)$coefficients["Intercept"],
                   Sens_pval=MTCC.sensSlope(x=water_year,y=value)$pval,
                   Sens_z_stat=MTCC.sensSlope(x=water_year,y=value)$z_stat,
                   Sens_n=MTCC.sensSlope(x=water_year,y=value)$n) %>%
  mutate(Significance=case_when(Sens_pval>=0.05 ~ " ",
                                Sens_pval<0.05 & Sens_pval >0.01 ~ "*",
                                Sens_pval<=0.01 & Sens_pval >0.001 ~ "**",
                                Sens_pval<=0.001 ~ "***")) %>%
  mutate_if(is.numeric, round, 4) %>%
  mutate(P_value_new=paste(Sens_pval,Significance,sep="")) %>%
  filter(Significance=="*")
## Looks like the hypo temp in Jan + Feb has been decreasing over time
## as well as the volume weighted mean temperature in Feb

#Plot all
DailyInterpol_season %>%
  pivot_longer(-c(water_year, season))  %>%
  filter(!name=="weekofyear") %>%
  ggplot(aes(x=water_year, y=value, color=name))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  facet_wrap(name~season, scales="free", nrow=2)+
  theme(legend.position="none")

#Plot just those 3 with statistically significant Sens slope
DailyInterpol_season %>%
  pivot_longer(-c(water_year, season)) %>%
  filter(!name=="weekofyear") %>%
  filter(season %in% c("spring") & name=="stability_Jperm2" | season=="fall" & name %in% c("DeltaSurfaceDeep_degC","EpiTemp_degC","buoyancyfrequency_1_s2")) %>%
  ggplot(aes(x=water_year, y=value, color=name))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  facet_wrap(name~season, scales="free", nrow=2)+
  theme(legend.position="none")+
  labs(title="Fall = 9,10,11; spring = 3,4,5")
ggsave( 
  "figures/UnderIce_SigSens_Temp_trends_season_v1.jpg",
  width = 6,
  height = 4,
  units = "in",
  dpi = 600
)


# Is the mean winter epi or hypo temp changing over time?
# Define as winter season (December 21st to March 19th)
DailyInterpol_season <- DailyInterpol %>%
  mutate(
    water_year = dataRetrieval::calcWaterYear(Date)
  ) %>%
  group_by(water_year) %>%
  mutate(season = case_when(dayofyear <= 79 | dayofyear >= 356 ~ "winter",
                            dayofyear > 79 & dayofyear <= 172 ~ "spring",
                            dayofyear > 172 & dayofyear <= 266 ~ "summer",
                            TRUE ~ "fall")) %>%
  group_by(water_year, season) %>%
  summarise(across(stability_Jperm2:DeltaSurfaceDeep_degC, ~ mean(.x, na.rm = TRUE)))

#How does it look?
UnderIce.Season.SensSlopeSummary<-
  DailyInterpol_season %>%
  pivot_longer(-c(water_year, season)) %>%
  filter(!name=="weekofyear") %>%
  group_by(season, name)%>%
  dplyr::summarize(Sens_Slope=MTCC.sensSlope(x=water_year,y=value)$coefficients["Year"],
                   Sens_Intercept=MTCC.sensSlope(x=water_year,y=value)$coefficients["Intercept"],
                   Sens_pval=MTCC.sensSlope(x=water_year,y=value)$pval,
                   Sens_z_stat=MTCC.sensSlope(x=water_year,y=value)$z_stat,
                   Sens_n=MTCC.sensSlope(x=water_year,y=value)$n) %>%
  mutate(Significance=case_when(Sens_pval>=0.05 ~ " ",
                                Sens_pval<0.05 & Sens_pval >0.01 ~ "*",
                                Sens_pval<=0.01 & Sens_pval >0.001 ~ "**",
                                Sens_pval<=0.001 ~ "***")) %>%
  mutate_if(is.numeric, round, 4) %>%
  mutate(P_value_new=paste(Sens_pval,Significance,sep="")) %>%
  filter(Significance=="*")
## Looking by season, the only statistically significant trends are in spring and fall, no winter

#Plot all
DailyInterpol_season %>%
  pivot_longer(-c(water_year, season))  %>%
  filter(!name=="weekofyear") %>%
  ggplot(aes(x=water_year, y=value, color=name))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  facet_wrap(name~season, scales="free", nrow=2)+
  theme(legend.position="none")

#Plot just those 3 with statistically significant Sens slope
DailyInterpol_season %>%
  pivot_longer(-c(water_year, season)) %>%
  filter(!name=="weekofyear") %>%
  filter(season %in% c("fall","spring") & name=="HypoTemp_degC" | season %in% c("fall") & name=="stability_Jperm2") %>%
  ggplot(aes(x=water_year, y=value, color=name))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  facet_wrap(name~season, scales="free", nrow=2)+
  theme(legend.position="none")+
  labs(title="Fall = Sept 22 - Dec 20; Spring = Mar 22 to June 20")

ggsave( 
  "figures/UnderIce_SigSens_Temp_trends_season_v2.jpg",
  width = 6,
  height = 4,
  units = "in",
  dpi = 600
)


## "White Christmas" analysis
## If we look at trends on specific days of the year, what comes out?

UnderIce.DOY.SensSlopeSummary<-
  DailyInterpol %>%
  filter(dayofyear > 336 | dayofyear < 120) %>% #earliest ice-in, latest ice-off
  mutate(water_year = dataRetrieval::calcWaterYear(Date)) %>%
  select(-c(Temp_0m:Temp_Bottom)) %>% #exclude discrete temps
  pivot_longer(-c(Date, water_year, dayofyear)) %>%
  filter(!name=="weekofyear") %>%
  group_by(dayofyear, name)%>%
  dplyr::summarize(Sens_Slope=MTCC.sensSlope(x=water_year,y=value)$coefficients["Year"],
                   Sens_Intercept=MTCC.sensSlope(x=water_year,y=value)$coefficients["Intercept"],
                   Sens_pval=MTCC.sensSlope(x=water_year,y=value)$pval,
                   Sens_z_stat=MTCC.sensSlope(x=water_year,y=value)$z_stat,
                   Sens_n=MTCC.sensSlope(x=water_year,y=value)$n) %>%
  mutate(Significance=case_when(Sens_pval>=0.05 ~ " ",
                                Sens_pval<0.05 & Sens_pval >0.01 ~ "*",
                                Sens_pval<=0.01 & Sens_pval >0.001 ~ "**",
                                Sens_pval<=0.001 ~ "***")) %>%
  mutate_if(is.numeric, round, 4) %>%
  mutate(P_value_new=paste(Sens_pval,Significance,sep="")) %>%
  filter(Significance=="*")
## The only dates that emerge are in late Feb and April 


## "White Christmas" analysis
## If we look at trends on specific WEEKs of the year, what comes out?

UnderIce.WOY.SensSlopeSummary<-
  DailyInterpol %>%
  filter(weekofyear >= 45 | weekofyear <= 15) %>% 
  mutate(water_year = dataRetrieval::calcWaterYear(Date)) %>%
  select(-c(Temp_0m:Temp_Bottom)) %>% #exclude discrete temps
  pivot_longer(-c(Date, water_year, weekofyear)) %>%
  group_by(weekofyear, name)%>%
  dplyr::summarize(Sens_Slope=MTCC.sensSlope(x=water_year,y=value)$coefficients["Year"],
                   Sens_Intercept=MTCC.sensSlope(x=water_year,y=value)$coefficients["Intercept"],
                   Sens_pval=MTCC.sensSlope(x=water_year,y=value)$pval,
                   Sens_z_stat=MTCC.sensSlope(x=water_year,y=value)$z_stat,
                   Sens_n=MTCC.sensSlope(x=water_year,y=value)$n) %>%
  mutate(Significance=case_when(Sens_pval>=0.05 ~ " ",
                                Sens_pval<0.05 & Sens_pval >0.01 ~ "*",
                                Sens_pval<=0.01 & Sens_pval >0.001 ~ "**",
                                Sens_pval<=0.001 ~ "***")) %>%
  mutate_if(is.numeric, round, 4) %>%
  mutate(P_value_new=paste(Sens_pval,Significance,sep="")) %>%
  filter(Significance=="*")
## The only dates that emerge are in late Feb and April 