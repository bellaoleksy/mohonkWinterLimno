#Run previous code to get in data####
source('analysis/00_main.R')
select <- dplyr::select
library(ggpubr)
library(rstatix)
#Explore under ice temperature trends

names(DailyInterpol)
str(DailyInterpol)

#Filter DailyInterpol by ice on and ice off dates
DailyInterpol_trim <- DailyInterpol %>%
  mutate(water_year = dataRetrieval::calcWaterYear(Date),
         Date_fed = hydro.day(Date)) %>%
  left_join(., MohonkIce %>%
              mutate(water_year = dataRetrieval::calcWaterYear(IceOutDate)) %>%
              select(water_year, IceInDayofYear_fed, IceOutDayofYear_fed, LengthOfIceCover_days),
            by="water_year") %>%
  group_by(water_year) %>%
  filter(Date_fed >= IceInDayofYear_fed & Date_fed <= IceOutDayofYear_fed) %>%
  mutate(temperatureDifferenceTop0mvsBottom9m=Temp_1m-Temp_9m,
         limit0.1=ifelse(temperatureDifferenceTop0mvsBottom9m<(-0.1),"Low0.1","High0.1"))

#Look at the time series
colFun<-colorRampPalette(c("light blue", "dark blue"))
legend_labels<-c("0m","1m","2m","3m","4m","5m","6m","7m","8m","9m")

DailyInterpol_trim %>%
  pivot_longer(Temp_0m:Temp_9m) %>%
  ggplot(aes(x=Date_fed, y=value, color=name))+
  geom_point()+
  scale_color_manual(values = colFun(10), labels = legend_labels) +
  labs(color = bquote("Water\ntemperature\ndepth"),
       x="Days since Oct 1")+
  geom_vline(aes(xintercept = IceInDayofYear_fed),
             size = 0.5) +
  geom_vline(aes(xintercept = IceOutDayofYear_fed),
             size = 0.5) +
  facet_wrap(.~water_year, scales="free_y")

#This makes me realize there are some years with poor data coverage that we should remove before the trend analysis
DailyInterpol_trim_summary <- DailyInterpol_trim %>%
  filter(!water_year %in% c(1985, 1990, 2001, 2009, 2014, 2015, 2019, 2020)) %>%
  group_by(water_year) %>%
  summarize(EpiTemp_degC=median(EpiTemp_degC, na.rm=TRUE),
            HypoTemp_degC=median(HypoTemp_degC, na.rm=TRUE),
            VolumeWeightedMeanTemp_degC=median(VolumeWeightedMeanTemp_degC, na.rm=TRUE),
            DeltaSurfaceDeep_degC=median(DeltaSurfaceDeep_degC, na.rm=TRUE),
            buoyancyfrequency_1_s2=median(buoyancyfrequency_1_s2, na.rm=TRUE),
            LengthOfIceCover_days=median(LengthOfIceCover_days, na.rm=TRUE))

#Is the number of inversely stratified days decreasing?
DailyInterpol_trim %>%
  filter(!water_year %in% c(1985, 1990, 2001, 2009, 2014, 2015, 2019, 2020)) %>%
  group_by(water_year,limit0.1) %>%
  summarize(n_days_inverse=n()) %>%
  filter(limit0.1=="Low0.1") %>%
  ggplot(aes(x=water_year, y=n_days_inverse))+
  geom_point()+
  geom_smooth()

#Is EpiTemp_degC changing?
DailyInterpol_trim_summary %>%
  ggplot(aes(x=water_year, y=EpiTemp_degC))+
  geom_point()

DailyInterpol_trim_summary %>%
  ggplot(aes(x=LengthOfIceCover_days, y=EpiTemp_degC))+
  geom_point()

#Is HypoTemp_degC changing?
DailyInterpol_trim_summary %>%
  ggplot(aes(x=water_year, y=HypoTemp_degC))+
  geom_point()+
  geom_smooth()

DailyInterpol_trim_summary %>%
  ggplot(aes(x=LengthOfIceCover_days, y=HypoTemp_degC))+
  geom_point()

#Is VolumeWeightedMeanTemp_degC changing?
DailyInterpol_trim_summary %>%
  ggplot(aes(x=water_year, y=VolumeWeightedMeanTemp_degC))+
  geom_point()+
  geom_smooth()

DailyInterpol_trim_summary %>%
  ggplot(aes(x=LengthOfIceCover_days, y=VolumeWeightedMeanTemp_degC))+
  geom_point()


#Is DeltaSurfaceDeep_degC changing?
DailyInterpol_trim_summary %>%
  ggplot(aes(x=water_year, y=DeltaSurfaceDeep_degC))+
  geom_point()+
  geom_smooth()

DailyInterpol_trim_summary %>%
  ggplot(aes(x=LengthOfIceCover_days, y=DeltaSurfaceDeep_degC))+
  geom_point()


#Is buoyancyfrequency_1_s2 changing?
DailyInterpol_trim_summary %>%
  ggplot(aes(x=water_year, y=buoyancyfrequency_1_s2))+
  geom_point()+
  geom_smooth()

DailyInterpol_trim_summary %>%
  ggplot(aes(x=LengthOfIceCover_days, y=buoyancyfrequency_1_s2))+
  geom_point()





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
  filter(!name %in% c("thermoclineDepth_m_thresh0.1","thermoclineDepth_m_maxdiff")) %>%
  ggplot(aes(x=year, y=value))+
  geom_point()+
  geom_smooth(method="lm", se=TRUE)+
  facet_wrap(~name, scales="free_y")+
  ggdark::dark_theme_bw(base_size=10)


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


#Can we recreate the stratification plot from the original Mohonk paper 
#but instead graph ice cover by DOY and look at avg. of ice over (1=yes, 0=no) over time?
test <- left_join(DailyInterpol %>%
                    mutate(water_year = dataRetrieval::calcWaterYear(Date)),
                  MohonkIce %>%
                    mutate(water_year = dataRetrieval::calcWaterYear(IceInDate)), by="water_year") %>%
  mutate(doy_fed=hydro.day(Date)) %>%
  group_by(water_year) %>%
  mutate(ice_cover_binomial = case_when(doy_fed >= IceInDayofYear_fed &
                                        doy_fed <= IceOutDayofYear_fed ~ 1,
                                        TRUE ~ 0))

test %>%
  group_by(doy_fed) %>%
  dplyr::summarize(median=median(ice_cover_binomial)) %>%
  ggplot(aes(x=doy_fed, y=median))+
  geom_point()

test %>%
  # filter(doy_fed==200) %>%
  ggplot(aes(x=doy_fed, y=ice_cover_binomial, color=water_year, group=water_year))+
  geom_jitter(alpha=0.1, width = 0.1, height = 0.1)+
  geom_smooth(se=F, alpha=0.1, linewidth=0.4)+
  scale_color_continuous(high = "#1A85FF", low = "#FFC20A") 

test2 <- test %>%
  group_by(doy_fed) %>%
  dplyr::summarize(median=median(ice_cover_binomial)) 

hist(test2$median)

# Composite thermocline depth and stability figure vs. day of year summarizing over all years
tmp.composite<-aggregate(test$stability_Jperm2,
                         by=list(test$doy_fed),
                         FUN=quantile,na.rm=T)

tmp2.composite<-aggregate(test$stability_Jperm2,
                          by = list(test$doy_fed),
                          FUN = function(x) quantile(x, probs = c(0.05,0.95),na.rm=T))

Stability.composite<-data.frame(doy_fed=tmp.composite$Group.1,
                                Min.stability_Jperm2=tmp.composite[["x"]][,1],
                                Fifth.stability_Jperm2=tmp2.composite[["x"]][,1],
                                TwentyFifth.stability_Jperm2=tmp.composite[["x"]][,2],
                                Median.stability_Jperm2=tmp.composite[["x"]][,3],
                                SeventyFifth.stability_Jperm2=tmp.composite[["x"]][,4],
                                NinetyFifth.stability_Jperm2=tmp2.composite[["x"]][,2],
                                Max.stability_Jperm2=tmp.composite[["x"]][,5])  

# min doy_fed for ice on and max doy_fed for ice off
min(test$IceInDayofYear_fed, na.rm=TRUE)
max(test$IceOutDayofYear_fed, na.rm=TRUE)
median(test$IceInDayofYear_fed, na.rm=TRUE)
median(test$IceOutDayofYear_fed, na.rm=TRUE)
lo<-60; hi<-202
testing <- Stability.composite %>%
  filter(doy_fed > 60 & doy_fed < 202) 
max(testing$NinetyFifth.stability_Jperm2,na.rm=T)
# gg.stability.composite<-
  ggplot()+
  geom_line(data=Stability.composite%>%
              slice(lo:hi),aes(y=Median.stability_Jperm2,x=doy_fed),col="white")+
  scale_y_continuous(limits=c(0,max(testing$NinetyFifth.stability_Jperm2,na.rm=T)),breaks=c(0,40,80))+
  # scale_x_continuous(limits=c(0,365))+
  geom_vline(xintercept=c(93,184),lty=2,col="darkgrey")+
  geom_ribbon(data=Stability.composite%>%slice(lo:hi),aes(x=doy_fed,ymin=Fifth.stability_Jperm2,ymax=NinetyFifth.stability_Jperm2),fill="lightskyblue1")+
  geom_ribbon(data=Stability.composite%>%slice(lo:hi),aes(x=doy_fed,ymin=TwentyFifth.stability_Jperm2,ymax=SeventyFifth.stability_Jperm2),fill="deepskyblue3")+
  geom_line(data=Stability.composite%>%slice(lo:hi),aes(x=doy_fed,y=SeventyFifth.stability_Jperm2),lwd=0.4,col="blue")+
  geom_line(data=Stability.composite%>%slice(lo:hi),aes(x=doy_fed,y=TwentyFifth.stability_Jperm2),lwd=0.4,col="blue")+
  geom_line(data=Stability.composite%>%slice(lo:hi),aes(x=doy_fed,y=Fifth.stability_Jperm2),lwd=0.4,col="blue")+
  geom_line(data=Stability.composite%>%slice(lo:hi),aes(x=doy_fed,y=NinetyFifth.stability_Jperm2),lwd=0.4,col="blue")+
  geom_line(data=Stability.composite%>%slice(lo:hi),aes(x=doy_fed,y=Median.stability_Jperm2),lwd=0.7,col="black")+
  ylab(expression(Schmidt~stability~(J~m^-2)~" "))+
  xlab(expression(Days~since~Oct~"1"))+
  theme_MS()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))+
  ggdark::dark_theme_bw(base_size=16)

  

# Weekly profiles over time -----------------------------------------------

ggplotly(MohonkWeeklyProfilesMetric %>%
    mutate(week=week(Date)) %>%
    pivot_longer(Temp_0m:Temp_12m) %>%
    left_join(MohonkIce, by=c("year"="Year")) %>%
    filter(dayofyear < 120) %>%
    filter(name %in% c("Temp_1m","Temp_12m")) %>%
    ggplot(aes(x=dayofyear, y=value, color=name, group=year))+
    geom_point()+
    # geom_vline(xintercept=MohonkIce$IceOutDayofYear, color="black", group=c("year"="Year"))+ #doesn't work how it should
    facet_wrap(~year))
  
  
# Is there a trend in deep water temperatures during the spring mixed period??
AnnualData %>%
  ggplot(aes(x=Year, y=DeepWaterTemp_SpringMixed_degC))+
  geom_point()+
  geom_smooth(method="lm")

lm <- lm(DeepWaterTemp_SpringMixed_degC~Year,AnnualData)
summary(lm)  
# Deep water temps are decreasing during mixed period

#What's the relationship between spring mixed period length and stratified period hypo temps?
library(plotly)
ggplotly(AnnualData %>%
  mutate(LengthSpringMixed_days=StartOfStratification_Day-IceOutDayofYear) %>%
  ggplot(
    aes(x=LengthSpringMixed_days, y=DeepWaterTemp_StratifiedPeriod_degC, label=Year)
    )+
  geom_point()+
  geom_smooth(method="lm")+
  geom_smooth(data=AnnualData %>%
                mutate(LengthSpringMixed_days=StartOfStratification_Day-IceOutDayofYear) %>%
                filter(LengthSpringMixed_days<100),
              aes(x=LengthSpringMixed_days, y=DeepWaterTemp_StratifiedPeriod_degC), color="orange",
              method="lm")+
  theme_bw(base_size=18))

#Whats the relationship between ice off date and the spring temps after ice off?
ggplotly(AnnualData %>%
           ggplot(
             aes(x=IceOutDayofYear, y=AirTemp_SpringPostIce_degC, label=Year)
           )+
           geom_point()+
           geom_smooth(method="lm")+
           theme_bw(base_size=18))
           

#What predicts summer hypo temps?
AnnualData2 <- AnnualData %>%
  mutate(LengthSpringMixed_days=StartOfStratification_Day-IceOutDayofYear) 
gam1 <- gam(DeepWaterTemp_StratifiedPeriod_degC ~ s(LengthSpringMixed_days, k=4) ,
                   # family=Gamma(link="log"),
                   data = AnnualData2,
                   # correlation = corCAR1(form = ~ Year),
                   method = "REML")
summary(gam1)
draw(gam1, residuals = TRUE)

gam2 <- gam(DeepWaterTemp_StratifiedPeriod_degC ~ s(IceOutDayofYear, k=4) +
              s(AirTemp_SpringPostIce_degC, k=4),
            # family=Gamma(link="log"),
            data = AnnualData2,
            # correlation = corCAR1(form = ~ Year),
            method = "REML")
summary(gam2)
draw(gam2, residuals = TRUE)
