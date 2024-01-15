#Run previous code to get in data####
source('analysis/00_main.R')


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
# Define as winter season (December 21st to March 19th)
DailyInterpol_test <- DailyInterpol %>%
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
  # filter(season=="winter") %>%
  # filter(Month=="1") %>%
  group_by(water_year, Month) %>%
  summarise(across(stability_Jperm2:DeltaSurfaceDeep_degC, ~ mean(.x, na.rm = TRUE)))
  # summarize(EpiTemp_degC = mean(EpiTemp_degC, na.rm=TRUE),
  #           HypoTemp_degC = mean(HypoTemp_degC, na.rm=TRUE),
  #           VolumeWeightedMeanTemp_degC = mean(VolumeWeightedMeanTemp_degC, na.rm=TRUE),
  #           )

#How does it look?
DailyInterpol_test %>%
  pivot_longer(-c(water_year, Month)) %>%
  filter(!name=="weekofyear") %>%
  filter(Month %in% c(12,1,2,3)) %>%
  ggplot(aes(x=water_year, y=value, color=name))+
  geom_point()+
  geom_smooth(method="lm",se=FALSE)+
  facet_wrap(name~Month, scales="free")+
  theme(legend.position="none")
