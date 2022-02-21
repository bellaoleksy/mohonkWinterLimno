NAO_daily<-read.csv("data/NAO_index_daily.csv")

ENSO<-read.csv("data/ONI_index_3month.csv")

#Look at TS of NAO daily values
NAO_daily %>%
  ggplot(aes(x=Year, y=NAO_index))+
  geom_point()

# Seasonal NAO indices ----------------------------------------------------

NAO_seasonal <- NAO_daily %>%
  #Adds a category for season
  mutate(Season = 
           ifelse(Month %in% c(12, 1, 2), "winter",
                  ifelse(Month %in% c(3, 4, 5), "spring",
                         ifelse(Month %in% c(6, 7, 8), "summer",
                                ifelse(Month %in% c(9, 10, 11), "fall", "ERROR"))))) %>%
  #Calculate mean for each year x season combination
  group_by(Year, Season) %>%
  summarize_at(vars(NAO_index), mean, na.rm=TRUE)

#Visualize patterns
NAO_seasonal %>%
  ggplot(aes(x=Year,y=NAO_index))+
  geom_point()+
  facet_wrap(.~Season)

#Put the season DF in a format that can be easily merged with the big ol' Annual df
NAO_seasonal_wide <- pivot_wider(NAO_seasonal, names_from = Season, values_from = NAO_index) %>%
  rename(winter_NAO=winter,
         spring_NAO=spring,
         summer_NAO=summer,
         fall_NAO=fall)

# Annual NAO indices ----------------------------------------------------
NAO_annual <- NAO_daily %>%
  group_by(Year) %>%
  summarize_at(vars(NAO_index), mean, na.rm=TRUE)%>%
  rename(annual_NAO=NAO_index)

#Merge with seasonal NAO df
NAO_summary <- left_join(NAO_annual, NAO_seasonal_wide, by="Year")
