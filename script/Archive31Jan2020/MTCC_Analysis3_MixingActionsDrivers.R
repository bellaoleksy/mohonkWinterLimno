#Load libraries
library(lubridate)
library(tidyverse)




# Seasonal NAO indices ----------------------------------------------------

#Read in data
NAO_daily<-read.csv("data/NAO_index_daily.csv")


#Make date column
NAO_daily$Date <- paste(NAO_daily$Year, NAO_daily$Month, NAO_daily$Day, sep="-") %>%
  ymd()

#Convert date to DOY
NAO_daily<-NAO_daily %>%
  mutate(DOY=yday(Date))

#Summarize NAO data in the following ways
# * "spring" mean (21 March to 21 June, DOY 81 - 172)
# * mean of spring and summer (21 March to 21 Sept, DOY 81 - 265)
# * spring through MA period (DOY 81 through.... end of stratification DOY)
# * summer MA period (mean over start and end of stratification DOY)

NAO_springmean <- NAO_daily %>%
  select(Year, DOY, NAO_index)%>%
  filter(DOY>=80 & DOY <172) %>%
  group_by(Year)%>%
  summarize_at(vars(NAO_index), mean, na.rm=TRUE)%>%
  rename(NAO_springmean=NAO_index)
  
NAO_springsummermean <- NAO_daily %>%
  select(Year, DOY, NAO_index)%>%
  filter(DOY>=80 & DOY <265) %>%
  group_by(Year)%>%
  summarize_at(vars(NAO_index), mean, na.rm=TRUE)%>%
  rename(NAO_springsummermean=NAO_index)

# * TO DO: Add mean from start of spring through end of summer mixe --------


# * TO DO: Add mean over summer mixed action period -----------------------


#Merge with seasonal NAO df
NAO_summary <- left_join(NAO_springmean, NAO_springsummermean, by="Year")


# Seasonal ENSO indices ----------------------------------------------------

#Read in data
ENSO_monthly<-read.csv("data/ONI_index_monthly.csv")

#Summarizing ENSO data slightly different from NAO since we only have monthly times steps: 
# * spring mean (april + may + june)
# * summer mean (july + august + sept)
# * spring + summer mean (april - sept)

ENSO_seasonal <- ENSO_monthly %>%
  #Adds a category for season
  mutate(Season =
                  ifelse(MON %in% c(4, 5, 6), "spring",
                         ifelse(MON %in% c(7, 8, 9), "summer",
                                ifelse(MON %in% c(1, 2, 3, 10, 11, 12), "other", "ERROR")))) %>%
  filter(Season %in% c("spring","summer"))%>%
  rename(Year=YR)

ENSO_spring <- ENSO_seasonal %>%
  filter(Season=="spring")%>%
  group_by(Year)%>%
  summarize_at(vars(ANOM), mean, na.rm=TRUE)%>%
  rename(ANOM_spring=ANOM)

ENSO_summer <- ENSO_seasonal %>%
  filter(Season=="summer")%>%
  group_by(Year)%>%
  summarize_at(vars(ANOM), mean, na.rm=TRUE)%>%
  rename(ANOM_summer=ANOM)

ENSO_springsummer <- ENSO_seasonal %>%
  group_by(Year)%>%
  summarize_at(vars(ANOM), mean, na.rm=TRUE)%>%
  rename(ANOM_springsummer=ANOM)

ENSO_summary <- left_join(ENSO_spring,ENSO_summer, by="Year" )
ENSO_summary <- left_join(ENSO_summary,ENSO_springsummer, by="Year" )
