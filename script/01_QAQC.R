
#Read in Bathymetry Data####
# setwd(BathyDir2014)
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(lubridate)){install.packages("lubridate")}

library(tidyverse)
library(lubridate)



#READ Bathymetry####
MohonkBathy<-read.csv('data/Mohonk_area_bathymetry_csv2.csv', fill = TRUE)

#READ Temperature data 1980-2016#### 
MohonkWeeklyProfiles<-read.csv("data/2016_MohonkLake_tblSecchi-Thermistor.csv", fill = TRUE)
#2020-11-30 IAO: Added in new data
MohonkWeeklyProfiles_2018_19<-read.csv("data/2018_2019_Mohonklake_Secchi_Thermistor.csv", fill = TRUE)

#Bind the dataframes together
MohonkWeeklyProfiles<-bind_rows(MohonkWeeklyProfiles,MohonkWeeklyProfiles_2018_19)

#Clean up some error values
MohonkWeeklyProfiles$METER3[MohonkWeeklyProfiles$METER3==783&!is.na(MohonkWeeklyProfiles$METER3)]<-78.3
MohonkWeeklyProfiles$METER7[2013]<-NA
MohonkWeeklyProfiles$METER9[2014]<-NA
MohonkWeeklyProfiles$METER10[394]<-NA
MohonkWeeklyProfiles$METER12[1613]<-42.4
#Replace any bottom tempeatures below freezing - there were multiple entries that appear to be depth
#Replace any entries >100 (there are some error codes of 9999)
MohonkWeeklyProfiles$BOTTOM[MohonkWeeklyProfiles$BOTTOM<32&!is.na(MohonkWeeklyProfiles$BOTTOM)]<-NA
MohonkWeeklyProfiles$BOTTOM[MohonkWeeklyProfiles$BOTTOM>75&!is.na(MohonkWeeklyProfiles$BOTTOM)]<-NA

#Remove an anonmylous 2015, 18Mar value that is 3x the temperature 1 m up####
MohonkWeeklyProfiles$BOTTOM[as.character(MohonkWeeklyProfiles$DATEREAD)=="2015-03-18"]<-NA

#Replace any 0 values with NA
MohonkWeeklyProfiles[, 5:18][MohonkWeeklyProfiles[, 5:18] == 0] <- NA

#Create new data frame with QA/QC data in metric
MohonkWeeklyProfilesMetric<-as.data.frame(matrix(nrow=length(MohonkWeeklyProfiles$DATEREAD),
                                                 ncol=length(names(MohonkWeeklyProfiles))))

names(MohonkWeeklyProfilesMetric)<-c("Date","Collector","DepthToBottom_m",
                                     "Comment","Temp_0m","Temp_1m","Temp_2m",
                                     "Temp_3m","Temp_4m","Temp_5m","Temp_6m",
                                     "Temp_7m","Temp_8m","Temp_9m","Temp_10m",
                                     "Temp_11m","Temp_12m","Temp_Bottom",
                                     "Secchi_m")


MohonkWeeklyProfilesMetric$Date<-as.Date(as.character(MohonkWeeklyProfiles$DATEREAD))

MohonkWeeklyProfilesMetric$Collector<-MohonkWeeklyProfiles$COLLECTOR

MohonkWeeklyProfilesMetric$Comment<-MohonkWeeklyProfiles$COMMENT

#Depth has two different metrics, 0 gets NA,
#>14 is probably feet and needs conversion to m, <14 is meters
MohonkWeeklyProfilesMetric$DepthToBottom_m<-MohonkWeeklyProfiles$DEPTH
MohonkWeeklyProfilesMetric$DepthToBottom_m[MohonkWeeklyProfiles$DEPTH==0]<-NA
MohonkWeeklyProfilesMetric$DepthToBottom_m[MohonkWeeklyProfiles$DEPTH>30&!is.na(MohonkWeeklyProfiles$DEPTH)]<-MohonkWeeklyProfiles$DEPTH[MohonkWeeklyProfiles$DEPTH>30&!is.na(MohonkWeeklyProfiles$DEPTH)]*0.3048

#Clean up any remaining >15 numbers, make them NA
MohonkWeeklyProfilesMetric$DepthToBottom_m[MohonkWeeklyProfilesMetric$DepthToBottom_m>15&!is.na(MohonkWeeklyProfilesMetric$DepthToBottom_m)]<-NA

#Convert to temperature in C
MohonkWeeklyProfilesMetric[,seq(5,18)]<-(MohonkWeeklyProfiles[,seq(5,18)]-32)*(5/9)

#Finds any values with a really small lowest number and replaces with NA
#I think these values, the depth and last temp reading were juxtaposed
find.errors2<-MohonkWeeklyProfilesMetric[(!is.na(MohonkWeeklyProfilesMetric$Temp_Bottom)&MohonkWeeklyProfilesMetric$Temp_Bottom<0),
                                         c("Date","Temp_Bottom")]

MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% find.errors2$DATEREAD,"BOTTOM"]<-NA

#1985-09-20  The temperature for meter10 has really high values
#(~10C more than the week before or after and above and below). Setting to NA!
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("1985-09-20"),"Temp_10m"]<-NA

#Get rid of anonmylous values on 1985-10-08 as all the values are 10C
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("1985-10-08"),5:18]<-NA

#The temperature at Meter8 and Meter9 is high, none of the other values are close.  Setting to NA!
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("1986-05-30"),"Temp_8m"]<-NA
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("1986-05-30"),"Temp_9m"]<-NA

#The row for 1986-08-13 needs to be NAs
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("1986-08-13"),5:18]<-NA

#The row for 1994-07-21 needs to be NAs
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("1994-07-21"),5:18]<-NA

#The row for 2010-06-08 needs to be NAs
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("2010-06-08"),5:18]<-NA

#The depth8.0 variable for 24Jul1992 is anomalous - 10C higher than values around it and should be set to NA
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("1992-07-24"),"Temp_8m"]<-NA

#The depth11.0 variable for 01Oct1993 is anomalous - 10C higher than values around it and should be set to NA
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("1993-10-01"),"Temp_11m"]<-NA

#The depth11.0 variable for 08Jun1998 is anomalous both in the thermistor string but also for the surrounding Dates and should be set to NA
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("1998-06-08"),"Temp_11m"]<-NA

#The depth4.0 variable for 28Feb2008 is below 0 and should be set to NA
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("2008-02-28"),"Temp_4m"]<-NA

#The depth11.0 variable for 20Mar2009 is below 0 and should be set to NA
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("2009-03-20"),"Temp_11m"]<-NA

#The depth12.0 variable for 07Jul2012 is 10C warmer than preceeding and following values and should be set to NA
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("2011-07-07"),"Temp_12m"]<-NA

#The comments say thermister giving false readings, set entire row for 2012-02-23 needs to be NAs
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("2012-02-23"),5:18]<-NA

#The surface reading for all three values seems more like the bottom value - and is really inconsistent with the below values and surrounding.  Setting all three of these surface readings to NA.
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("2012-05-7"),"Temp_0m"]<-NA
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("2012-05-11"),"Temp_0m"]<-NA
MohonkWeeklyProfilesMetric[MohonkWeeklyProfilesMetric$Date %in% as.Date("2012-05-17"),"Temp_0m"]<-NA

#Loop through all the temperature readings for quality control histograms
for(j in 5:18){
  hist(MohonkWeeklyProfilesMetric[,j],main=names(MohonkWeeklyProfilesMetric)[j])
}  

#Convert to m from feet
MohonkWeeklyProfilesMetric$Secchi_m<-MohonkWeeklyProfiles$SECCHI*0.3048  

#Any secchi with 0 gets converted to NA
MohonkWeeklyProfilesMetric$Secchi_m[MohonkWeeklyProfilesMetric$Secchi_m==0]<-NA

#Graph histogram of Secchi
hist(MohonkWeeklyProfilesMetric$Secchi_m)

##Upload 2017 data#### 
MohonkWeeklyProfiles2017<-read.csv("data/2017_MohonkLake_Secchi_Thermistor.csv", fill = TRUE)  
MohonkWeeklyProfiles2017Metric<-MohonkWeeklyProfiles2017

#Convert to temperature in C
MohonkWeeklyProfiles2017Metric[,seq(5,18)]<-(MohonkWeeklyProfiles2017Metric[,seq(5,18)]-32)*(5/9)

#Rename column headers
names(MohonkWeeklyProfiles2017Metric)<-c("Date","Collector","DepthToBottom_m",
                                         "Comment","Temp_0m","Temp_1m","Temp_2m",
                                         "Temp_3m","Temp_4m","Temp_5m","Temp_6m",
                                         "Temp_7m","Temp_8m","Temp_9m","Temp_10m",
                                         "Temp_11m","Temp_12m","Temp_Bottom",
                                         "Secchi_m")

#Convert date variable to date
MohonkWeeklyProfiles2017Metric$Date<-as.Date(as.character(MohonkWeeklyProfiles2017Metric$Date))
str(MohonkWeeklyProfiles2017Metric)

#Convert to m from feet
MohonkWeeklyProfiles2017Metric$Secchi_m<-MohonkWeeklyProfiles2017Metric$Secchi_m*0.3048  

#Any secchi with 0 gets converted to NA
MohonkWeeklyProfiles2017Metric$Secchi_m[MohonkWeeklyProfiles2017Metric$Secchi_m==0]<-NA

#Graph histogram of Secchi
hist(MohonkWeeklyProfiles2017Metric$Secchi_m)

#Loop through all the temperature readings for quality control histograms
for(j in 5:18){
  #hist(MohonkWeeklyProfiles2017Metric[,j],main=names(MohonkWeeklyProfiles2017Metric)[j])
}  

#Add an extra column called "BOTTOM" that is NA to match with <2016 data
MohonkWeeklyProfiles2017Metric$BOTTOM<-NA

#*Create a separate file for manual 2017 collection####
MohonkWeeklyProfiles2017metric.manual<-MohonkWeeklyProfiles2017Metric

##Upload sensor data####    
MohonkWeeklyProfilesSensor<-read.csv("data/MohonkSensor-AllData-01Apr2016to06Aug2018-ModifiedHeaders.csv", fill = TRUE)  

#Fix date/time format
MohonkWeeklyProfilesSensor$DateTime<-as.POSIXct(as.character(MohonkWeeklyProfilesSensor$DateTime))

#Remove the -100000 error code
#Loop through all the temperature readings for quality control histograms
for(j in 2:11){
  MohonkWeeklyProfilesSensor[MohonkWeeklyProfilesSensor[,j]<(-20),j]<-NA
  #hist(MohonkWeeklyProfilesSensor[,j],main=names(MohonkWeeklyProfilesSensor)[j])
}

#Remove all rows of data where Temp_9m>13C
MohonkWeeklyProfilesSensor[MohonkWeeklyProfilesSensor$Temp_9m>13|is.na(MohonkWeeklyProfilesSensor$Temp_9m),2:11]<-NA

#Downsample to daily values
#Create a date column
MohonkWeeklyProfilesSensor$Date<-as.Date(MohonkWeeklyProfilesSensor$DateTime)

#aggregate by date for each column
MohonkWeeklyProfilesSensorDaily<-aggregate(MohonkWeeklyProfilesSensor,by=list(MohonkWeeklyProfilesSensor$Date),FUN=mean,na.rm=T)

#Add some dummy columns to match up with the other data frames
MohonkWeeklyProfilesSensorDaily$Collector<-NA
MohonkWeeklyProfilesSensorDaily$DepthToBottom_m<-NA
MohonkWeeklyProfilesSensorDaily$Comment<-NA
MohonkWeeklyProfilesSensorDaily$Temp_10m<-NA
MohonkWeeklyProfilesSensorDaily$Temp_11m<-NA
MohonkWeeklyProfilesSensorDaily$Temp_12m<-NA
MohonkWeeklyProfilesSensorDaily$Temp_Bottom<-NA
MohonkWeeklyProfilesSensorDaily$Secchi_m<-NA
MohonkWeeklyProfilesSensorDaily$BOTTOM<-NA

#Output the correct order
MohonkWeeklyProfilesSensorDaily.cols<-MohonkWeeklyProfilesSensorDaily[,names(MohonkWeeklyProfiles2017Metric)]

#Merge 2017 MP data with 2017 sensor daily data with MP data superceding the sensor data
#Second is the sensor data frame for all dates except those found in MohonkWeeklyProfiles2017Metric$Date and year = 2017
MohonkWeeklyProfiles2017Metric<-rbind(MohonkWeeklyProfiles2017Metric,
                                      MohonkWeeklyProfilesSensorDaily.cols[!(MohonkWeeklyProfilesSensorDaily.cols$Date %in% MohonkWeeklyProfiles2017Metric$Date)&as.numeric(format(MohonkWeeklyProfilesSensorDaily.cols$Date,"%Y"))==2017,])


#Create new data frames for the sensor data
MohonkWeeklyProfiles2017metric.sensor<-MohonkWeeklyProfilesSensor%>%filter(year(Date)=="2017")
MohonkWeeklyProfiles2017metric.sensor.daily<-MohonkWeeklyProfilesSensorDaily.cols%>%filter(year(Date)=="2017")

#Order by date
MohonkWeeklyProfiles2017Metric<-MohonkWeeklyProfiles2017Metric[order(MohonkWeeklyProfiles2017Metric$Date),]

# Seasonal NAO indices ----------------------------------------------------
#2022-03-16 New Data from: https://psl.noaa.gov/data/timeseries/daily/
#Data only available through 2020 for the time being
#Read in data
NAO_daily<-read.csv("data/NAO_index_daily_1948-2020.csv")

#Make date column
NAO_daily$Date <- paste(NAO_daily$Year, NAO_daily$Month, NAO_daily$Day, sep="-") %>%
  ymd()

#Convert date to DOY
NAO_daily<-NAO_daily %>%
  mutate(DOY=yday(Date))

# Seasonal ENSO - MEI indices ----------------------------------------------------
#2020-12-10 IAO: Uploading new ENSO data 
####Source: ENSO (MEI v2) - 1979 to present, monthly
####https://psl.noaa.gov/data/climateindices/
#Read in data
# ENSO_MEI_monthly<-read.csv("data/ENSO_MEI_1979-2020.csv")
ENSO_monthly<-read.csv("data/ONI_index_monthly_1950-2022.csv")


## Local weather ####   
#2020-12-01 IAO downloaded wx data going back to 1930
#2022-07-08 IAO added 1930-2021 data in metric units
MohonkDailyWeatherFull.upload<-read.csv("data/MohonkPreserveWeatherData-1896-2022-NOAA-NCEI-metric.csv", fill = TRUE) 
str(MohonkDailyWeatherFull.upload)
MohonkDailyWeatherFull.upload <- MohonkDailyWeatherFull.upload %>%
  mutate(Date = ymd(Date)) %>%
  rename(Precip_mm = Precipitation_mm, 
         Snow_mm = Snowfall_mm,#check units-- is this m or mm? 
         SnowDepth_mm = SnowDepth_mm,
         TempMean_degC = TempAvg_degC) 

#Keep relevant columns for weather data frame
MohonkDailyWeatherFull<- MohonkDailyWeatherFull.upload %>%
  dplyr::select(c("Date","Precip_mm","Snow_mm",
                  "SnowDepth_mm","TempMax_degC",
                  "TempMin_degC","TempMean_degC"))
str(MohonkDailyWeatherFull.upload)

# Old from before we had metric data -- can be deleted at some point
# MohonkDailyWeatherFull.upload$Date<-as.Date(as.character(MohonkDailyWeatherFull.upload$DATE))
# MohonkDailyWeatherFull.upload$Precip_mm<-MohonkDailyWeatherFull.upload$PRCP*25.4 #convert to mm
# MohonkDailyWeatherFull.upload$Snow_mm<-MohonkDailyWeatherFull.upload$SNOW*25.4 #convert to m
# MohonkDailyWeatherFull.upload$SnowDepth_mm<-MohonkDailyWeatherFull.upload$SNWD*25.4 #convert to mm
# MohonkDailyWeatherFull.upload$TempMax_degC<-(MohonkDailyWeatherFull.upload$TMAX-32)*5/9 #convert to C
# MohonkDailyWeatherFull.upload$TempMin_degC<-(MohonkDailyWeatherFull.upload$TMIN-32)*5/9 #convert to C
# MohonkDailyWeatherFull.upload$TempMean_degC<-(MohonkDailyWeatherFull.upload$TempMax_degC+MohonkDailyWeatherFull.upload$TempMin_degC)/2
# 
# 
# MohonkDailyWeatherFull<-MohonkDailyWeatherFull.upload[,c("Date","Precip_mm","Snow_mm",
#                                                  "SnowDepth_mm","TempMax_degC",
#                                                  "TempMin_degC","TempMean_degC")]

# Seasonal ENSO indices ----------------------------------------------------

#Read in data
# ENSO_monthly<-read.csv("data/ONI_index_monthly.csv")

##Upload Mohonk NOAA National Weather Service Temp and Precip daily data####    
MohonkDailyWeather.upload<-read.csv("data/MohonkPreserveWeatherData-1985-2017-NOAA-NCEI.csv", fill = TRUE) 
MohonkDailyWeather.upload$Date<-as.Date(as.character(MohonkDailyWeather.upload$DATE))
MohonkDailyWeather.upload$Precip_mm<-MohonkDailyWeather.upload$PRCP_in*25.4
MohonkDailyWeather.upload$Snow_mm<-MohonkDailyWeather.upload$SNOW_in*25.4
MohonkDailyWeather.upload$SnowDepth_mm<-MohonkDailyWeather.upload$SNWD_in*25.4
MohonkDailyWeather.upload$TempMax_degC<-(MohonkDailyWeather.upload$TMAX_degF-32)*5/9
MohonkDailyWeather.upload$TempMin_degC<-(MohonkDailyWeather.upload$TMIN_degF-32)*5/9
MohonkDailyWeather.upload$TempMean_degC<-(MohonkDailyWeather.upload$TempMax_degC+MohonkDailyWeather.upload$TempMin_degC)/2

#Keep relevant columns for weather data frame
MohonkDailyWeather<-MohonkDailyWeather.upload[,c("Date","Precip_mm","Snow_mm",
                                                 "SnowDepth_mm","TempMax_degC",
                                                 "TempMin_degC","TempMean_degC")]



##Upload Mohonk Ice on and Ice off data####
MohonkIce.upload<-read.csv("data/MohonkLake-IceOnIceOff-1932-2022.csv", fill = TRUE) 
MohonkIce.upload$Year<-seq(1932,2022,by=1) #Corrected the script to include 2019


#Create new column of ice in date. Replace "No Date" with NA, format the others to dates
MohonkIce.upload$IceInDate<-as.character(MohonkIce.upload$ICEIN)
MohonkIce.upload$IceInDate[MohonkIce.upload$IceInDate=="No date"]<-NA
# MohonkIce.upload$IceInDate<-as.Date(MohonkIce.upload$IceInDate)
MohonkIce.upload <- MohonkIce.upload %>%
  mutate(IceInDate=mdy(IceInDate))

#Create new column of ice in date. Replace "No Date
MohonkIce.upload$IceOutDate<-as.character(MohonkIce.upload$ICEOUT)
MohonkIce.upload$IceOutDate[MohonkIce.upload$IceOutDate=="No date"]<-NA
MohonkIce.upload <- MohonkIce.upload %>%
  mutate(IceOutDate=mdy(IceOutDate))
# MohonkIce.upload$IceOutDate<-as.Date(MohonkIce.upload$IceOutDate)

#Create new data frame with only relevant columns
MohonkIce<-MohonkIce.upload[,c("Year","IceInDate","IceOutDate")]
MohonkIce$IceInDayofYear<-as.POSIXlt(MohonkIce$IceInDate, format = "%d%b%y")$yday+1
MohonkIce$IceOutDayofYear<-as.POSIXlt(MohonkIce$IceOutDate, format = "%d%b%y")$yday+1
MohonkIce$LengthOfIceCover_days<-as.numeric(MohonkIce$IceOutDate-MohonkIce$IceInDate)
MohonkIcePost1985<-MohonkIce[MohonkIce$Year>=1984,]

str(MohonkIce)

####Merge different data frames together####
MohonkWeeklyProfilesMetric<-rbind(MohonkWeeklyProfilesMetric,MohonkWeeklyProfiles2017Metric)
str(MohonkWeeklyProfilesMetric)
#Create a year variable and day of the year
MohonkWeeklyProfilesMetric$year<-as.numeric(format(MohonkWeeklyProfilesMetric$Date,"%Y"))
MohonkWeeklyProfilesMetric$dayofyear<-as.POSIXlt(MohonkWeeklyProfilesMetric$Date, format = "%d%b%y")$yday+1

#Order by date
MohonkWeeklyProfilesMetric<-MohonkWeeklyProfilesMetric[order(MohonkWeeklyProfilesMetric$Date),]

###Create Secchi Data frame####
MohonkWeeklySecchi<-MohonkWeeklyProfilesMetric[,c("Date","Secchi_m")]
#*Generate Secchi for output from 1985 to 2019####
# MohonkWeeklySecchi_export<-MohonkWeeklySecchi%>%filter(Date>"1984-12-31")%>%drop_na(Secchi_m)

###Generate file for export to EDI####
#Only 1985 to present
#Only temperature columnms
#Remove all rows with NA across the temp columns
# MohonkWeeklyProfilesMetric_export<-MohonkWeeklyProfilesMetric%>%
#                                     mutate(Temp_13m=Temp_Bottom)%>%
#                                     filter(year>=1985)%>%
#                                     mutate(sumNA=is.na(Temp_0m)+
#                                              is.na(Temp_1m)+
#                                              is.na(Temp_2m)+
#                                              is.na(Temp_3m)+
#                                              is.na(Temp_4m)+
#                                              is.na(Temp_5m)+
#                                              is.na(Temp_6m)+
#                                              is.na(Temp_7m)+
#                                              is.na(Temp_8m)+
#                                              is.na(Temp_9m)+
#                                              is.na(Temp_10m)+
#                                              is.na(Temp_11m)+
#                                              is.na(Temp_12m)+
#                                              is.na(Temp_13m))%>%
#                                     filter(sumNA<14)%>%
#                                     dplyr::select(Date,Temp_0m:Temp_12m,Temp_13m)
#Export temperature profiles  
#write_csv(MohonkWeeklyProfilesMetric_export,"data/MohonkLake_TemperatureProfiles_1985to2019.csv")
#Write secchi data
# write_csv(MohonkWeeklySecchi_export,"figures/MohonkLake_SecchiDepth_1985to2019.csv")





#Remove unneccessary data frames
rm(find.errors2)
rm(MohonkDailyWeatherFull.upload) #Can remove beause MohonkDailyWeatherFull.upload includes data back to 1930s
rm(MohonkDailyWeather.upload) #Can remove beause MohonkDailyWeatherFull.upload includes data back to 1930s
rm(MohonkIce.upload)
rm(MohonkWeeklyProfilesSensor)
rm(MohonkWeeklyProfiles)
rm(MohonkWeeklyProfiles2017)
rm(MohonkWeeklyProfiles2017Metric)
rm(MohonkWeeklyProfilesSensorDaily)
rm(MohonkWeeklyProfilesSensorDaily.cols)
# rm(MohonkIcePost1985)

rm(MohonkWeeklyProfiles_2018_19,
   MohonkWeeklyProfiles2017metric.sensor,
   MohonkWeeklyProfiles2017metric.manual,
   MohonkWeeklyProfiles2017metric.sensor.daily)

