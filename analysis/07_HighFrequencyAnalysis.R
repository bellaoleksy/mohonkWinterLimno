#Script 07-HighFrequencyAnalysis.R####
#Explore the high frequency winter water temperatures and associated metrics for Mohonk Lake
#Created 18Jul2024, by David Richardson (DCR)


#Libraries####
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(zoo)) {install.packages("zoo")}

#Load libraries
library(tidyverse)
library(zoo) #for linearly interpolation

#Run functions####
source("script/01_functions.R")

#Read in data####
#Get rid of error codes####
SensorData<-read_csv("data/MohonkSensor-AllData-01Apr2016to06Aug2018-ModifiedHeaders.csv")%>%
              mutate(Temp_0m=ifelse(Temp_0m<(-100),NA,Temp_0m),
                     Temp_1m=ifelse(Temp_1m<(-100),NA,Temp_1m),
                     Temp_2m=ifelse(Temp_2m<(-100),NA,Temp_2m),
                     Temp_3m=ifelse(Temp_3m<(-100),NA,Temp_3m),
                     Temp_4m=ifelse(Temp_4m<(-100),NA,Temp_4m),
                     Temp_5m=ifelse(Temp_5m<(-100),NA,Temp_5m),
                     Temp_6m=ifelse(Temp_6m<(-100),NA,Temp_6m),
                     Temp_7m=ifelse(Temp_7m<(-100),NA,Temp_7m),
                     Temp_8m=ifelse(Temp_8m<(-100),NA,Temp_8m),
                     Temp_9m=ifelse(Temp_9m<(-100),NA,Temp_9m)
                     )
#Read in ice data####
IceOnIceOff<-read_csv("data/MohonkLake-IceOnIceOff-1932-2023.csv")%>%
  mutate(IceIn_1_date=mdy(ICEIN_1),
         IceOut_1_date=mdy(ICEOUT_1),
         IceIn_2_date=mdy(ICEOUT_2),
         IceOut_2_date=mdy(ICEOUT_1),
         IceIn_3_date=mdy(ICEIN_3),
         IceOut_3_date=mdy(ICEOUT_3))

#Read in ice data extraction####
IceDataExtraction<-read_csv("data/MohonkIceDataExtraction.csv")%>%
                    mutate(Date_format=mdy(Date))%>%
                    mutate(year=year(Date_format))


#Read in mesonet data####
#Identify all the individual .csv files####
Mesonet_files<-list.files("data/MesonetWeatherData",pattern = "*.csv")
#initialize empty list####
list_data<-list()

#Loop through all files and upload each months data####
for(fileIndex in 1:length(Mesonet_files)){
  list_data[[fileIndex]]<-read_csv(paste0("data/MesonetWeatherData/",Mesonet_files[fileIndex]))
}

#Bind all the months together####
MesonetData<-do.call(bind_rows, list_data)

#Rename some columns in mesonet data####
names(MesonetData)

#rename the column headers to clean up####
MesonetData<-MesonetData%>%
    setNames(gsub("\\^","",gsub("\\/","p",gsub("\\[|\\]","",sub(" ", "_", names(MesonetData))))))%>% #clean up the column headers, remove space, brackets and slashes
  mutate(DateTime=ymd_hms(time_end)) #Format a dateTIme variable

#READ Bathymetry####
MohonkBathy<-read.csv('data/Mohonk_area_bathymetry_csv2.csv', fill = TRUE)
##########################End of loading data####################

#plot ice % cover####
ggplot(data=IceDataExtraction%>%filter(year>=2016&year<=2018),aes(x=Date_format,y=IceCover_Percent))+geom_point()

#Check out individual plots####
ggplot(data=SensorData%>%pivot_longer(-1),aes(x=DateTime,y=value,color=name))+geom_line()+
    geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
    geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))

#Zoom in on 2016-2017 winter####
#*Set limits####
lims <- as.POSIXct(strptime(c("2016-12-10 19:00", "2017-04-20 20:00"), 
                            format = "%Y-%m-%d %H:%M"))
#Plot that winter####
ggplot(data=SensorData%>%pivot_longer(-1)%>%filter(DateTime>=as.POSIXct("2016-11-24 19:00:00 EST")&DateTime<=as.POSIXct("2017-04-20 20:00:00 EDT")),
       aes(x=DateTime,y=value,color=name))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  scale_x_datetime(limits=lims)+scale_y_continuous(limits=c(0,7))

#Graph the Percent ice cover
ggplot(data=IceDataExtraction,aes(x=as.POSIXct(Date_format),y=IceCover_Percent))+geom_point()+
  scale_x_datetime(limits=lims)

#Graph the windspeed or other met data here####
ggplot(data=MesonetData,aes(x=DateTime,y=temp_2m_max_degC))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  scale_x_datetime(limits=lims)

#Zoom in on 2017-2018 winter####
#*Set limits####
lims <- as.POSIXct(strptime(c("2017-12-12 19:00", "2018-04-24 20:00"), 
                            format = "%Y-%m-%d %H:%M"))
#Plot that winter####
ggplot(data=SensorData%>%pivot_longer(-1)%>%filter(DateTime>=as.POSIXct("2017-12-12 19:00:00 EST")&DateTime<=as.POSIXct("2018-04-24 20:00:00 EDT")),
       aes(x=DateTime,y=value,color=name))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  scale_x_datetime(limits=lims)

#Graph the Percent ice cover
ggplot(data=IceDataExtraction,aes(x=as.POSIXct(Date_format),y=IceCover_Percent))+geom_point()+
  scale_x_datetime(limits=lims)

##################################################
#Calculate metrics for the under ice data####
#This code takes a bit of time to run so I will comment it out#####

# #Create additional columns of data that indicate the stability and thermocline depth at that time
# #Vector of the depths
# depths.v <- seq(0, 9, 1)
# 
# #Calculate the thermocline depth and stability for all days####
# #*Filter the dataset for just winter#####
# SensorData_derived<-SensorData%>%
#                     filter(DateTime>=as.POSIXct("2016-11-20 00:00:00 EST"))%>%
#                     filter(DateTime<=as.POSIXct("2018-04-24 00:00:00 EST"))%>%
#                     filter(DateTime<=as.POSIXct("2017-04-24 00:00:00 EST")|DateTime>=as.POSIXct("2017-11-20 00:00:00 EST"))
# #*Check out the filtered data####
# #ggplot(data=SensorData_derived%>%pivot_longer(-1),aes(x=DateTime,y=value,color=name))+geom_line()+
#   #geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
#   #geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))
# 
# #Set up place holders for the new derived variables####
# SensorData_derived$thermoclineDepth_m_thresh0.1<-NA
# SensorData_derived$thermoclineDepth_m_maxdiff<-NA
# SensorData_derived$stability_Jperm2<-NA
# SensorData_derived$buoyancyfrequency_1_s2<-NA
# 
# #Code to run thermocline depth and stability calcs for each time point####
# for (dateTimeIndex in 1:nrow(SensorData_derived)) {
#   #Figure out if there is at least three temperature measurements in the profile
#   if (sum(!is.na(SensorData_derived[dateTimeIndex, 2:11])) < 3) {
#   } else{
#     #Call thermocline function
#     SensorData_derived$thermoclineDepth_m_thresh0.1[dateTimeIndex] <-
#       thermocline.Depth(depths.v, SensorData_derived[dateTimeIndex, 2:11], 0.1)
#     #Call thermocline function
#     SensorData_derived$thermoclineDepth_m_maxdiff[dateTimeIndex] <-
#       thermocline.Depth.max(depths.v, SensorData_derived[dateTimeIndex, 2:11])
#     #call stability function
#     SensorData_derived$stability_Jperm2[dateTimeIndex] <-
#       stability.calc(
#         as.numeric(SensorData_derived[dateTimeIndex, 2:11]),
#         depths.v,
#         MohonkBathy$SurfaceAreaAtThatDepth_m2,
#         MohonkBathy$Depth_m_LowerLimit
#       )
#     #call stability function
#     SensorData_derived$buoyancyfrequency_1_s2[dateTimeIndex] <-
#       buoyancy.freq.profile.max(SensorData_derived[dateTimeIndex, 2:11], depths.v)
#     
#   } #end of else
#   print(dateTimeIndex)
# } #End of for loop

#Calculate some other metrics here####
#*Top vs. bottom (Pierson et al. indicates inverse strat at top is 0.1C below that of bottom)
#SensorData_derived<-SensorData_derived%>%mutate(temperatureDifferenceTop0mvsBottom9m=Temp_0m-Temp_9m)
# write_csv(SensorData_derived,file="output/MohkWinterLimno_SensorData_derived.csv")

#################################################################################
#Read back in the file that was exported in above code####
SensorData_derived<-read_csv("output/MohkWinterLimno_SensorData_derived.csv")

############STOPPED HERE################

#Expand out each of the parts of the data frame to make sure we have all 15 minute time steps accounted for####
SensorData_derived2016<-SensorData_derived%>%filter(DateTime<=as.POSIXct("2017-07-01 00:00:00 EST"))
max(SensorData_derived2016$DateTime)
min(SensorData_derived2016$DateTime)
SensorData_derived2016<-left_join(tibble(DateTime=seq(min(SensorData_derived2016$DateTime),max(SensorData_derived2016$DateTime)+37*24*60*60,by="15 mins")),SensorData_derived2016,by="DateTime")
#Expand out the 2016 data set to go into April past the ice in to include other variables during that time####

SensorData_derived2017<-SensorData_derived%>%filter(DateTime>as.POSIXct("2017-07-01 00:00:00 EST"))
max(SensorData_derived2017$DateTime)
min(SensorData_derived2017$DateTime)
SensorData_derived2017<-left_join(tibble(DateTime=seq(min(SensorData_derived2017$DateTime),max(SensorData_derived2017$DateTime),by="15 mins")),SensorData_derived2017,by="DateTime")

#Bind them back together####
SensorData_derivedFill<-bind_rows(SensorData_derived2016,SensorData_derived2017)

#Merge with weather data - interpolate somehow#### 
  #Left join with sensorData_derived
  SensorData_derivedFill<-left_join(SensorData_derivedFill,MesonetData,by="DateTime")
  #linearly interpolate relevant variables from hourly to 15 minute data to match the sensors####
  SensorData_derivedFill<-SensorData_derivedFill%>%mutate(temp_2m_max_degC=na.approx(temp_2m_max_degC,na.rm=FALSE,maxgap=4),
                                        temp_2m_min_degC=na.approx(temp_2m_min_degC,na.rm=FALSE,maxgap=4),
                                        temp_2m_avg_degC=na.approx(temp_2m_avg_degC,na.rm=FALSE,maxgap=4),
                                        relative_humidity_max_percent=na.approx(relative_humidity_max_percent,na.rm=FALSE,maxgap=4),
                                        relative_humidity_min_percent=na.approx(relative_humidity_min_percent,na.rm=FALSE,maxgap=4),
                                        relative_humidity_avg_percent=na.approx(relative_humidity_avg_percent,na.rm=FALSE,maxgap=4),
                                        precip_incremental_mm=na.approx(precip_incremental_mm,na.rm=FALSE,maxgap=4),
                                        precip_local_mm=na.approx(precip_local_mm,na.rm=FALSE,maxgap=4),
                                        wind_speed_prop_avg_mps=na.approx(wind_speed_prop_avg_mps,na.rm=FALSE,maxgap=4),
                                        wind_speed_prop_max_mps=na.approx(wind_speed_prop_max_mps,na.rm=FALSE,maxgap=4),
                                        wind_direction_prop_avg_degrees=na.approx(wind_direction_prop_avg_degrees,na.rm=FALSE,maxgap=4),
                                        wind_speed_sonic_avg_mps=na.approx(wind_speed_sonic_avg_mps,na.rm=FALSE,maxgap=4),
                                        wind_speed_sonic_max_mps=na.approx(wind_speed_sonic_max_mps,na.rm=FALSE,maxgap=4),
                                        wind_direction_sonic_avg_degrees=na.approx(wind_direction_sonic_avg_degrees,na.rm=FALSE,maxgap=4),
                                        solar_insolation_avg_Wpm2=na.approx(solar_insolation_avg_Wpm2,na.rm=FALSE,maxgap=4),
                                        station_pressure_max_mbar=na.approx(station_pressure_max_mbar,na.rm=FALSE,maxgap=4),
                                        station_pressure_min_mbar=na.approx(station_pressure_min_mbar,na.rm=FALSE,maxgap=4),
                                        station_pressure_avg_mbar=na.approx(station_pressure_avg_mbar,na.rm=FALSE,maxgap=4),
                                        solar_insolation_total_MJpm2=na.approx(solar_insolation_total_MJpm2,na.rm=FALSE,maxgap=4)
                                        )

#Merge with percent ice cover#####
#*Create a dateTime variable that is noon for each day####
IceDataExtraction_sub<-IceDataExtraction%>%mutate(DateTime=mdy_hms(paste0(Date," ","12:00:00")))%>%dplyr::select(DateTime,IceCover_Percent,IceIn,IceOut)
#Merge with Sensor derived
SensorData_derived<-left_join(SensorData_derived,IceDataExtraction_sub,by="DateTime")

#Graph some data###
#*temperature data####
ggplot(data=SensorData_derived%>%dplyr::select(DateTime:Temp_9m)%>%pivot_longer(-1),aes(x=DateTime,y=value,color=name))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))
#*Stability data####
ggplot(data=SensorData_derived,aes(x=DateTime,y=stability_Jperm2))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  scale_y_continuous(limits=c(-0.5,10))
#*BuoyancyFreq. data####
ggplot(data=SensorData_derived,aes(x=DateTime,y=buoyancyfrequency_1_s2))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))
#*Thermocline based on max diff or threshold doesn't show much####
ggplot(data=SensorData_derived,aes(x=DateTime,y=thermoclineDepth_m_thresh0.1))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))
#*Top vs. bottom (Pierson et al. indicates inverse strat at top is 0.1C below that of bottom)####
ggplot(data=SensorData_derived,aes(x=DateTime,y=temperatureDifferenceTop0mvsBottom9m))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  geom_hline(yintercept=-0.1,color="red")

#*Graph % ice cover for those two winters####
ggplot(data=SensorData_derived,aes(x=DateTime,y=IceCover_Percent))+geom_point()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))

#*Graph some met data for those two winters####
ggplot(data=SensorData_derivedFill,aes(x=DateTime,y=temp_2m_min_degC))+geom_point()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))

