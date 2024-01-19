#Script 07-HighFrequencyAnalysis.R####
#Explore the high frequency winter water temperatures and associated metrics for Mohonk Lake
#Created 18Jul2024, by David Richardson (DCR)


#Libraries####
if (!require(tidyverse)) {
  install.packages("tidyverse")
}

#Load libraries
library(tidyverse)

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

MesonetData<-MesonetData%>%
    setNames(gsub("\\^","",gsub("\\/","p",gsub("\\[|\\]","",sub(" ", "_", names(MesonetData))))))%>% #clean up the column headers, remove space, brackets and slashes
  mutate(dateTime=ymd_hms(time_end)) #Format a dateTIme variable
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
ggplot(data=MesonetData,aes(x=dateTime,y=temp_2m_max_degC))+geom_line()+
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
