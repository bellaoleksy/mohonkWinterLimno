##Mohonk Lake Climate change analysis
##Created 09Nov2017 David Richardson (hereafter DCR) 
##Updated 11Sep2018
##Updated 09Apr2019 - include spring analysis
##Updated 26Jul2019> separates out functions and loading data into separate scripts
##Updated 10Jan2020> includes additional data merging from IAO and DCR meeting on 10Jan2020 at CIES
##                  Anything in here requires modification of data.frame name
##Analyze trends in epi and hypo temperature, stability, phenology of stratification

#Libraries
if(!require(mblm)){install.packages("mblm")}
if(!require(rLakeAnalyzer)){install.packages("rLakeAnalyzer")}
if(!require(zoo)){install.packages("zoo")}
if(!require(trend)){install.packages("trend")}
if(!require(zyp)){install.packages("zyp")}
if(!require(Kendall)){install.packages("Kendall")}
if(!require(tidyverse)){install.packages("tidyverse")}

library(mblm)
library(rLakeAnalyzer)
library(zoo)
library(trend)
library(zyp)
library(Kendall)
library(tidyverse)
library(lubridate)



#Create additional columns of data that indicate the stability and thermocline depth at that time
#Vector of the depths
depths.v<-seq(0,13,1)

#Calculate the thermocline depth and stability for all days####
MohonkWeeklyProfilesMetric.derivedData<-MohonkWeeklyProfilesMetric
MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_thresh0.1<-NA
MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_maxdiff<-NA
MohonkWeeklyProfilesMetric.derivedData$stability_Jperm2<-NA
MohonkWeeklyProfilesMetric.derivedData$buoyancyfrequency_1_s2<-NA

#Code to run thermocline depth and stability calcs for each day
for(iii in 1:length(MohonkWeeklyProfilesMetric.derivedData$Date)){
  #Figure out if there is at least three temperature measurements in the profile
  if(sum(!is.na(MohonkWeeklyProfilesMetric.derivedData[iii,5:18]))<3){}else{
    #Call thermocline function
    MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_thresh0.1[iii]<-thermocline.Depth(depths.v,MohonkWeeklyProfilesMetric.derivedData[iii,5:18],0.1)
    #Call thermocline function
    MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_maxdiff[iii]<-thermocline.Depth.max(depths.v,MohonkWeeklyProfilesMetric.derivedData[iii,5:18])
    #call stability function
    MohonkWeeklyProfilesMetric.derivedData$stability_Jperm2[iii]<-stability.calc(as.numeric(MohonkWeeklyProfilesMetric.derivedData[iii,5:18]),depths.v,MohonkBathy$SurfaceAreaAtThatDepth_m2,MohonkBathy$Depth_m_LowerLimit)
    #call stability function
    MohonkWeeklyProfilesMetric.derivedData$buoyancyfrequency_1_s2[iii]<-buoyancy.freq.profile.max(MohonkWeeklyProfilesMetric.derivedData[iii,5:18],depths.v)
    
  }
  #End of for loop
}

####Debug Stability values####
##Debug stability - find weird negative values - check data

#Plot all thermocline depth and stability
# plot(MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_thresh0.1~MohonkWeeklyProfilesMetric.derivedData$Date,type='l')
# plot(MohonkWeeklyProfilesMetric.derivedData$stability_Jperm2~MohonkWeeklyProfilesMetric.derivedData$Date,type='l')
# plot(MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_maxdiff~MohonkWeeklyProfilesMetric.derivedData$Date,type='l')
# plot(MohonkWeeklyProfilesMetric.derivedData$buoyancyfrequency_1_s2~MohonkWeeklyProfilesMetric.derivedData$Date,type='l')
# Plot just one year to see the thermocline difference
# plot(MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_maxdiff[MohonkWeeklyProfilesMetric.derivedData$year==2017]~MohonkWeeklyProfilesMetric.derivedData$Date[MohonkWeeklyProfilesMetric.derivedData$year==2017],col="blue",ylab="Thermocline D (m)",ylim=c(13,0))
# lines(MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_thresh0.1[MohonkWeeklyProfilesMetric.derivedData$year==2017]~MohonkWeeklyProfilesMetric.derivedData$Date[MohonkWeeklyProfilesMetric.derivedData$year==2017],col="red")
# abline(lm(MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_thresh0.1[MohonkWeeklyProfilesMetric.derivedData$year==2017]~MohonkWeeklyProfilesMetric.derivedData$Date[MohonkWeeklyProfilesMetric.derivedData$year==2017]),col="green")
# summary(lm(MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_thresh0.1[MohonkWeeklyProfilesMetric.derivedData$year==2017]~MohonkWeeklyProfilesMetric.derivedData$Date[MohonkWeeklyProfilesMetric.derivedData$year==2017]))

#Compare the two thermocline depths
#plot(MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_maxdiff~MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_thresh0.1)
#cor.test(MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_maxdiff,MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_thresh0.1)

#Compare the buoyancy freq and stability
#plot(MohonkWeeklyProfilesMetric.derivedData$stability_Jperm2~MohonkWeeklyProfilesMetric.derivedData$buoyancyfrequency_1_s2,xlab="BF_1/s2",ylab="Stability")
#cor.test(MohonkWeeklyProfilesMetric.derivedData$stability_Jperm2,MohonkWeeklyProfilesMetric.derivedData$buoyancyfrequency_1_s2)


#Daily Interpolation of temperature data####
#Remove 1984 and prior
MohonkWeeklyProfilesMetric.derivedData<-MohonkWeeklyProfilesMetric.derivedData[MohonkWeeklyProfilesMetric.derivedData$year>1984,]

# Create new data frame based on complete years ---------------------------
MohonkWeeklyProfilesMetric.derivedData.fullYears<-MohonkWeeklyProfilesMetric.derivedData      

#Remove years with missing data (1997,2014)
  #MohonkWeeklyProfilesMetric.derivedData.fullYears<-MohonkWeeklyProfilesMetric.derivedData.fullYears[MohonkWeeklyProfilesMetric.derivedData.fullYears$year!=1997&MohonkWeeklyProfilesMetric.derivedData.fullYears$year!=2014,]

#Generate vector of years available         
YearAvail<-as.numeric(levels(factor(MohonkWeeklyProfilesMetric.derivedData.fullYears$year)))

#Generate names of columns of interest to interpolate to the daily level
VarsOfInterest<-names(MohonkWeeklyProfilesMetric.derivedData.fullYears)[c(1,5:18,21:26)]

#Generate dataframe for interpolated data
DailyInterpol<-data.frame(matrix(nrow=0,ncol=length(VarsOfInterest)))
names(DailyInterpol)<-VarsOfInterest

#For loop to step through each year
for(jj in 1:length(YearAvail)){
  #Generate a vector of all the days in the year
  temp<-MohonkWeeklyProfilesMetric.derivedData.fullYears[MohonkWeeklyProfilesMetric.derivedData.fullYears$year==YearAvail[jj],]
  #Generate data frame of all the days in the year
  AllDays<-data.frame(Date=seq(as.Date(paste(YearAvail[jj],"-01-01",sep="")),as.Date(paste(YearAvail[jj],"-12-31",sep="")),by="+1 day"))
  #Create empty columns of interest
  AllDays[,VarsOfInterest[-1]]<-NA
  
  #Populate over the values that exist from the temp data frame into the AllDays
  #Use VarsOfInterest[-1] which removes the Date column
  AllDays[which(as.character(AllDays$Date) %in% as.character(temp$Date)),VarsOfInterest[-1]]<-temp[,VarsOfInterest[-1]]
  
  #Interpolate all the columns through Zoo package
  zoo.temp<-data.frame(Date=AllDays$Date,na.approx(AllDays[,VarsOfInterest[-1]],x=AllDays$Date,xout=AllDays$Date,na.rm=F))
  #Replace day of year with uninterpolated value
  zoo.temp$dayofyear<-as.POSIXlt(zoo.temp$Date, format = "%d%b%y")$yday+1
  #Continually build daily interpolated data with each year
  DailyInterpol<-rbind(DailyInterpol,zoo.temp)
  
#End of the loop to interpolate  
}

#Redo year because interpolation has some as NA
DailyInterpol$year<-as.numeric(format(DailyInterpol$Date,"%Y"))
#Interpolate the end and beginning of each year
#For loop to step through each year
for(kk in 1:(length(YearAvail)-1)){
  #Generate a vector of all the days in the year
  temp0<-DailyInterpol[DailyInterpol$year==YearAvail[kk],]
  temp1<-DailyInterpol[DailyInterpol$year==YearAvail[kk+1],]
  #check if year n and year n+1 are 1 year apart, if not, then do nothing
  if((YearAvail[kk]+1)-YearAvail[kk]!=1){}else{
    #find the last value before an NA and the first value after an NA
    NonNAindexY0 <- which(!is.na(temp0$Temp_1m))
    lastNonNA <- max(NonNAindexY0)
    
    NonNAindexY1 <- which(!is.na(temp1$Temp_1m))
    firstNonNA <- min(NonNAindexY1)
    #Generate a data frame from the last non NA value of one year to the first of the next
    tempCross<-rbind(temp0[lastNonNA:length(temp0$Date),],temp1[1:firstNonNA,])
    
    #Interpolate all the columns through Zoo package
    zoo.tempYearEnd<-data.frame(Date=tempCross$Date,na.approx(tempCross[,VarsOfInterest[-1]],x=tempCross$Date,xout=tempCross$Date,na.rm=F))
    #Replace day of year with uninterpolated value
    zoo.tempYearEnd$dayofyear<-as.POSIXlt(zoo.tempYearEnd$Date, format = "%d%b%y")$yday+1
    #store the results in DailyInterpol
    DailyInterpol[DailyInterpol$Date %in% zoo.tempYearEnd$Date,]<-zoo.tempYearEnd      
  } #End of else loop for consecutive years
  
  #End of for loop through the years
}

# #Calculate the average from the surface (1-3m) and hypolimnion ( --------
#Calculate the average from the surface (1-3m) and hypolimnion (10-12m) for that peak day
DailyInterpol$EpiTemp_degC<-rowMeans(DailyInterpol[,c("Temp_1m","Temp_2m","Temp_3m")])
DailyInterpol$HypoTemp_degC<-rowMeans(DailyInterpol[,c("Temp_10m","Temp_11m","Temp_12m")])

#Create a data frame that gets the proportion of volume at each depth
  cx <- c(0,cumsum(MohonkBathy$SurfaceAreaAtThatDepth_m2))
#Upper depth
MohonkBathy.volume<-data.frame(UpperDepth_m=MohonkBathy$Depth_m_LowerLimit[seq(1,(length(MohonkBathy$Depth_m_LowerLimit)-1))],LowerDepth_m=MohonkBathy$Depth_m_LowerLimit[seq(2,length(MohonkBathy$Depth_m_LowerLimit))])
MohonkBathy.volume$MeanArea_m2<-(cx[(2+1):length(cx)] - cx[1:(length(cx) - 2)]) / 2
MohonkBathy.volume$Volume_m3<-MohonkBathy.volume$MeanArea_m2*1
MohonkBathy.volume$Volume_proportion<-MohonkBathy.volume$Volume_m3/sum(MohonkBathy.volume$Volume_m3)

#Calculate the volume weighted mean temperature
#The last reading at the bottom will be extended down to 18m
for(index.l in 1:length(DailyInterpol$Date)){
  #Check the profile. If there is missing data below 9m, then make extend the lowest value
  tmp.profile<-DailyInterpol[index.l,c("Temp_0m","Temp_1m","Temp_2m","Temp_3m","Temp_4m","Temp_5m","Temp_6m","Temp_7m","Temp_8m","Temp_9m","Temp_10m","Temp_11m","Temp_12m","Temp_Bottom")]
  
  #If there are less than 8 temperature readings, average is NA; otherwise, calculate
  if(sum(!is.na(tmp.profile))<8){DailyInterpol$VolumeWeightedMeanTemp_degC[index.l]<-NA}else{
    #Find the last non-NA entry, extrapolate that down to 13m  
    tmp.profile[(tail(which(!is.na(tmp.profile)),1)+1):length(tmp.profile)]<-tmp.profile[tail(which(!is.na(tmp.profile)),1)]
    
    #Take the averages between depths 
    cx <- c(0, cumsum(ifelse(is.na(DailyInterpol[index.l,c("Temp_0m","Temp_1m","Temp_2m","Temp_3m","Temp_4m","Temp_5m","Temp_6m","Temp_7m","Temp_8m","Temp_9m","Temp_10m","Temp_11m","Temp_12m","Temp_Bottom")]), 0, DailyInterpol[index.l,c("Temp_0m","Temp_1m","Temp_2m","Temp_3m","Temp_4m","Temp_5m","Temp_6m","Temp_7m","Temp_8m","Temp_9m","Temp_10m","Temp_11m","Temp_12m","Temp_Bottom")])))
    cn <- c(0, cumsum(ifelse(is.na(DailyInterpol[index.l,c("Temp_0m","Temp_1m","Temp_2m","Temp_3m","Temp_4m","Temp_5m","Temp_6m","Temp_7m","Temp_8m","Temp_9m","Temp_10m","Temp_11m","Temp_12m","Temp_Bottom")]), 0, 1)))
    rx <- cx[(2+1):length(cx)] - cx[1:(length(cx) - 2)]
    rn <- cn[(2+1):length(cx)] - cn[1:(length(cx) - 2)]
    rsum <- rx / rn
    #Calculate the weighted volume weighted average temperature for each row by multiplying the interpolated volumes from bathymetry by the interpolated temperatures
    DailyInterpol$VolumeWeightedMeanTemp_degC[index.l]<-sum(MohonkBathy.volume$Volume_proportion*c(rsum,rep(rsum[length(rsum)],length(MohonkBathy.volume$Volume_proportion)-length(rsum))))  
  } #End of for loop
}

#Create a week of the year variable 
#Based on this technique, there will always be 1 (non-leap year, 365 days)
#Or 2 (leap year, 366 days) in the 53rd week.  Just lump those values with 52nd week    
DailyInterpol$weekofyear<-(DailyInterpol$dayofyear-1)%/%7 +1
DailyInterpol$weekofyear[DailyInterpol$weekofyear==53]<-52
#max(DailyInterpol$weekofyear)

####ANNUAL DATA Summmary####         
#Create a vector of all the years in the thermister data frame
#Create a blank vector for the maximum stability
#Annual data data frame
AnnualData<-as.data.frame(matrix(ncol=1,nrow=length(as.numeric(levels(factor(DailyInterpol$year))))))
names(AnnualData)<-c("Year")
#Redo year because interpolation missed some and created NAs
AnnualData$Year<-as.numeric(levels(factor(DailyInterpol$year)))


#Merge AnnualData with IceOff and ice length data
AnnualData<-left_join(AnnualData,MohonkIcePost1985%>%select(Year,IceOutDayofYear,LengthOfIceCover_days),by=c("Year"))

#Stability cutoff for defining stratified period - in units of J/m2
#*Caluclate stability cutoff#### 
  #**Method 1 to determine cutoff - get the median of each dayOfYear, pettitt.test on the median composite stability#### 
  medianCompositeStability<-DailyInterpol%>%
    select(dayofyear,stability_Jperm2)%>%
    group_by(dayofyear)%>%
    summarize(MedianStability_Jperm2=median(stability_Jperm2,na.rm=TRUE))
  #Breakpoint analysis
  Stability.cutoff1<-medianCompositeStability$MedianStability_Jperm2[pettitt.test(medianCompositeStability$MedianStability_Jperm2)$estimate]
  #plot(medianCompositeStability$MedianStability_Jperm2~medianCompositeStability$dayofyear)
  #abline(h=Stability.cutoff1)
  
  #**Methood 2 to determine cutoff - get the cutoff each year and take the median of those####
  AnnualBreakpoint.method2<-DailyInterpol%>%
    mutate(Year=year(Date))%>%
    select(Year, dayofyear, stability_Jperm2)%>%
    filter(!is.na(stability_Jperm2))%>%
    group_by(Year)%>%
    summarize(AnnualBreakpoint=stability_Jperm2[pettitt.test(stability_Jperm2)$estimate[1]])
  #hist(temp$AnnualBreakpoint)
  Stability.cutoff2<-median(AnnualBreakpoint.method2$AnnualBreakpoint)

  #**set the stability cutoff equal to the second method - but there was consistency between the two
  Stability.cutoff<-Stability.cutoff2

#**Initialize some columns in the AnnualData DF####
  AnnualData$MaxStability_Jperm2<-NA
  AnnualData$DayMaxStability<-NA
  AnnualData$AUC.Stability_Jperm2day<-NA
  AnnualData$StartOfStratification_Day<-NA
  AnnualData$EndOfStratification_Day<-NA
  AnnualData$LengthOfStrat_days<-NA
  AnnualData$ThermoclineSlope_mperd<-NA
  #Day 172 to 264
  AnnualData$SurfaceWaterTemp_Summer_degC<-NA
  AnnualData$DeepWaterTemp_Summer_degC<-NA
  #Calculate the average from start to end of stratification for each year
  #Use StartOfStratification_Day and EndOfStratification_Day
  AnnualData$SurfaceWaterTemp_StratifiedPeriod_degC<-NA
  AnnualData$DeepWaterTemp_StratifiedPeriod_degC<-NA
  #Calculate the average for spring (21Mar to 21Jun) for each year
  #Day 80 to <172 (to avoid overlap with summer)
  AnnualData$SurfaceWaterTemp_Spring_degC<-NA
  AnnualData$DeepWaterTemp_Spring_degC<-NA
  #Calculate the average for spring mixed period for each year
  #Use ice off (IceOutDayofYear) to StartOfStratification_Day
  AnnualData$SurfaceWaterTemp_SpringMixed_degC<-NA
  AnnualData$DeepWaterTemp_SpringMixed_degC<-NA
  #**Calculate the average for spring mixed post ice period for each year####
  #Use ice off (IceOutDayofYear) to +7 days
  AnnualData$SurfaceWaterTemp_SpringPostIce_degC<-NA
  AnnualData$DeepWaterTemp_SpringPostIce_degC<-NA
  
  
#*Get the area under the curve for the stability FOR each year####
for(k in 1:length(AnnualData$Year)){
  year.tmp<-AnnualData$Year[k]
  #Pull out relevant data for that year
  dayofyear.tmp<-DailyInterpol$dayofyear[DailyInterpol$year==year.tmp]
  stability.tmp<-DailyInterpol$stability_Jperm2[DailyInterpol$year==year.tmp]
  thermocline.tmp<-DailyInterpol$thermoclineDepth_m_thresh0.1[DailyInterpol$year==year.tmp]
  top123temp.tmp<-DailyInterpol[DailyInterpol$year==year.tmp,c("Temp_1m","Temp_2m","Temp_3m")]
  bottom101112.tmp<-DailyInterpol[DailyInterpol$year==year.tmp,c("Temp_10m","Temp_11m","Temp_12m")]
  
  #plot(stability.tmp~dayofyear.tmp,main=paste("Year=",year.tmp,sep=""),xlim=c(0,365),ylim=c(0,610))
  #Blue line is 10
  #abline(h=Stability.cutoff,col="blue")
  #REd lines are start and end of stratification
  #abline(v=dayofyear.tmp[min(which(stability.tmp>Stability.cutoff))],col="red")
  #abline(v=dayofyear.tmp[max(which(stability.tmp>Stability.cutoff))],col="red")
  
  #Exclude 1997 because of the paucity of data
  if(year.tmp==1997){}else{
    AnnualData$MaxStability_Jperm2[k]<-max(stability.tmp,na.rm=TRUE)
    AnnualData$DayMaxStability[k]<-dayofyear.tmp[which.max(stability.tmp)]
    }
  
  #**Calculations of the total stratified stability####
  #Area under the curve for first time above cutoff from above to first time below
  #Exclude 1984,1997,2014 because of incomplete curves
  if(year.tmp==1984|year.tmp==1997|year.tmp==2014){}else{
    #Pull out the day which is the first >cutoff reading, pull datasets for day of year, stability, and thermocline depth 
    dayofyear.tmp2<-dayofyear.tmp[min(which(stability.tmp>Stability.cutoff)):max(which(stability.tmp>Stability.cutoff))]
    stability.tmp2<-stability.tmp[min(which(stability.tmp>Stability.cutoff)):max(which(stability.tmp>Stability.cutoff))]
    thermocline.tmp2<-thermocline.tmp[min(which(stability.tmp>Stability.cutoff)):max(which(stability.tmp>Stability.cutoff))]
    #Calculate the area under the curve for those days
    AnnualData$AUC.Stability_Jperm2day[k]<-sum(diff(dayofyear.tmp2[!is.na(stability.tmp2)]) * (head(stability.tmp2[!is.na(stability.tmp2)],-1)+tail(stability.tmp2[!is.na(stability.tmp2)],-1)))/2
    AnnualData$StartOfStratification_Day[k]<-dayofyear.tmp[min(which(stability.tmp>Stability.cutoff))]
    AnnualData$EndOfStratification_Day[k]<-dayofyear.tmp[max(which(stability.tmp>Stability.cutoff))]
    AnnualData$LengthOfStrat_days[k]<-AnnualData$EndOfStratification_Day[k]-AnnualData$StartOfStratification_Day[k]
    
    #Calculate the slope of the thermocline
    lm<-lm(thermocline.tmp2~dayofyear.tmp2)
    s.slope<-sens.slope(thermocline.tmp2[!is.na(thermocline.tmp2)])
    AnnualData$ThermoclineSlope_mperd[k]<-s.slope$estimates
    #plot(thermocline.tmp2~dayofyear.tmp2)
    
    #abline(lm(thermocline.tmp2~dayofyear.tmp2))

    #mtext(paste("SenSlope=",round(s.slope$estimates,4),"LM slope=",round(lm$coefficients[2],4)),side=3,line=-1)
  }
  
  #**Calculate the average of June 21 to Sep 21 for each year####
  #Day 172 to 264
  AnnualData$SurfaceWaterTemp_Summer_degC[k]<-mean(as.matrix(top123temp.tmp[dayofyear.tmp>=172&dayofyear.tmp<=264,]),na.rm=T)
  AnnualData$DeepWaterTemp_Summer_degC[k]<-mean(as.matrix(bottom101112.tmp[dayofyear.tmp>=172&dayofyear.tmp<=264,]),na.rm=T)
  #**Calculate the average from start to end of stratification for each year####
  #Use StartOfStratification_Day and EndOfStratification_Day
  AnnualData$SurfaceWaterTemp_StratifiedPeriod_degC[k]<-mean(as.matrix(top123temp.tmp[dayofyear.tmp>=AnnualData$StartOfStratification_Day[k]&dayofyear.tmp<=AnnualData$EndOfStratification_Day[k],]),na.rm=T)
  AnnualData$DeepWaterTemp_StratifiedPeriod_degC[k]<-mean(as.matrix(bottom101112.tmp[dayofyear.tmp>=AnnualData$StartOfStratification_Day[k]&dayofyear.tmp<=AnnualData$EndOfStratification_Day[k],]),na.rm=T)
  #**Calculate the average for spring (21Mar to 21Jun) for each year####
  #Day 80 to <172 (to avoid overlap with summer)
  AnnualData$SurfaceWaterTemp_Spring_degC[k]<-mean(as.matrix(top123temp.tmp[dayofyear.tmp>=80&dayofyear.tmp<172,]),na.rm=T)
  AnnualData$DeepWaterTemp_Spring_degC[k]<-mean(as.matrix(bottom101112.tmp[dayofyear.tmp>=80&dayofyear.tmp<172,]),na.rm=T)
  
  #**Calculate the average for spring mixed period for each year####
  #Use ice off (IceOutDayofYear) to StartOfStratification_Day
  AnnualData$SurfaceWaterTemp_SpringMixed_degC[k]<-mean(as.matrix(top123temp.tmp[dayofyear.tmp>=AnnualData$IceOutDayofYear[k]&dayofyear.tmp<AnnualData$StartOfStratification_Day[k],]),na.rm=T)
  AnnualData$DeepWaterTemp_SpringMixed_degC[k]<-mean(as.matrix(bottom101112.tmp[dayofyear.tmp>=AnnualData$IceOutDayofYear[k]&dayofyear.tmp<AnnualData$StartOfStratification_Day[k],]),na.rm=T)
  
  #**Calculate the average for spring mixed post ice period for each year####
  #Use ice off (IceOutDayofYear) to +7 days
  AnnualData$SurfaceWaterTemp_SpringPostIce_degC[k]<-mean(as.matrix(top123temp.tmp[dayofyear.tmp>AnnualData$IceOutDayofYear[k]&dayofyear.tmp<=AnnualData$IceOutDayofYear[k]+7,]),na.rm=T)
  AnnualData$DeepWaterTemp_SpringPostIce_degC[k]<-mean(as.matrix(bottom101112.tmp[dayofyear.tmp>AnnualData$IceOutDayofYear[k]&dayofyear.tmp<=AnnualData$IceOutDayofYear[k]+7,]),na.rm=T)
  
  
  }
#End of for loop through the years

#Some looking at the selection of the different spring periods
#There are three: spring, post ice to spring mix, and ice off+7 days
# 
# temp<-AnnualData%>%select(Year,IceOutDayofYear,StartOfStratification_Day)%>%mutate(Difference=StartOfStratification_Day-IceOutDayofYear)
# par(mfrow=c(3,1))
# par(mar = c(0,0,0,0))
# par(oma = c(5,5,2,2))
# plot(AnnualData$SurfaceWaterTemp_Spring_degC~AnnualData$Year,ylab="Sp",main="Surface")
# mtext(side=2,text="Spring",line=2.2)
# plot(AnnualData$SurfaceWaterTemp_SpringMixed_degC~AnnualData$Year,ylab="Mix")
# mtext(side=2,text="Mixed",line=2.2)
# plot(AnnualData$SurfaceWaterTemp_SpringPostIce_degC~AnnualData$Year,ylab="Ice")
# mtext(side=2,text="WeekPostIce",line=2.2)
# 
# par(mfrow=c(3,1))
# par(mar = c(0,0,0,0))
# par(oma = c(5,5,2,2))
# plot(AnnualData$DeepWaterTemp_Spring_degC~AnnualData$Year,ylab="Sp",main="Deep")
# mtext(side=2,text="Spring",line=2.2)
# plot(AnnualData$DeepWaterTemp_SpringMixed_degC~AnnualData$Year,ylab="Mix")
# mtext(side=2,text="Mixed",line=2.2)
# plot(AnnualData$DeepWaterTemp_SpringPostIce_degC~AnnualData$Year,ylab="Ice")
# mtext(side=2,text="WeekPostIce",line=2.2)
# 
# par(mfrow=c(3,1))
# par(mar = c(0,0,0,0))
# par(oma = c(5,5,2,2))
# plot(AnnualData$SurfaceWaterTemp_Spring_degC-AnnualData$DeepWaterTemp_Spring_degC~AnnualData$Year,ylab="Sp",main="Difference")
# mtext(side=2,text="Spring",line=2.2)
# plot(AnnualData$SurfaceWaterTemp_SpringMixed_degC-AnnualData$DeepWaterTemp_SpringMixed_degC~AnnualData$Year,ylab="Mix")
# mtext(side=2,text="Mixed",line=2.2)
# plot(AnnualData$SurfaceWaterTemp_SpringPostIce_degC-AnnualData$DeepWaterTemp_SpringPostIce_degC~AnnualData$Year,ylab="Ice")
# mtext(side=2,text="WeekPostIce",line=2.2)

#Determine the day of peak stratification
AnnualData$DateMaxStability<-as.Date(AnnualData$DayMaxStability, origin=as.Date(paste(AnnualData$Year,"-01-01",sep="")))

#**Calculate the average from the surface (1-3m) and hypolimnion (10-12m) for that peak stratification day#####
temp.Annualmeans<-as.data.frame(matrix(ncol=1,nrow=sum(!is.na(AnnualData$DateMaxStability))))
names(temp.Annualmeans)<-c("Year")
temp.Annualmeans$Year<-AnnualData$Year[!is.na(AnnualData$DateMaxStability)]
temp.Annualmeans$SurfaceWaterTemp_PeakStratification_degC<-rowMeans(DailyInterpol[DailyInterpol$Date %in% AnnualData$DateMaxStability,c("Temp_1m","Temp_2m","Temp_3m")])
temp.Annualmeans$DeepWaterTemp_PeakStratification_degC<-rowMeans(DailyInterpol[DailyInterpol$Date %in% AnnualData$DateMaxStability,c("Temp_10m","Temp_11m","Temp_12m")])

#Merge with annual data
AnnualData<-merge(AnnualData,temp.Annualmeans,by="Year",all.x=T)  
  #remove the temporary data frame for peak stratification
  rm(temp.Annualmeans)
#Convert the area under the curve to a Mixing action with units of gigaJoules*day
AnnualData$MixingAction_gigaJday<-AnnualData$AUC.Stability_Jperm2day*69000/(10^9)

#*Summarize NAO data in the following ways####
# * "spring" mean (21 March to 21 June, DOY 81 - 172)
# * mean of spring and summer (21 March to 21 Sept, DOY 81 - 265)
# * spring through MA period (DOY 81 through.... end of stratification DOY)
# * summer MA period (mean over start and end of stratification DOY)

#**NAO spring mean -----------------------
NAO_springmean <- NAO_daily %>%
  filter(Year>=1985)%>%
  select(Year, DOY, NAO_index)%>%
  filter(DOY>=80 & DOY <172) %>%
  group_by(Year)%>%
  summarize_at(vars(NAO_index), mean, na.rm=TRUE)%>%
  rename(NAO_Spring=NAO_index)

#**NAO Summer mean -----------------------
NAO_summermean <- NAO_daily %>%
  filter(Year>=1985)%>%
  select(Year, DOY, NAO_index)%>%
  filter(DOY>=172 & DOY <265) %>%
  group_by(Year)%>%
  summarize_at(vars(NAO_index), mean, na.rm=TRUE)%>%
  rename(NAO_Summer=NAO_index)

#**NAO stratified period --------
NAO_stratifiedperiod <- NAO_daily %>%
  left_join(.,AnnualData%>%
              select(Year,StartOfStratification_Day,
                     EndOfStratification_Day,IceOutDayofYear),
            by="Year") %>%
  mutate(StratifiedPeriod=ifelse(DOY>=StartOfStratification_Day
                                 &
                                DOY<EndOfStratification_Day,NAO_index,NA)) %>%
  group_by(Year) %>%
  summarize(NAO_StratifiedPeriod=mean(StratifiedPeriod, na.rm=TRUE))

#**NAO SpringMixed -----------------------
NAO_springmixed <- NAO_daily%>%
  filter(Year>=1985)%>%
  left_join(.,AnnualData%>%
              select(Year,StartOfStratification_Day,
                     EndOfStratification_Day,IceOutDayofYear),
            by="Year") %>%
  mutate(SpringMixedPeriod=ifelse(DOY>IceOutDayofYear
                                  &
                                  DOY<=StartOfStratification_Day,
                                  NAO_index,NA))%>%
  group_by(Year) %>%
  summarize(NAO_SpringMixed=mean(SpringMixedPeriod, na.rm=TRUE))

  
#***Merge all NAO indices  -----------------------
NAO_summary <- left_join(NAO_springmean, NAO_summermean, by="Year")
NAO_summary <- left_join(NAO_summary, NAO_stratifiedperiod, by="Year")
NAO_summary <- left_join(NAO_summary, NAO_springmixed, by="Year")


#**Seasonal ENSO indices ----------------------------------------------------

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
  rename(ENSO_Spring=ANOM)

ENSO_summer <- ENSO_seasonal %>%
  filter(Season=="summer")%>%
  group_by(Year)%>%
  summarize_at(vars(ANOM), mean, na.rm=TRUE)%>%
  rename(ENSO_Summer=ANOM)


ENSO_summary <- left_join(ENSO_spring,ENSO_summer, by="Year" )


#***Merge with AnnualData by Year####
AnnualData<-left_join(AnnualData,NAO_summary,by=c("Year"))
AnnualData<-left_join(AnnualData,ENSO_summary,by=c("Year"))

#Daily Interpolate Secchi####
#There are several repeated days - some are NA for both duplicates, other have two secchi readings
#Remove those duplicated values by averaging the secchi values
MohonkWeeklySecchi<-MohonkWeeklySecchi%>%group_by(Date)%>%summarise(Secchi_m=mean(Secchi_m,na.rm=TRUE))

#Create data frame with dates from daily interpolate dataframe
DailyInterpol.secchi<-data.frame(Date=DailyInterpol$Date)

#Merge dates with secchi weekly data frame 
DailyInterpol.secchi<-left_join(DailyInterpol.secchi,MohonkWeeklySecchi,by="Date")

#Linearly interpolate across all years (this is slightly different than the temperature interpolation)
#This ignores gaps and keeps in all years
DailyInterpol.secchi<-data.frame(Date=DailyInterpol.secchi$Date,
                                 na.approx(DailyInterpol.secchi$Secchi_m,
                                           x=DailyInterpol.secchi$Date,
                                           xout=DailyInterpol.secchi$Date,
                                           na.rm=FALSE))
#Rename the Secchi column
names(DailyInterpol.secchi)[2]<-"Secchi_m"

#Add in a year column
DailyInterpol.secchi<-DailyInterpol.secchi%>%
  mutate(Year=year(Date))

#*Secchi to annual summary####
  #**Add in columns for all the important dates#### 
    #Create day of year
    #Get the start spring start of summer and end of summer - those are set for each year
    #merge with AnnualData to get IceOff, start and end of stratification in there for each year
    #Set NA or value columns based on the 4 periods of interest - leave off spring post ice because there will likely be 0 or 1 measurements in there?
  DailyInterpol.secchiPeriods<-DailyInterpol.secchi%>%
      mutate(dayOfYear=yday(Date))%>% 
      mutate(StartOfSummer=172,StartOfSpring=80,EndOfSummer=265)%>% 
      left_join(.,AnnualData%>%select(Year,StartOfStratification_Day,EndOfStratification_Day,IceOutDayofYear),by="Year")%>%
      mutate(SummerPeriod=ifelse(dayOfYear>=StartOfSummer&dayOfYear<EndOfSummer,Secchi_m,NA))%>%
      mutate(StratifiedPeriod=ifelse(dayOfYear>=StartOfStratification_Day&dayOfYear<EndOfStratification_Day,Secchi_m,NA))%>%
      mutate(SpringPeriod=ifelse(dayOfYear>StartOfSpring&dayOfYear<StartOfSummer,Secchi_m,NA))%>%
      mutate(SpringMixedPeriod=ifelse(dayOfYear>IceOutDayofYear&dayOfYear<=StartOfStratification_Day,Secchi_m,NA))
  #**Summarize the 4 different secchi periods by year####
  AnnualData_SecchiPeriods<-DailyInterpol.secchiPeriods%>%group_by(Year)%>%summarize(SecchiDepth_Summer_m=mean(SummerPeriod,na.rm=TRUE),SecchiDepth_StratifiedPeriod_m=mean(StratifiedPeriod,na.rm=TRUE),SecchiDepth_Spring_m=mean(SpringPeriod,na.rm=TRUE),SecchiDepth_SpringMixed_m=mean(SpringMixedPeriod,na.rm=TRUE))
  #**Merge with AnnualData
  AnnualData<-left_join(AnnualData,AnnualData_SecchiPeriods,by="Year")

#*VolumeWeightedMeans to annual summary####
  #**Add in columns for all the important dates#### 
  #Create day of year
  #Get the start spring start of summer and end of summer - those are set for each year
  #merge with AnnualData to get IceOff, start and end of stratification in there for each year
  #Set NA or value columns based on the 5 periods of interest - include spring post ice 
  DailyInterpol.volWtMeansPeriods<-DailyInterpol%>%
    mutate(Year=year(Date),dayOfYear=yday(Date))%>% 
    select(Year,dayOfYear,VolumeWeightedMeanTemp_degC)%>%
    mutate(StartOfSummer=172,StartOfSpring=80,EndOfSummer=265)%>% 
    left_join(.,AnnualData%>%select(Year,StartOfStratification_Day,EndOfStratification_Day,IceOutDayofYear),by="Year")%>%
    mutate(SummerPeriod=ifelse(dayOfYear>=StartOfSummer&dayOfYear<EndOfSummer,VolumeWeightedMeanTemp_degC,NA))%>%
    mutate(StratifiedPeriod=ifelse(dayOfYear>=StartOfStratification_Day&dayOfYear<EndOfStratification_Day,VolumeWeightedMeanTemp_degC,NA))%>%
    mutate(SpringPeriod=ifelse(dayOfYear>StartOfSpring&dayOfYear<StartOfSummer,VolumeWeightedMeanTemp_degC,NA))%>%
    mutate(SpringMixedPeriod=ifelse(dayOfYear>IceOutDayofYear&dayOfYear<=StartOfStratification_Day,VolumeWeightedMeanTemp_degC,NA))%>%
    mutate(PostIcePeriod=ifelse(dayOfYear>IceOutDayofYear&dayOfYear<=(IceOutDayofYear+7),VolumeWeightedMeanTemp_degC,NA))
  #**Summarize the 5 different volume weighted mean water temp periods by year####
  AnnualData_volWtMeansPeriods<-DailyInterpol.volWtMeansPeriods%>%group_by(Year)%>%summarize(VolumeWeightedMeanTemp_Summer_degC=mean(SummerPeriod,na.rm=TRUE),VolumeWeightedMeanTemp_StratifiedPeriod_degC=mean(StratifiedPeriod,na.rm=TRUE),VolumeWeightedMeanTemp_Spring_degC=mean(SpringPeriod,na.rm=TRUE),VolumeWeightedMeanTemp_SpringMixed_degC=mean(SpringMixedPeriod,na.rm=TRUE),VolumeWeightedMeanTemp_SpringPostIce_degC=mean(PostIcePeriod,na.rm=TRUE))
  #**Merge with AnnualData
  AnnualData<-left_join(AnnualData,AnnualData_volWtMeansPeriods,by="Year")
  
#*Air temperature to annual summary####
  #**Truncate to the correct data size (1985-2017)
  MohonkDailyWeatherTruncate<-MohonkDailyWeather%>%filter(Date>=min(DailyInterpol$Date))
  #**Add in columns for all the important dates#### 
  #Create day of year
  #Get the start spring start of summer and end of summer - those are set for each year
  #merge with AnnualData to get IceOff, start and end of stratification in there for each year
  #Set NA or value columns based on the 5 periods of interest - include spring post ice 
  DailyInterpol.airTemp<-MohonkDailyWeatherTruncate%>%
    mutate(Year=year(Date),
           dayOfYear=yday(Date))%>% 
    select(Year,dayOfYear,TempMean_degC)%>%
    mutate(StartOfSummer=172,StartOfSpring=80,EndOfSummer=265)%>% 
    left_join(.,AnnualData%>%
                select(Year,StartOfStratification_Day,
                       EndOfStratification_Day,IceOutDayofYear),
              by="Year")%>%
    mutate(SummerPeriod=ifelse(dayOfYear>=StartOfSummer&dayOfYear<EndOfSummer,TempMean_degC,NA))%>%
    mutate(StratifiedPeriod=ifelse(dayOfYear>=StartOfStratification_Day&dayOfYear<EndOfStratification_Day,TempMean_degC,NA))%>%
    mutate(SpringPeriod=ifelse(dayOfYear>StartOfSpring&dayOfYear<StartOfSummer,TempMean_degC,NA))%>%
    mutate(SpringMixedPeriod=ifelse(dayOfYear>IceOutDayofYear&dayOfYear<=StartOfStratification_Day,TempMean_degC,NA))%>%
    mutate(PostIcePeriod=ifelse(dayOfYear>IceOutDayofYear&dayOfYear<=(IceOutDayofYear+7),TempMean_degC,NA))

    #**Summarize the 5 different air temp periods by year####
  AnnualData_airTempPeriods<-DailyInterpol.airTemp%>%
    group_by(Year)%>%
    summarize(AirTemp_Summer_degC=mean(SummerPeriod,na.rm=TRUE),
              AirTemp_StratifiedPeriod_degC=mean(StratifiedPeriod,na.rm=TRUE),
              AirTemp_Spring_degC=mean(SpringPeriod,na.rm=TRUE),
              AirTemp_SpringMixed_degC=mean(SpringMixedPeriod,na.rm=TRUE),
              AirTemp_SpringPostIce_degC=mean(PostIcePeriod,na.rm=TRUE))
  #**Merge with AnnualData
  AnnualData<-left_join(AnnualData,AnnualData_airTempPeriods,by="Year")
  




