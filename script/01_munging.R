##Mohonk Lake Climate change analysis
##Created 09Nov2017 David Richardson (hereafter DCR)
##Updated 11Sep2018
##Updated 09Apr2019 - include spring analysis
##Updated 26Jul2019> separates out functions and loading data into separate scripts
##Updated 10Jan2020> includes additional data merging from IAO and DCR meeting on 10Jan2020 at CIES
##                  Anything in here requires modification of data.frame name
##Analyze trends in epi and hypo temperature, stability, phenology of stratification

#Libraries
if (!require(mblm)) {
  install.packages("mblm")
}
if (!require(rLakeAnalyzer)) {
  install.packages("rLakeAnalyzer")
}
if (!require(zoo)) {
  install.packages("zoo")
}
if (!require(trend)) {
  install.packages("trend")
}
if (!require(zyp)) {
  install.packages("zyp")
}
if (!require(Kendall)) {
  install.packages("Kendall")
}
if (!require(tidyverse)) {
  install.packages("tidyverse")
}

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
depths.v <- seq(0, 13, 1)

#Calculate the thermocline depth and stability for all days####
MohonkWeeklyProfilesMetric.derivedData <- MohonkWeeklyProfilesMetric
MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_thresh0.1 <-
  NA
MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_maxdiff <-
  NA
MohonkWeeklyProfilesMetric.derivedData$stability_Jperm2 <- NA
MohonkWeeklyProfilesMetric.derivedData$buoyancyfrequency_1_s2 <- NA

#Code to run thermocline depth and stability calcs for each day
for (iii in 1:length(MohonkWeeklyProfilesMetric.derivedData$Date)) {
  #Figure out if there is at least three temperature measurements in the profile
  if (sum(!is.na(MohonkWeeklyProfilesMetric.derivedData[iii, 5:18])) < 3) {
  } else{
    #Call thermocline function
    MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_thresh0.1[iii] <-
      thermocline.Depth(depths.v, MohonkWeeklyProfilesMetric.derivedData[iii, 5:18], 0.1)
    #Call thermocline function
    MohonkWeeklyProfilesMetric.derivedData$thermoclineDepth_m_maxdiff[iii] <-
      thermocline.Depth.max(depths.v, MohonkWeeklyProfilesMetric.derivedData[iii, 5:18])
    #call stability function
    MohonkWeeklyProfilesMetric.derivedData$stability_Jperm2[iii] <-
      stability.calc(
        as.numeric(MohonkWeeklyProfilesMetric.derivedData[iii, 5:18]),
        depths.v,
        MohonkBathy$SurfaceAreaAtThatDepth_m2,
        MohonkBathy$Depth_m_LowerLimit
      )
    #call stability function
    MohonkWeeklyProfilesMetric.derivedData$buoyancyfrequency_1_s2[iii] <-
      buoyancy.freq.profile.max(MohonkWeeklyProfilesMetric.derivedData[iii, 5:18], depths.v)
    
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
MohonkWeeklyProfilesMetric.derivedData <-
  MohonkWeeklyProfilesMetric.derivedData[MohonkWeeklyProfilesMetric.derivedData$year >
                                           1984, ]

# Create new data frame based on complete years ---------------------------
MohonkWeeklyProfilesMetric.derivedData.fullYears <-
  MohonkWeeklyProfilesMetric.derivedData

#Remove years with missing data (1997,2014)
#MohonkWeeklyProfilesMetric.derivedData.fullYears<-MohonkWeeklyProfilesMetric.derivedData.fullYears[MohonkWeeklyProfilesMetric.derivedData.fullYears$year!=1997&MohonkWeeklyProfilesMetric.derivedData.fullYears$year!=2014,]

#Generate vector of years available
YearAvail <-
  as.numeric(levels(
    factor(MohonkWeeklyProfilesMetric.derivedData.fullYears$year)
  ))

#Generate names of columns of interest to interpolate to the daily level
VarsOfInterest <-
  names(MohonkWeeklyProfilesMetric.derivedData.fullYears)[c(1, 5:18, 21:26)]

#Generate dataframe for interpolated data
DailyInterpol <-
  data.frame(matrix(nrow = 0, ncol = length(VarsOfInterest)))
names(DailyInterpol) <- VarsOfInterest

#For loop to step through each year
for (jj in 1:length(YearAvail)) {
  #Generate a vector of all the days in the year
  temp <-
    MohonkWeeklyProfilesMetric.derivedData.fullYears[MohonkWeeklyProfilesMetric.derivedData.fullYears$year ==
                                                       YearAvail[jj], ]
  #Generate data frame of all the days in the year
  AllDays <-
    data.frame(Date = seq(as.Date(paste(
      YearAvail[jj], "-01-01", sep = ""
    )), as.Date(paste(
      YearAvail[jj], "-12-31", sep = ""
    )), by = "+1 day"))
  #Create empty columns of interest
  AllDays[, VarsOfInterest[-1]] <- NA
  
  #Populate over the values that exist from the temp data frame into the AllDays
  #Use VarsOfInterest[-1] which removes the Date column
  AllDays[which(as.character(AllDays$Date) %in% as.character(temp$Date)), VarsOfInterest[-1]] <-
    temp[, VarsOfInterest[-1]]
  
  #Interpolate all the columns through Zoo package
  zoo.temp <-
    data.frame(Date = AllDays$Date,
               na.approx(
                 AllDays[, VarsOfInterest[-1]],
                 x = AllDays$Date,
                 xout = AllDays$Date,
                 na.rm = F
               ))
  #Replace day of year with uninterpolated value
  zoo.temp$dayofyear <-
    as.POSIXlt(zoo.temp$Date, format = "%d%b%y")$yday + 1
  #Continually build daily interpolated data with each year
  DailyInterpol <- rbind(DailyInterpol, zoo.temp)
  
  #End of the loop to interpolate
}

#Redo year because interpolation has some as NA
DailyInterpol$year <- as.numeric(format(DailyInterpol$Date, "%Y"))
#Interpolate the end and beginning of each year
#For loop to step through each year
for (kk in 1:(length(YearAvail) - 1)) {
  #Generate a vector of all the days in the year
  temp0 <- DailyInterpol[DailyInterpol$year == YearAvail[kk], ]
  temp1 <- DailyInterpol[DailyInterpol$year == YearAvail[kk + 1], ]
  #check if year n and year n+1 are 1 year apart, if not, then do nothing
  if ((YearAvail[kk] + 1) - YearAvail[kk] != 1) {
  } else{
    #find the last value before an NA and the first value after an NA
    NonNAindexY0 <- which(!is.na(temp0$Temp_1m))
    lastNonNA <- max(NonNAindexY0)
    
    NonNAindexY1 <- which(!is.na(temp1$Temp_1m))
    firstNonNA <- min(NonNAindexY1)
    #Generate a data frame from the last non NA value of one year to the first of the next
    tempCross <-
      rbind(temp0[lastNonNA:length(temp0$Date), ], temp1[1:firstNonNA, ])
    
    #Interpolate all the columns through Zoo package
    zoo.tempYearEnd <-
      data.frame(
        Date = tempCross$Date,
        na.approx(
          tempCross[, VarsOfInterest[-1]],
          x = tempCross$Date,
          xout = tempCross$Date,
          na.rm = F
        )
      )
    #Replace day of year with uninterpolated value
    zoo.tempYearEnd$dayofyear <-
      as.POSIXlt(zoo.tempYearEnd$Date, format = "%d%b%y")$yday + 1
    #store the results in DailyInterpol
    DailyInterpol[DailyInterpol$Date %in% zoo.tempYearEnd$Date, ] <-
      zoo.tempYearEnd
  } #End of else loop for consecutive years
  
  #End of for loop through the years
}

# #Calculate the average from the surface (1-3m) and hypolimnion ( --------
#Calculate the average from the surface (1-3m) and hypolimnion (10-12m) for that peak day
DailyInterpol$EpiTemp_degC <-
  rowMeans(DailyInterpol[, c("Temp_1m", "Temp_2m", "Temp_3m")])
DailyInterpol$HypoTemp_degC <-
  rowMeans(DailyInterpol[, c("Temp_10m", "Temp_11m", "Temp_12m")])

#Create a data frame that gets the proportion of volume at each depth
cx <- c(0, cumsum(MohonkBathy$SurfaceAreaAtThatDepth_m2))
#Upper depth
MohonkBathy.volume <-
  data.frame(
    UpperDepth_m = MohonkBathy$Depth_m_LowerLimit[seq(1, (length(MohonkBathy$Depth_m_LowerLimit) -
                                                            1))],
    LowerDepth_m = MohonkBathy$Depth_m_LowerLimit[seq(2, length(MohonkBathy$Depth_m_LowerLimit))]
  )
MohonkBathy.volume$MeanArea_m2 <-
  (cx[(2 + 1):length(cx)] - cx[1:(length(cx) - 2)]) / 2
MohonkBathy.volume$Volume_m3 <- MohonkBathy.volume$MeanArea_m2 * 1
MohonkBathy.volume$Volume_proportion <-
  MohonkBathy.volume$Volume_m3 / sum(MohonkBathy.volume$Volume_m3)

#Calculate the volume weighted mean temperature
#The last reading at the bottom will be extended down to 18m
for (index.l in 1:length(DailyInterpol$Date)) {
  #Check the profile. If there is missing data below 9m, then make extend the lowest value
  tmp.profile <-
    DailyInterpol[index.l, c(
      "Temp_0m",
      "Temp_1m",
      "Temp_2m",
      "Temp_3m",
      "Temp_4m",
      "Temp_5m",
      "Temp_6m",
      "Temp_7m",
      "Temp_8m",
      "Temp_9m",
      "Temp_10m",
      "Temp_11m",
      "Temp_12m",
      "Temp_Bottom"
    )]
  
  #If there are less than 8 temperature readings, average is NA; otherwise, calculate
  if (sum(!is.na(tmp.profile)) < 8) {
    DailyInterpol$VolumeWeightedMeanTemp_degC[index.l] <- NA
  } else{
    #Find the last non-NA entry, extrapolate that down to 13m
    tmp.profile[(tail(which(!is.na(tmp.profile)), 1) + 1):length(tmp.profile)] <-
      tmp.profile[tail(which(!is.na(tmp.profile)), 1)]
    
    #Take the averages between depths
    cx <-
      c(0, cumsum(ifelse(
        is.na(DailyInterpol[index.l, c(
          "Temp_0m",
          "Temp_1m",
          "Temp_2m",
          "Temp_3m",
          "Temp_4m",
          "Temp_5m",
          "Temp_6m",
          "Temp_7m",
          "Temp_8m",
          "Temp_9m",
          "Temp_10m",
          "Temp_11m",
          "Temp_12m",
          "Temp_Bottom"
        )]), 0, DailyInterpol[index.l, c(
          "Temp_0m",
          "Temp_1m",
          "Temp_2m",
          "Temp_3m",
          "Temp_4m",
          "Temp_5m",
          "Temp_6m",
          "Temp_7m",
          "Temp_8m",
          "Temp_9m",
          "Temp_10m",
          "Temp_11m",
          "Temp_12m",
          "Temp_Bottom"
        )]
      )))
    cn <-
      c(0, cumsum(ifelse(is.na(DailyInterpol[index.l, c(
        "Temp_0m",
        "Temp_1m",
        "Temp_2m",
        "Temp_3m",
        "Temp_4m",
        "Temp_5m",
        "Temp_6m",
        "Temp_7m",
        "Temp_8m",
        "Temp_9m",
        "Temp_10m",
        "Temp_11m",
        "Temp_12m",
        "Temp_Bottom"
      )]), 0, 1)))
    rx <- cx[(2 + 1):length(cx)] - cx[1:(length(cx) - 2)]
    rn <- cn[(2 + 1):length(cx)] - cn[1:(length(cx) - 2)]
    rsum <- rx / rn
    #Calculate the weighted volume weighted average temperature for each row by multiplying the interpolated volumes from bathymetry by the interpolated temperatures
    DailyInterpol$VolumeWeightedMeanTemp_degC[index.l] <-
      sum(MohonkBathy.volume$Volume_proportion * c(rsum, rep(
        rsum[length(rsum)],
        length(MohonkBathy.volume$Volume_proportion) - length(rsum)
      )))
  } #End of for loop
}

#Create a week of the year variable
#Based on this technique, there will always be 1 (non-leap year, 365 days)
#Or 2 (leap year, 366 days) in the 53rd week.  Just lump those values with 52nd week
DailyInterpol$weekofyear <- (DailyInterpol$dayofyear - 1) %/% 7 + 1
DailyInterpol$weekofyear[DailyInterpol$weekofyear == 53] <- 52
#max(DailyInterpol$weekofyear)

#*Create a difference between surface and bottom temperature on a daily scale####
DailyInterpol <-
  DailyInterpol %>% mutate(DeltaSurfaceDeep_degC = EpiTemp_degC - HypoTemp_degC)

####ANNUAL DATA Summmary####
#Create a vector of all the years in the thermister data frame
#Create a blank vector for the maximum stability
#Annual data data frame
AnnualData <-
  as.data.frame(matrix(ncol = 1, nrow = length(as.numeric(levels(
    factor(DailyInterpol$year)
  )))))
names(AnnualData) <- c("Year")
#Redo year because interpolation missed some and created NAs
AnnualData$Year <- as.numeric(levels(factor(DailyInterpol$year)))


#Merge AnnualData with IceOff and ice length data
AnnualData <-
  full_join(
    AnnualData,
    MohonkIcePost1985 %>%
      dplyr::select(Year, IceOutDayofYear, LengthOfIceCover_days),
    by = c("Year")
  )

#*Also include the ice on date that ends the open water season.####
#Have to clear out the first row and then reset years by 1 to frame around the open water season.
#If the ice on date is small - that means it ended in the following year and just put 366+the ice in day - the most it went to was a month into the following year
AnnualData <-
  left_join(
    AnnualData,
    MohonkIcePost1985 %>% filter(Year > 1985) %>%
      dplyr::select(Year, IceInDayofYear) %>%
      mutate(Year = Year - 1) %>%
      mutate(
        IceInDayofYear = if_else(IceInDayofYear < 35, 365 + IceInDayofYear, IceInDayofYear)
      )
  )

AnnualData <- AnnualData %>%
  filter(!Year == "2019")
##TEMPORAILY GET RID OF 2019. Seems to be giving issue with calculating MA because of NA for 'IceInDayOfYear'

#Stability cutoff for defining stratified period - in units of J/m2
#*Caluclate stability cutoff####
#**Method 1 to determine cutoff - get the median of each dayOfYear, pettitt.test on the median composite stability####
medianCompositeStability <- DailyInterpol %>%
  dplyr::select(dayofyear, stability_Jperm2) %>%
  group_by(dayofyear) %>%
  dplyr::summarize(MedianStability_Jperm2 = median(stability_Jperm2, na.rm =
                                              TRUE))
#Breakpoint analysis
Stability.cutoff1 <-
  medianCompositeStability$MedianStability_Jperm2[pettitt.test(medianCompositeStability$MedianStability_Jperm2)$estimate]
#plot(medianCompositeStability$MedianStability_Jperm2~medianCompositeStability$dayofyear)
#abline(h=Stability.cutoff1)

#**Methood 2 to determine cutoff - get the cutoff each year and take the median of those####
AnnualBreakpoint.method2 <- DailyInterpol %>%
  mutate(Year = year(Date)) %>%
  dplyr::select(Year, dayofyear, stability_Jperm2) %>%
  filter(!is.na(stability_Jperm2)) %>%
  group_by(Year) %>%
  dplyr::summarize(AnnualBreakpoint = stability_Jperm2[pettitt.test(stability_Jperm2)$estimate[1]])
#hist(temp$AnnualBreakpoint)
Stability.cutoff2 <-
  median(AnnualBreakpoint.method2$AnnualBreakpoint)

#**Method 3 to determine cutoff - get the cutoff each fall and take the median of those
#Cut at day 230 which is after the peak, can play around with this number
AnnualBreakpoint.method3 <- DailyInterpol %>%
  filter(dayofyear >= 240) %>%
  mutate(Year = year(Date)) %>%
  dplyr::select(Year, dayofyear, stability_Jperm2) %>%
  filter(!is.na(stability_Jperm2)) %>%
  group_by(Year) %>%
  dplyr::summarize(AnnualBreakpoint = stability_Jperm2[pettitt.test(stability_Jperm2)$estimate[1]])
median(AnnualBreakpoint.method3$AnnualBreakpoint) #Take the median
#Median of both method from spring and fall
median(
  c(
    AnnualBreakpoint.method2$AnnualBreakpoint,
    AnnualBreakpoint.method3$AnnualBreakpoint
  )
) #Take the median

#**set the stability cutoff equal to the second method - but there was consistency between the two
Stability.cutoff <- Stability.cutoff2

#**Initialize some columns in the AnnualData DF####
AnnualData$MaxStability_Jperm2 <- NA
AnnualData$DayMaxStability <- NA
AnnualData$AUC.Stability_Jperm2day <- NA
AnnualData$AUC.Stability.openwater_Jperm2day <- NA
AnnualData$StartOfStratification_Day <- NA
AnnualData$EndOfStratification_Day <- NA
AnnualData$LengthOfStrat_days <- NA
AnnualData$ThermoclineSlope_mperd <- NA
#Day 172 to 264
AnnualData$SurfaceWaterTemp_Summer_degC <- NA
AnnualData$DeepWaterTemp_Summer_degC <- NA
#Calculate the average from start to end of stratification for each year
#Use StartOfStratification_Day and EndOfStratification_Day
AnnualData$SurfaceWaterTemp_StratifiedPeriod_degC <- NA
AnnualData$DeepWaterTemp_StratifiedPeriod_degC <- NA
#Calculate the average for spring (21Mar to 21Jun) for each year
#Day 80 to <172 (to avoid overlap with summer)
AnnualData$SurfaceWaterTemp_Spring_degC <- NA
AnnualData$DeepWaterTemp_Spring_degC <- NA
#Calculate the average for spring mixed period for each year
#Use ice off (IceOutDayofYear) to StartOfStratification_Day
AnnualData$SurfaceWaterTemp_SpringMixed_degC <- NA
AnnualData$DeepWaterTemp_SpringMixed_degC <- NA
#**Calculate the average for spring mixed post ice period for each year####
#Use ice off (IceOutDayofYear) to +7 days
AnnualData$SurfaceWaterTemp_SpringPostIce_degC <- NA
AnnualData$DeepWaterTemp_SpringPostIce_degC <- NA
#**Calcuate the slope and intercept for the Delta Surface to Deep water during the spring mix period####
AnnualData$DeltaSurfaceDeepTemp_slope_degCpDay <- NA
AnnualData$DeltaSurfaceDeepTemp_intercept_degC <- NA

#*Get the area under the curve for the stability FOR each year####
#k<-31 for debugging
for (k in 1:length(AnnualData$Year)) {
  year.tmp <- AnnualData$Year[k]
  #Pull out relevant data for that year
  dayofyear.tmp <-
    DailyInterpol$dayofyear[DailyInterpol$year == year.tmp]
  stability.tmp <-
    DailyInterpol$stability_Jperm2[DailyInterpol$year == year.tmp]
  thermocline.tmp <-
    DailyInterpol$thermoclineDepth_m_thresh0.1[DailyInterpol$year == year.tmp]
  top123temp.tmp <-
    DailyInterpol[DailyInterpol$year == year.tmp, c("Temp_1m", "Temp_2m", "Temp_3m")]
  bottom101112.tmp <-
    DailyInterpol[DailyInterpol$year == year.tmp, c("Temp_10m", "Temp_11m", "Temp_12m")]
  deltaSurfaceDeep.tmp <-
    DailyInterpol[DailyInterpol$year == year.tmp, c("DeltaSurfaceDeep_degC")]
  
  #Extract the start and end of the open water period from the ice data
  StartOpenWater_doy <-
    AnnualData$IceOutDayofYear[AnnualData$Year == year.tmp]
  EndOpenWater_doy <-
    AnnualData$IceInDayofYear[AnnualData$Year == year.tmp]
  
  #plot(stability.tmp~dayofyear.tmp,main=paste("Year=",year.tmp,sep=""),xlim=c(0,365),ylim=c(0,610))
  #Blue line is 10
  #abline(h=Stability.cutoff,col="blue")
  #REd lines are start and end of stratification
  #abline(v=dayofyear.tmp[min(which(stability.tmp>Stability.cutoff))],col="red")
  #abline(v=dayofyear.tmp[max(which(stability.tmp>Stability.cutoff))],col="red")
  
  #Exclude 1997 because of the paucity of data
  if (year.tmp == 1997) {
  } else{
    AnnualData$MaxStability_Jperm2[k] <- max(stability.tmp, na.rm = TRUE)
    AnnualData$DayMaxStability[k] <-
      dayofyear.tmp[which.max(stability.tmp)]
  }
  
  #**Calculations of the total stratified stability####
  #Area under the curve for first time above cutoff from above to first time below
  #Exclude 1984,1997,2014 because of incomplete curves
  if (year.tmp == 1984 | year.tmp == 1997 | year.tmp == 2014) {
  } else{
    #Pull out the day which is the first >cutoff reading, pull datasets for day of year, stability, and thermocline depth
    dayofyear.tmp2 <-
      dayofyear.tmp[min(which(stability.tmp > Stability.cutoff)):max(which(stability.tmp >
                                                                             Stability.cutoff))]
    stability.tmp2 <-
      stability.tmp[min(which(stability.tmp > Stability.cutoff)):max(which(stability.tmp >
                                                                             Stability.cutoff))]
    thermocline.tmp2 <-
      thermocline.tmp[min(which(stability.tmp > Stability.cutoff)):max(which(stability.tmp >
                                                                               Stability.cutoff))]
    
    #Extract the stability for the open water period
    dayofyear.tmp.openwater <-
      dayofyear.tmp[dayofyear.tmp > StartOpenWater_doy &
                      dayofyear.tmp < EndOpenWater_doy]
    stability.tmp.openwater <-
      stability.tmp[dayofyear.tmp > StartOpenWater_doy &
                      dayofyear.tmp < EndOpenWater_doy]
    if (EndOpenWater_doy > 365) {
      MaxDay <- max(dayofyear.tmp.openwater) + 1
      dayofyear.tmp.openwater <-
        c(dayofyear.tmp.openwater,
          seq(MaxDay, EndOpenWater_doy, by = 1))
      stability.tmp.openwater <-
        c(stability.tmp.openwater,
          rep(stability.tmp.openwater[max(which(!is.na(stability.tmp.openwater)))], length(
            seq(MaxDay, EndOpenWater_doy, by = 1)
          )))
    }
    
    #Calculate the area under the curve for those days in the stratified period and open water period
    AnnualData$AUC.Stability_Jperm2day[k] <-
      sum(diff(dayofyear.tmp2[!is.na(stability.tmp2)]) * (head(stability.tmp2[!is.na(stability.tmp2)], -1) +
                                                            tail(stability.tmp2[!is.na(stability.tmp2)], -1)) / 2)
    AnnualData$AUC.Stability.openwater_Jperm2day[k] <-
      sum(diff(dayofyear.tmp.openwater[!is.na(stability.tmp.openwater)]) * (
        head(stability.tmp.openwater[!is.na(stability.tmp.openwater)], -1) + tail(stability.tmp.openwater[!is.na(stability.tmp.openwater)], -1)
      ) / 2)
    
    #Summarize some phenology of stratification
    AnnualData$StartOfStratification_Day[k] <-
      dayofyear.tmp[min(which(stability.tmp > Stability.cutoff))]
    AnnualData$EndOfStratification_Day[k] <-
      dayofyear.tmp[max(which(stability.tmp > Stability.cutoff))]
    AnnualData$LengthOfStrat_days[k] <-
      AnnualData$EndOfStratification_Day[k] - AnnualData$StartOfStratification_Day[k]
    
    #Calculate the slope of the thermocline
    lm <- lm(thermocline.tmp2 ~ dayofyear.tmp2)
    s.slope <- sens.slope(thermocline.tmp2[!is.na(thermocline.tmp2)])
    AnnualData$ThermoclineSlope_mperd[k] <- s.slope$estimates
    #plot(thermocline.tmp2~dayofyear.tmp2)
    
    #abline(lm(thermocline.tmp2~dayofyear.tmp2))
    
    #mtext(paste("SenSlope=",round(s.slope$estimates,4),"LM slope=",round(lm$coefficients[2],4)),side=3,line=-1)
  }
  
  #**Calculate the average of June 21 to Sep 21 for each year####
  #Day 172 to 264
  AnnualData$SurfaceWaterTemp_Summer_degC[k] <-
    mean(as.matrix(top123temp.tmp[dayofyear.tmp >= 172 &
                                    dayofyear.tmp <= 264, ]), na.rm = T)
  AnnualData$DeepWaterTemp_Summer_degC[k] <-
    mean(as.matrix(bottom101112.tmp[dayofyear.tmp >= 172 &
                                      dayofyear.tmp <= 264, ]), na.rm = T)
  #**Calculate the average from start to end of stratification for each year####
  #Use StartOfStratification_Day and EndOfStratification_Day
  AnnualData$SurfaceWaterTemp_StratifiedPeriod_degC[k] <-
    mean(as.matrix(top123temp.tmp[dayofyear.tmp >= AnnualData$StartOfStratification_Day[k] &
                                    dayofyear.tmp <= AnnualData$EndOfStratification_Day[k], ]), na.rm = T)
  AnnualData$DeepWaterTemp_StratifiedPeriod_degC[k] <-
    mean(as.matrix(bottom101112.tmp[dayofyear.tmp >= AnnualData$StartOfStratification_Day[k] &
                                      dayofyear.tmp <= AnnualData$EndOfStratification_Day[k], ]), na.rm = T)
  #**Calculate the average for spring (21Mar to 21Jun) for each year####
  #Day 80 to <172 (to avoid overlap with summer)
  AnnualData$SurfaceWaterTemp_Spring_degC[k] <-
    mean(as.matrix(top123temp.tmp[dayofyear.tmp >= 80 &
                                    dayofyear.tmp < 172, ]), na.rm = T)
  AnnualData$DeepWaterTemp_Spring_degC[k] <-
    mean(as.matrix(bottom101112.tmp[dayofyear.tmp >= 80 &
                                      dayofyear.tmp < 172, ]), na.rm = T)
  
  #**Calculate the average for spring mixed period for each year####
  #Use ice off (IceOutDayofYear) to StartOfStratification_Day
  AnnualData$SurfaceWaterTemp_SpringMixed_degC[k] <-
    mean(as.matrix(top123temp.tmp[dayofyear.tmp >= AnnualData$IceOutDayofYear[k] &
                                    dayofyear.tmp < AnnualData$StartOfStratification_Day[k], ]), na.rm = T)
  AnnualData$DeepWaterTemp_SpringMixed_degC[k] <-
    mean(as.matrix(bottom101112.tmp[dayofyear.tmp >= AnnualData$IceOutDayofYear[k] &
                                      dayofyear.tmp < AnnualData$StartOfStratification_Day[k], ]), na.rm = T)
  
  #**Calculate the average for spring mixed post ice period for each year####
  #Use ice off (IceOutDayofYear) to +7 days
  AnnualData$SurfaceWaterTemp_SpringPostIce_degC[k] <-
    mean(as.matrix(top123temp.tmp[dayofyear.tmp > AnnualData$IceOutDayofYear[k] &
                                    dayofyear.tmp <= AnnualData$IceOutDayofYear[k] + 7, ]), na.rm = T)
  AnnualData$DeepWaterTemp_SpringPostIce_degC[k] <-
    mean(as.matrix(bottom101112.tmp[dayofyear.tmp > AnnualData$IceOutDayofYear[k] &
                                      dayofyear.tmp <= AnnualData$IceOutDayofYear[k] + 7, ]), na.rm = T)
  
  #**Caclulate the slope of the delta temperature for each year####
  #Use ice off (IceOutDayofYear) to StartOfStratification_Day
  deltaSpringMix <-
    deltaSurfaceDeep.tmp[dayofyear.tmp >= AnnualData$IceOutDayofYear[k] &
                           dayofyear.tmp < AnnualData$StartOfStratification_Day[k]]
  dayofYearSpringMix <-
    dayofyear.tmp[dayofyear.tmp >= AnnualData$IceOutDayofYear[k] &
                    dayofyear.tmp < AnnualData$StartOfStratification_Day[k]]
  #Exclude 1997, 2014 because of the paucity of data, 2015 has stratification date happen before ice off day
  if (year.tmp == 1997 | year.tmp == 2014 | year.tmp == 2015) {
  } else{
    lm.deltaSpringMix <- summary(lm(deltaSpringMix ~ dayofYearSpringMix))
    AnnualData$DeltaSurfaceDeepTemp_slope_degCpDay[k] <-
      lm.deltaSpringMix$coefficients[2, "Estimate"]
    AnnualData$DeltaSurfaceDeepTemp_intercept_degC[k] <-
      lm.deltaSpringMix$coefficients[1, "Estimate"]
  }
}
#End of for loop through the years

#Some looking at the selection of the different spring periods
#There are three: spring, post ice to spring mix, and ice off+7 days
#
# temp<-AnnualData%>%dplyr::select(Year,IceOutDayofYear,StartOfStratification_Day)%>%mutate(Difference=StartOfStratification_Day-IceOutDayofYear)
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
AnnualData$DateMaxStability <-
  as.Date(AnnualData$DayMaxStability, origin = as.Date(paste(AnnualData$Year, "-01-01", sep =
                                                               "")))

#**Calculate the average from the surface (1-3m) and hypolimnion (10-12m) for that peak stratification day#####
temp.Annualmeans <-
  as.data.frame(matrix(ncol = 1, nrow = sum(!is.na(
    AnnualData$DateMaxStability
  ))))
names(temp.Annualmeans) <- c("Year")
temp.Annualmeans$Year <-
  AnnualData$Year[!is.na(AnnualData$DateMaxStability)]
temp.Annualmeans$SurfaceWaterTemp_PeakStratification_degC <-
  rowMeans(DailyInterpol[DailyInterpol$Date %in% AnnualData$DateMaxStability, c("Temp_1m", "Temp_2m", "Temp_3m")])
temp.Annualmeans$DeepWaterTemp_PeakStratification_degC <-
  rowMeans(DailyInterpol[DailyInterpol$Date %in% AnnualData$DateMaxStability, c("Temp_10m", "Temp_11m", "Temp_12m")])

#Merge with annual data
AnnualData <- merge(AnnualData, temp.Annualmeans, by = "Year", all.x = T)
#remove the temporary data frame for peak stratification
rm(temp.Annualmeans)
#Convert the area under the curve to a Mixing action with units of gigaJoules*day
AnnualData$MixingAction_gigaJday <-
  AnnualData$AUC.Stability_Jperm2day * 69000 / (10 ^ 9)
AnnualData$MixingAction.OpenWater_gigaJday <-
  AnnualData$AUC.Stability.openwater_Jperm2day * 69000 / (10 ^ 9)

# NAO dataframe 1950-present ----------------------------------------------

NAO_summary <- NAO_daily %>%
  mutate(
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
    water_year = ifelse(
      season %in% c("fall", "winter") &
        Month %in% c("9","10","11","12"),
      # season %in% c("winter", "spring") &
      #   Month %in% c("10", "11", "12", "1", "2", "3", "4"),
      Year + 1, #IAO 2022-07-08 changed to +1 instead of -1!!!
      Year
    )
  ) %>% #this water_year term is only relevent for winter
  #metrics. We want 1 Oct-30 April to all correspond to the same water year
  group_by(water_year, season) %>%
  dplyr::summarize(NAO_index = mean(NAO_index)) %>%
  pivot_wider(
    names_from = "season",
    names_sep = "_",
    names_prefix = "NAO_index_",
    values_from = "NAO_index"
  ) %>%
  ungroup() 
  # mutate(Year = water_year + 1) # Consider dropping Year because we want to join with water_year on other dataframes. 


# Why not just use the dataRetrieval package calcWaterYear function and see if the answers differ
# library(dataRetrieval)
# NAO_summary_alt <- NAO_daily %>%
#   mutate(
#     season =
#       ifelse(
#         Month %in% c(12, 1, 2),
#         "winter",
#         ifelse(
#           Month %in% c(3, 4, 5),
#           "spring",
#           ifelse(
#             Month %in% c(6, 7, 8),
#             "summer",
#             ifelse(Month %in% c(9, 10, 11), "fall", "error")
#           )
#         )
#       ),
#     water_year = calcWaterYear(Date)
#   )  %>%#this water_year term is only relevent for winter
#   #metrics. We want 1 Oct-30 April to all correspond to the same water year
#   group_by(water_year, season) %>%
#   dplyr::summarize(NAO_index = mean(NAO_index)) %>%
#   # filter(season%in%c("winter","spring")) %>% #where winter is [1oct-28feb]  & spring is [1march-30april]
#   pivot_wider(
#     names_from = "season",
#     names_sep = "_",
#     names_prefix = "NAO_index_",
#     values_from = "NAO_index"
#   ) %>%
#   ungroup() %>%
#   mutate(Year = water_year + 1)
# 
# test<- NAO_summary_alt %>%
#   filter(Date>="2018-09-01" & Date <= "2018-11-30")


#**Seasonal ENSO indices ----------------------------------------------------

#Summarizing ENSO data slightly different from NAO since we only have monthly times steps:
# * spring mean (april + may + june)
# * summer mean (july + august + sept)


# ENSO_seasonal <- ENSO_monthly %>%
#   #Adds a category for season
#   mutate(Season =
#            ifelse(MON %in% c(4, 5, 6), "spring",
#                   ifelse(MON %in% c(7, 8, 9), "summer",
#                          ifelse(MON %in% c(1, 2, 3, 10, 11, 12), "other", "ERROR")))) %>%
#   filter(Season %in% c("spring","summer"))%>%
#   rename(Year=YR)
#
# ENSO_seasonal <- ENSO_monthly %>%
#   #Adds a category for season
#   mutate(Season =
#            ifelse(MON %in% c(4, 5, 6), "spring",
#                   ifelse(MON %in% c(7, 8, 9), "summer",
#                          ifelse(MON %in% c(1, 2, 3, 10, 11, 12), "other", "ERROR")))) %>%
#   filter(Season %in% c("spring","summer"))%>%
#   rename(Year=YR)
#
# ENSO_spring <- ENSO_seasonal %>%
#   filter(Season=="spring")%>%
#   group_by(Year)%>%
#   summarize_at(vars(ANOM), mean, na.rm=TRUE)%>%
#   rename(ENSO_Spring=ANOM)
#
# ENSO_summer <- ENSO_seasonal %>%
#   filter(Season=="summer")%>%
#   group_by(Year)%>%
#   summarize_at(vars(ANOM), mean, na.rm=TRUE)%>%
#   rename(ENSO_Summer=ANOM)
#
#
# ENSO_summary <- left_join(ENSO_spring,ENSO_summer, by="Year" )


#***Merge with AnnualData by Year####
AnnualData <- left_join(AnnualData, NAO_summary, by = c("Year"="water_year"))
# AnnualData<-left_join(AnnualData,ENSO_summary,by=c("Year"))

#**Seasonal and monthly ENSO- ONI indices ----------------------------------------------------
#ONI stands for Oceanic Nina Index

ENSO_summary <- ENSO_monthly %>%
  mutate(
    season =
      ifelse(
        month %in% c(12, 1, 2),
        "winter",
        ifelse(
          month %in% c(3, 4, 5),
          "spring",
          ifelse(
            month %in% c(6, 7, 8),
            "summer",
            ifelse(month %in% c(9, 10, 11), "fall", "error")
          )
        )
      ),
    water_year = ifelse(
      season %in% c("fall", "winter") &
        month %in% c("9","10","11","12"),
      year + 1, #IAO 2022-07-08 changed to +1 instead of -1!!!
      year
    )
  ) %>% #this water_year term is only relevent for winter
  #   water_year = ifelse(
  #     season %in% c("winter", "spring") &
  #       month %in% c("10", "11", "12", "1", "2", "3", "4"),
  #     year - 1,
  #     year
  #   )
  # ) %>% #this water_year term is only relevent for winter
  #metrics. We want 1 Oct-30 April to all correspond to the same water year
  group_by(water_year, season) %>%
  dplyr::summarize(ENSO_index = mean(ENSO_index)) %>%
  pivot_wider(
    names_from = "season",
    names_sep = "_",
    names_prefix = "ENSO_index_",
    values_from = "ENSO_index"
  ) %>%
  ungroup()
  # mutate(Year = water_year + 1)


ENSO_monthly_trim <- ENSO_monthly %>%
  mutate(
    water_year = ifelse(
        # month %in% c("10", "11", "12", "1", "2", "3", "4"),
      month %in% c("10", "11", "12"),
      year + 1,
      year
    ),
    MonthAbb = month.abb[month]
  ) %>% #this water_year term is only relevent for winter
  #metrics. We want 1 Oct-30 April to all correspond to the same water year
  filter(month %in% c("10", "11", "12", "1", "2", "3", "4")) %>%
  select(water_year,ENSO_index,MonthAbb) %>%
  rename(ENSO=ENSO_index) %>%
  pivot_wider(
    names_from = "MonthAbb",
    names_sep = "_",
    names_prefix = "ENSO_",
    values_from = "ENSO"
  ) %>%
  ungroup() %>%
  mutate(
    # Year = water_year + 1,
         ENSO_Oct_to_Mar = ENSO_Oct+ENSO_Nov+ENSO_Dec+
                                 ENSO_Jan+ENSO_Feb+ENSO_Mar)

ENSO_summary<-left_join(ENSO_summary, ENSO_monthly_trim, by=c("water_year"))


#***Merge with AnnualData by Year####
AnnualData <- left_join(AnnualData, ENSO_summary, by = c("Year"="water_year"))

##quick QAQC- how do  ENSO and ENSO_MEI compare?
# ggplot(AnnualData, aes(x=ENSO_Spring.x,y=ENSO_Spring))+geom_point()
# ggplot(AnnualData, aes(x=ENSO_Summer.x,y=ENSO_Summer))+geom_point()
#Yes they are highly correlated.


# * Load NOAA annual temperature anomaly ----------------------------------
#Pulled from: https://www.ncdc.noaa.gov/cag/global/time-series
#Specs: Time scale = Annual
#       Region = Global
#       Surface = Land and Ocean

# NOAA_anomaly <- read.csv("data/NOAA_globaltempanomaly.csv") %>%
#   rename(GlobalTempanomaly_C = Value)

NOAA_anomaly_monthly <- read.csv("data/NOAA_globaltempanomaly_monthly_1929-2022.csv") %>%
  rename(GlobalTempanomaly_C = Value)

NOAA_anomaly <- NOAA_anomaly_monthly %>%
  group_by(Year) %>%
  dplyr::summarize(GlobalTempanomaly_C=mean(GlobalTempanomaly_C, na.rm=T))

#* Merge temp anom. with AnnualData by Year####
AnnualData <- left_join(AnnualData, NOAA_anomaly, by = c("Year"))

#Daily Interpolate Secchi####
#There are several repeated days - some are NA for both duplicates, other have two secchi readings
#Remove those duplicated values by averaging the secchi values
MohonkWeeklySecchi <-
  MohonkWeeklySecchi %>% group_by(Date) %>% summarise(Secchi_m = mean(Secchi_m, na.rm =
                                                                        TRUE))

#Create data frame with dates from daily interpolate dataframe
DailyInterpol.secchi <- data.frame(Date = DailyInterpol$Date)

#Merge dates with secchi weekly data frame
DailyInterpol.secchi <-
  left_join(DailyInterpol.secchi, MohonkWeeklySecchi, by = "Date")

#Linearly interpolate across all years (this is slightly different than the temperature interpolation)
#This ignores gaps and keeps in all years
DailyInterpol.secchi <- data.frame(
  Date = DailyInterpol.secchi$Date,
  na.approx(
    DailyInterpol.secchi$Secchi_m,
    x = DailyInterpol.secchi$Date,
    xout = DailyInterpol.secchi$Date,
    na.rm = FALSE
  )
)
#Rename the Secchi column
names(DailyInterpol.secchi)[2] <- "Secchi_m"

#Add in a year column
DailyInterpol.secchi <- DailyInterpol.secchi %>%
  mutate(Year = year(Date))

#*Secchi to annual summary####
#**Add in columns for all the important dates####
#Create day of year
#Get the start spring start of summer and end of summer - those are set for each year
#merge with AnnualData to get IceOff, start and end of stratification in there for each year
#Set NA or value columns based on the 4 periods of interest - leave off spring post ice because there will likely be 0 or 1 measurements in there?
DailyInterpol.secchiPeriods <- DailyInterpol.secchi %>%
  mutate(dayOfYear = yday(Date)) %>%
  mutate(
    StartOfSummer = 172,
    StartOfSpring = 80,
    EndOfSummer = 265
  ) %>%
  left_join(
    .,
    AnnualData %>% dplyr::select(
      Year,
      StartOfStratification_Day,
      EndOfStratification_Day,
      IceOutDayofYear
    ),
    by = "Year"
  ) %>%
  mutate(SummerPeriod = ifelse(dayOfYear >= StartOfSummer &
                                 dayOfYear < EndOfSummer, Secchi_m, NA)) %>%
  mutate(
    StratifiedPeriod = ifelse(
      dayOfYear >= StartOfStratification_Day &
        dayOfYear < EndOfStratification_Day,
      Secchi_m,
      NA
    )
  ) %>%
  mutate(SpringPeriod = ifelse(
    dayOfYear > StartOfSpring & dayOfYear < StartOfSummer,
    Secchi_m,
    NA
  )) %>%
  mutate(
    SpringMixedPeriod = ifelse(
      dayOfYear > IceOutDayofYear &
        dayOfYear <= StartOfStratification_Day,
      Secchi_m,
      NA
    )
  )
#**Summarize the 4 different secchi periods by year####
AnnualData_SecchiPeriods <-
  DailyInterpol.secchiPeriods %>% group_by(Year) %>% dplyr::summarize(
    SecchiDepth_Summer_m = mean(SummerPeriod, na.rm = TRUE),
    SecchiDepth_StratifiedPeriod_m = mean(StratifiedPeriod, na.rm = TRUE),
    SecchiDepth_Spring_m = mean(SpringPeriod, na.rm = TRUE),
    SecchiDepth_SpringMixed_m = mean(SpringMixedPeriod, na.rm = TRUE)
  )
#**Merge with AnnualData
AnnualData <-
  left_join(AnnualData, AnnualData_SecchiPeriods, by = "Year")

#*VolumeWeightedMeans to annual summary####
#**Add in columns for all the important dates####
#Create day of year
#Get the start spring start of summer and end of summer - those are set for each year
#merge with AnnualData to get IceOff, start and end of stratification in there for each year
#Set NA or value columns based on the 5 periods of interest - include spring post ice
DailyInterpol.volWtMeansPeriods <- DailyInterpol %>%
  mutate(Year = year(Date), dayOfYear = yday(Date)) %>%
  dplyr::select(Year, dayOfYear, VolumeWeightedMeanTemp_degC) %>%
  mutate(
    StartOfSummer = 172,
    StartOfSpring = 80,
    EndOfSummer = 265
  ) %>%
  left_join(
    .,
    AnnualData %>% dplyr::select(
      Year,
      StartOfStratification_Day,
      EndOfStratification_Day,
      IceOutDayofYear
    ),
    by = "Year"
  ) %>%
  mutate(
    SummerPeriod = ifelse(
      dayOfYear >= StartOfSummer &
        dayOfYear < EndOfSummer,
      VolumeWeightedMeanTemp_degC,
      NA
    )
  ) %>%
  mutate(
    StratifiedPeriod = ifelse(
      dayOfYear >= StartOfStratification_Day &
        dayOfYear < EndOfStratification_Day,
      VolumeWeightedMeanTemp_degC,
      NA
    )
  ) %>%
  mutate(
    SpringPeriod = ifelse(
      dayOfYear > StartOfSpring &
        dayOfYear < StartOfSummer,
      VolumeWeightedMeanTemp_degC,
      NA
    )
  ) %>%
  mutate(
    SpringMixedPeriod = ifelse(
      dayOfYear > IceOutDayofYear &
        dayOfYear <= StartOfStratification_Day,
      VolumeWeightedMeanTemp_degC,
      NA
    )
  ) %>%
  mutate(
    PostIcePeriod = ifelse(
      dayOfYear > IceOutDayofYear &
        dayOfYear <= (IceOutDayofYear + 7),
      VolumeWeightedMeanTemp_degC,
      NA
    )
  )
#**Summarize the 5 different volume weighted mean water temp periods by year####
AnnualData_volWtMeansPeriods <-
  DailyInterpol.volWtMeansPeriods %>% group_by(Year) %>% dplyr::summarize(
    VolumeWeightedMeanTemp_Summer_degC = mean(SummerPeriod, na.rm = TRUE),
    VolumeWeightedMeanTemp_StratifiedPeriod_degC = mean(StratifiedPeriod, na.rm =
                                                          TRUE),
    VolumeWeightedMeanTemp_Spring_degC = mean(SpringPeriod, na.rm = TRUE),
    VolumeWeightedMeanTemp_SpringMixed_degC = mean(SpringMixedPeriod, na.rm =
                                                     TRUE),
    VolumeWeightedMeanTemp_SpringPostIce_degC = mean(PostIcePeriod, na.rm =
                                                       TRUE)
  )
#**Merge with AnnualData
AnnualData <-
  left_join(AnnualData, AnnualData_volWtMeansPeriods, by = "Year")

#*Air temperature to annual summary####
#**Truncate to the correct data size (1985-2017)
MohonkDailyWeatherTruncate <-
  MohonkDailyWeatherFull %>%
  filter(Date >= min(DailyInterpol$Date))
#**Add in columns for all the important dates####
#Create day of year
#Get the start spring start of summer and end of summer - those are set for each year
#merge with AnnualData to get IceOff, start and end of stratification in there for each year
#Set NA or value columns based on the 5 periods of interest - include spring post ice
DailyInterpol.airTemp <- MohonkDailyWeatherTruncate %>%
  mutate(Year = year(Date),
         dayOfYear = yday(Date)) %>%
  dplyr::select(Year, dayOfYear, TempMean_degC) %>%
  mutate(
    StartOfSummer = 172,
    StartOfSpring = 80,
    EndOfSummer = 265
  ) %>%
  left_join(
    .,
    AnnualData %>%
      dplyr::select(
        Year,
        StartOfStratification_Day,
        EndOfStratification_Day,
        IceOutDayofYear
      ),
    by = "Year"
  ) %>%
  mutate(SummerPeriod = ifelse(
    dayOfYear >= StartOfSummer &
      dayOfYear < EndOfSummer,
    TempMean_degC,
    NA
  )) %>%
  mutate(
    StratifiedPeriod = ifelse(
      dayOfYear >= StartOfStratification_Day &
        dayOfYear < EndOfStratification_Day,
      TempMean_degC,
      NA
    )
  ) %>%
  mutate(
    SpringPeriod = ifelse(
      dayOfYear > StartOfSpring &
        dayOfYear < StartOfSummer,
      TempMean_degC,
      NA
    )
  ) %>%
  mutate(
    SpringMixedPeriod = ifelse(
      dayOfYear > IceOutDayofYear &
        dayOfYear <= StartOfStratification_Day,
      TempMean_degC,
      NA
    )
  ) %>%
  mutate(PostIcePeriod = ifelse(
    dayOfYear > IceOutDayofYear &
      dayOfYear <= (IceOutDayofYear + 7),
    TempMean_degC,
    NA
  ))

#**Summarize the 5 different air temp periods by year####
AnnualData_airTempPeriods <- DailyInterpol.airTemp %>%
  group_by(Year) %>%
  dplyr::summarize(
    AirTemp_Summer_degC = mean(SummerPeriod, na.rm = TRUE),
    AirTemp_StratifiedPeriod_degC = mean(StratifiedPeriod, na.rm =
                                           TRUE),
    AirTemp_Spring_degC = mean(SpringPeriod, na.rm = TRUE),
    AirTemp_SpringMixed_degC = mean(SpringMixedPeriod, na.rm =
                                      TRUE),
    AirTemp_SpringPostIce_degC = mean(PostIcePeriod, na.rm = TRUE)
  )
#**Merge with AnnualData
AnnualData <-
  left_join(AnnualData, AnnualData_airTempPeriods, by = "Year")

#*Precip to annual summary####
#**Truncate to the correct data size (1985-2017)
#**Add in columns for all the important dates####
#Create day of year
#Get the start spring start of summer and end of summer - those are set for each year
#merge with AnnualData to get IceOff, start and end of stratification in there for each year
#Set NA or value columns based on the 5 periods of interest - include spring post ice
DailyInterpol.precip <- MohonkDailyWeatherTruncate %>%
  mutate(Year = year(Date),
         dayOfYear = yday(Date)) %>%
  dplyr::select(Year, dayOfYear, Precip_mm) %>%
  mutate(
    StartOfSummer = 172,
    StartOfSpring = 80,
    EndOfSummer = 265
  ) %>%
  left_join(
    .,
    AnnualData %>%
      dplyr::select(
        Year,
        StartOfStratification_Day,
        EndOfStratification_Day,
        IceOutDayofYear
      ),
    by = "Year"
  ) %>%
  mutate(SummerPeriod = ifelse(
    dayOfYear >= StartOfSummer & dayOfYear < EndOfSummer,
    Precip_mm,
    NA
  )) %>%
  mutate(
    StratifiedPeriod = ifelse(
      dayOfYear >= StartOfStratification_Day &
        dayOfYear < EndOfStratification_Day,
      Precip_mm,
      NA
    )
  ) %>%
  mutate(SpringPeriod = ifelse(
    dayOfYear > StartOfSpring & dayOfYear < StartOfSummer,
    Precip_mm,
    NA
  )) %>%
  mutate(
    SpringMixedPeriod = ifelse(
      dayOfYear > IceOutDayofYear &
        dayOfYear <= StartOfStratification_Day,
      Precip_mm,
      NA
    )
  ) %>%
  mutate(PostIcePeriod = ifelse(
    dayOfYear > IceOutDayofYear &
      dayOfYear <= (IceOutDayofYear + 7),
    Precip_mm,
    NA
  ))

#**Summarize the 5 different precip periods by year - summation####
AnnualData_precipPeriods <- DailyInterpol.precip %>%
  group_by(Year) %>%
  dplyr::summarize(
    Precip_Summer_mm = sum(SummerPeriod, na.rm = TRUE),
    Precip_StratifiedPeriod_mm = sum(StratifiedPeriod, na.rm =
                                       TRUE),
    Precip_Spring_mm = sum(SpringPeriod, na.rm = TRUE),
    Precip_SpringMixed_mm = sum(SpringMixedPeriod, na.rm = TRUE),
    Precip_SpringPostIce_degC = sum(PostIcePeriod, na.rm = TRUE)
  )

#Replace sums of 0 with NA because those are missing for 1997 and 2014 (and spring mixed for 2015)
AnnualData_precipPeriods <-
  AnnualData_precipPeriods %>% mutate(
    Precip_StratifiedPeriod_mm = na_if(Precip_StratifiedPeriod_mm, 0),
    Precip_SpringMixed_mm = na_if(Precip_SpringMixed_mm, 0)
  )

#**Merge with AnnualData
AnnualData <-
  left_join(AnnualData, AnnualData_precipPeriods, by = "Year")




# Winter metrics ----------------------------------------------------------

#IAO- added 2022-Feb-18 and updated 2022-06-08

#here I am using a function for calculating water-year DOY. This will help facilitate plotting and analysizing trends in ice-in since they span either side of the winter-year (e.g., 2011-2012). For example, an IceInDayofYear_fed value of 150 means Ice-In occured 150 days after the start of the water-year (Oct1)
MohonkIce <- MohonkIce %>%
  mutate(
    IceInDayofYear_fed = hydro.day(IceInDate),
    IceOutDayofYear_fed = hydro.day(IceOutDate)
  )

#I would recommend using IceInDayofYear_fed instead of IceInDayofYear as a response variable, otherwise you have a handful of observations with very low DOY:
MohonkIce %>%
  ggplot(aes(x = Year, y = IceInDayofYear)) +
  geom_point(size = 3, shape = 21, fill = "white")

#Starting with the 'MohonkDailyWeatherFull' dataframe which has daily min, mean, max temps and precip as snow or rain, I created a dataframe with monthly to seasonal cumulative metrics.
MohonkDailyWeather_monthly <- MohonkDailyWeatherFull %>%
  mutate(
    year = year(Date),
    month = month(Date),
    month_name = month(Date, label = TRUE),
    water_year = dataRetrieval::calcWaterYear(Date) #IAO - 2022-07-08 - I don't trust myself anymore so just using a function
    # water_year = ifelse(month %in% c("1", "2", "3", "4"),
    #                     year - 1,
    #                     year)
  ) %>% 
  filter(!month %in% c("5", "6", "7", "8")) %>%  #exclude May-August, which are *most likely* not directly influencing winter ice phenology
  mutate(water_year_corrected = case_when(month == '9' ~ water_year + 1,
                         TRUE ~ water_year)) %>% #This is a little janky but we want to pretend that Sept is part of the same water year as the next month (Oct) for the purposes of summarizing the data below
  group_by(water_year, month_name) %>%
  dplyr::summarize(
    cumMeanDailyT = sum(TempMean_degC, na.rm = TRUE),
    cumSnow = sum(Snow_mm, na.rm = TRUE),
    cumRain = sum(Precip_mm, na.rm = TRUE),
    percPrecipRain = (cumRain / (cumRain + cumSnow)) *
      100,
    nDaysMeanBelowZero = sum(TempMean_degC < 0, na.rm =
                               TRUE),
    nDaysMinBelowZero = sum(TempMin_degC < 0, na.rm = TRUE)
  ) %>%
  #Convert dataframe from long to wide format
  pivot_wider(
    names_from = "month_name",
    names_sep = "_",
    values_from = c(
      "cumMeanDailyT",
      "cumSnow",
      "cumRain",
      "percPrecipRain",
      "nDaysMeanBelowZero",
      "nDaysMinBelowZero"
    )
  ) %>%
  
  ##Predictors probably more relevant for ice-on DOY
  mutate(
    cumMeanDailyT_SepOct = cumMeanDailyT_Sep + cumMeanDailyT_Oct,
    cumMeanDailyT_SepOctNov = cumMeanDailyT_Sep + cumMeanDailyT_Oct + cumMeanDailyT_Nov,
    cumMeanDailyT_OctNov = cumMeanDailyT_Oct + cumMeanDailyT_Nov,
    cumMeanDailyT_OctNovDec = cumMeanDailyT_Oct + cumMeanDailyT_Nov + cumMeanDailyT_Dec,
    
    cumSnow_OctNov = cumSnow_Oct + cumSnow_Nov,
    cumSnow_OctNovDec = cumSnow_Oct + cumSnow_Nov + cumSnow_Dec,
    cumSnow_SepOct = cumSnow_Oct + cumSnow_Sep,
    cumSnow_SepOctNov = cumSnow_Oct + cumSnow_Sep + cumSnow_Nov,
    
    cumRain_SepOct = cumRain_Sep + cumRain_Oct,
    cumRain_SepOctNov = cumRain_Sep + cumRain_Oct + cumRain_Nov,
    cumRain_OctNov = cumRain_Oct + cumRain_Nov,
    cumRain_OctNovDec = cumRain_Oct + cumRain_Nov + cumRain_Dec,
    
    nDaysMeanBelowZero_SepOct = nDaysMeanBelowZero_Sep + nDaysMeanBelowZero_Oct,
    nDaysMeanBelowZero_SepOctNov = nDaysMeanBelowZero_Sep + nDaysMeanBelowZero_Oct + nDaysMeanBelowZero_Nov,
    nDaysMeanBelowZero_OctNov = nDaysMeanBelowZero_Oct + nDaysMeanBelowZero_Nov,
    nDaysMeanBelowZero_OctNovDec = nDaysMeanBelowZero_Oct + nDaysMeanBelowZero_Nov + nDaysMeanBelowZero_Dec,
    
    nDaysMinBelowZero_SepOct = nDaysMinBelowZero_Sep + nDaysMinBelowZero_Oct,
    nDaysMinBelowZero_SepOctNov = nDaysMinBelowZero_Sep + nDaysMinBelowZero_Oct + nDaysMinBelowZero_Nov,
    nDaysMinBelowZero_OctNov = nDaysMinBelowZero_Oct + nDaysMinBelowZero_Nov,
    nDaysMinBelowZero_OctNovDec = nDaysMinBelowZero_Oct + nDaysMinBelowZero_Nov + nDaysMinBelowZero_Dec,
    
    ##Predictors probably more relevant for ice-off DOY
    cumMeanDailyT_MarApr = cumMeanDailyT_Mar + cumMeanDailyT_Apr,
    cumMeanDailyT_FebMarApr = cumMeanDailyT_Feb + cumMeanDailyT_Mar + cumMeanDailyT_Apr,
    cumMeanDailyT_FebMar = cumMeanDailyT_Feb + cumMeanDailyT_Mar,
    cumMeanDailyT_JanFebMar = cumMeanDailyT_Jan + cumMeanDailyT_Feb + cumMeanDailyT_Mar,
    
    cumSnow_MarApr = cumSnow_Mar + cumSnow_Apr,
    cumSnow_FebMarApr = cumSnow_Feb + cumSnow_Mar + cumSnow_Apr,
    cumSnow_FebMar = cumSnow_Feb + cumSnow_Mar,
    cumSnow_JanFebMar = cumSnow_Jan + cumSnow_Feb + cumSnow_Mar,
    
    cumRain_MarApr = cumRain_Mar + cumRain_Apr,
    cumRain_FebMarApr = cumRain_Feb + cumRain_Mar + cumRain_Apr,
    cumRain_FebMar = cumRain_Feb + cumRain_Mar,
    cumRain_JanFebMar = cumRain_Jan + cumRain_Feb + cumRain_Mar,
    
    nDaysMeanAboveZero_MarApr = 61 - (nDaysMeanBelowZero_Mar + nDaysMeanBelowZero_Apr),
    nDaysMeanAboveZero_Mar = 31 - nDaysMeanBelowZero_Mar,
    nDaysMeanAboveZero_FebMar = 59 - (nDaysMeanBelowZero_Feb + nDaysMeanBelowZero_Mar),
    
    nDaysMinAboveZero_MarApr = 61 - (nDaysMinBelowZero_Mar + nDaysMinBelowZero_Apr),
    nDaysMinAboveZero_Mar = 31 - nDaysMinBelowZero_Mar,
    nDaysMinAboveZero_FebMar = 59 - (nDaysMinBelowZero_Feb + nDaysMinBelowZero_Mar),
    
    # Year = water_year + 1
  ) %>% #year = the year of ice-off
  
  ungroup() %>%
  
  #join MohonkIce dataframe with ice on/off days
  left_join(., MohonkIce, by = c("water_year"="Year")) %>%
  rename(Year=water_year) %>% #Need to rename water_year as Year
  
  
  # get rid of unnecessary columns
  select(-IceOutDayofYear_fed,-IceInDate, -IceOutDate, -IceInDate) %>%
  
  
  #moves select response variables to
  #front of dataframe, then lists the rest of predictor variables.
  select(
    Year,
    # water_year,
    IceInDayofYear,
    IceOutDayofYear,
    LengthOfIceCover_days,
    IceInDayofYear_fed,
    everything()
  )


#Calculated some snow predictors in a separate dataframe, then joined with MohonkDailyWeather_monthly
#Note that there isn't good snow depth data prior to the 1950s
SnowPredictors <- MohonkDailyWeatherFull %>%
  mutate(
    year = year(Date),
    month = month(Date),
    month_name = month(Date, label = TRUE),
    water_year = dataRetrieval::calcWaterYear(Date) #IAO - 2022-07-08 - I don't trust myself anymore so just using a function
    # water_year = ifelse(month %in% c("1", "2", "3", "4"),
    #                     year - 1,
    #                     year)
  ) %>%
  group_by(water_year) %>%
  dplyr::summarize(
    maxSnowDepth_mm = max(SnowDepth_mm, na.rm = FALSE),
    datemaxSnowDepth = Date[which(SnowDepth_mm == max(SnowDepth_mm, na.rm =
                                                        FALSE))],
    maxSnowDepthDOY_fed = hydro.day(datemaxSnowDepth)
  ) %>%
  select(-datemaxSnowDepth) %>%
  filter(row_number() == 1) #There are a handful of years where the summary function calculates duplicate values for some reason. This line selects just the 1st observation


# colnames <-
#   (intersect(
#     colnames(MohonkDailyWeather_monthly),
#     colnames(SnowPredictors)
#   )) #identify common columns between data.tables
MohonkIceWeather <-
  left_join(MohonkDailyWeather_monthly, SnowPredictors, by = c("Year"="water_year"))

# colnames <-
#   (intersect(
#     colnames(MohonkDailyWeather_monthly),
#     colnames(ENSO_summary)
#   )) #identify common columns between data.tables
MohonkIceWeather <-
  left_join(MohonkIceWeather, ENSO_summary,  by = c("Year"="water_year"))

# colnames <-
#   (intersect(colnames(MohonkDailyWeather_monthly),  colnames(NAO_summary))) #identify common columns between data.tables
MohonkIceWeather <-
  left_join(MohonkIceWeather, NAO_summary,  by = c("Year"="water_year"))

colnames <-
  (intersect(
    colnames(MohonkDailyWeather_monthly),
    colnames(NOAA_anomaly)
  )) #identify common columns between data.tables
MohonkIceWeather <-
  left_join(MohonkIceWeather, NOAA_anomaly, by = colnames)

rm(MohonkDailyWeather_monthly, SnowPredictors)


## Add months and seasons to MohonkDailyWeatherFull
MohonkDailyWeatherFull<-MohonkDailyWeatherFull %>% 
  mutate(Month=month(Date),
         Year=year(Date)) %>%
  mutate(
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
        # water_year = ifelse(
    #   season %in% c("winter", "spring") &
    #     Month %in% c("10", "11", "12", "1", "2", "3", "4"),
    #   Year - 1,
    #   Year
    
  )

