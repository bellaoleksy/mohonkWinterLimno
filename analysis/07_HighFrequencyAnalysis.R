#Script 07-HighFrequencyAnalysis.R####
#Explore the high frequency winter water temperatures and associated metrics for Mohonk Lake
#Created 18Jul2024, by David Richardson (DCR)

#Run functions####
source("script/00_libraries.R")
source("script/01_functions.R")

#Read in HF data####
#Get rid of error codes
SensorData <-
  read_csv("data/MohonkSensor-AllData-01Apr2016to06Aug2018-ModifiedHeaders.csv") %>%
  mutate(
    Temp_0m = ifelse(Temp_0m < (-100), NA, Temp_0m),
    Temp_1m = ifelse(Temp_1m < (-100), NA, Temp_1m),
    Temp_2m = ifelse(Temp_2m < (-100), NA, Temp_2m),
    Temp_3m = ifelse(Temp_3m < (-100), NA, Temp_3m),
    Temp_4m = ifelse(Temp_4m < (-100), NA, Temp_4m),
    Temp_5m = ifelse(Temp_5m < (-100), NA, Temp_5m),
    Temp_6m = ifelse(Temp_6m < (-100), NA, Temp_6m),
    Temp_7m = ifelse(Temp_7m < (-100), NA, Temp_7m),
    Temp_8m = ifelse(Temp_8m < (-100), NA, Temp_8m),
    Temp_9m = ifelse(Temp_9m < (-100), NA, Temp_9m)
  )
#Read in ice data####
IceOnIceOff <-
  read_csv("data/MohonkLake-IceOnIceOff-1932-2023.csv") %>%
  mutate(
    IceIn_1_date = mdy(ICEIN_1),
    IceOut_1_date = mdy(ICEOUT_1),
    IceIn_2_date = mdy(ICEOUT_2),
    IceOut_2_date = mdy(ICEOUT_1),
    IceIn_3_date = mdy(ICEIN_3),
    IceOut_3_date = mdy(ICEOUT_3)
  )

#Read in ice data extraction####
IceDataExtraction <- read_csv("data/MohonkIceDataExtraction.csv") %>%
  mutate(Date_format = mdy(Date)) %>%
  mutate(year = year(Date_format))


#Read in mesonet data####
#Identify all the individual .csv files
Mesonet_files <-
  list.files("data/MesonetWeatherData", pattern = "*.csv")
#initialize empty list
list_data <- list()

#Loop through all files and upload each months data
for (fileIndex in 1:length(Mesonet_files)) {
  list_data[[fileIndex]] <-
    read_csv(paste0("data/MesonetWeatherData/", Mesonet_files[fileIndex]))
}

#Bind all the months together
MesonetData <- do.call(bind_rows, list_data)

#Rename some columns in mesonet data
names(MesonetData)

#rename the column headers to clean up
MesonetData <- MesonetData %>%
  setNames(gsub("\\^", "", gsub("\\/", "p", gsub(
    "\\[|\\]", "", sub(" ", "_", names(MesonetData))
  )))) %>% #clean up the column headers, remove space, brackets and slashes
  mutate(DateTime = ymd_hms(time_end)) #Format a dateTIme variable

#Read in Bathymetry####
MohonkBathy <-
  read.csv('data/Mohonk_area_bathymetry_csv2.csv', fill = TRUE)


# . -----------------------------------------------------------------------

#Data munging for under-ice HF data####
#Calculate metrics for the under ice data
#This code takes a bit of time to run so I will comment it out
# #Create additional columns of data that indicate the stability and thermocline depth at that time
# #Vector of the depths
# depths.v <- seq(0, 9, 1)
#
# #Calculate the thermocline depth and stability for all days
# #*Filter the dataset for just winter
# SensorData_derived<-SensorData%>%
#                     filter(DateTime>=as.POSIXct("2016-11-20 00:00:00 EST"))%>%
#                     filter(DateTime<=as.POSIXct("2018-04-24 00:00:00 EST"))%>%
#                     filter(DateTime<=as.POSIXct("2017-04-24 00:00:00 EST")|DateTime>=as.POSIXct("2017-11-20 00:00:00 EST"))
# #*Check out the filtered data
# #ggplot(data=SensorData_derived%>%pivot_longer(-1),aes(x=DateTime,y=value,color=name))+geom_line()+
#   #geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
#   #geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))
#
# #Set up place holders for the new derived variables
# SensorData_derived$thermoclineDepth_m_thresh0.1<-NA
# SensorData_derived$thermoclineDepth_m_maxdiff<-NA
# SensorData_derived$stability_Jperm2<-NA
# SensorData_derived$buoyancyfrequency_1_s2<-NA
#
# #Code to run thermocline depth and stability calcs for each time point
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

#Calculate some other metrics here
#*Top vs. bottom (Pierson et al. indicates inverse strat at top is 0.1C below that of bottom)
#SensorData_derived<-SensorData_derived%>%mutate(temperatureDifferenceTop0mvsBottom9m=Temp_0m-Temp_9m)
# write_csv(SensorData_derived,file="output/MohkWinterLimno_SensorData_derived.csv")


#Read in derived sensor data####
SensorData_derived <-
  read_csv("output/MohkWinterLimno_SensorData_derived.csv")

#Expand out each of the parts of the data frame to make sure we have all 15 minute time steps accounted for
SensorData_derived2016 <-
  SensorData_derived %>% filter(DateTime <= as.POSIXct("2017-07-01 00:00:00 EST"))
max(SensorData_derived2016$DateTime)
min(SensorData_derived2016$DateTime)
SensorData_derived2016 <-
  left_join(tibble(DateTime = seq(
    min(SensorData_derived2016$DateTime),
    max(SensorData_derived2016$DateTime) + 37 * 24 * 60 * 60,
    by = "15 mins"
  )), SensorData_derived2016, by = "DateTime")
#Expand out the 2016 data set to go into April past the ice in to include other variables during that time

SensorData_derived2017 <-
  SensorData_derived %>% filter(DateTime > as.POSIXct("2017-07-01 00:00:00 EST"))
max(SensorData_derived2017$DateTime)
min(SensorData_derived2017$DateTime)
SensorData_derived2017 <-
  left_join(tibble(DateTime = seq(
    min(SensorData_derived2017$DateTime),
    max(SensorData_derived2017$DateTime),
    by = "15 mins"
  )), SensorData_derived2017, by = "DateTime")

#Bind them back together
SensorData_derivedFill <-
  bind_rows(SensorData_derived2016, SensorData_derived2017)

#Merge with weather data -
#Left join with sensorData_derived
SensorData_derivedFill <-
  left_join(SensorData_derivedFill, MesonetData, by = "DateTime")
#linearly interpolate relevant variables from hourly to 15 minute data to match the sensors
SensorData_derivedFill <-
  SensorData_derivedFill %>% mutate(
    temp_2m_max_degC = na.approx(temp_2m_max_degC, na.rm = FALSE, maxgap = 4),
    temp_2m_min_degC = na.approx(temp_2m_min_degC, na.rm =
                                   FALSE, maxgap = 4),
    temp_2m_avg_degC = na.approx(temp_2m_avg_degC, na.rm =
                                   FALSE, maxgap = 4),
    relative_humidity_max_percent =
      na.approx(
        relative_humidity_max_percent,
        na.rm = FALSE,
        maxgap = 4
      ),
    relative_humidity_min_percent =
      na.approx(
        relative_humidity_min_percent,
        na.rm = FALSE,
        maxgap = 4
      ),
    relative_humidity_avg_percent =
      na.approx(
        relative_humidity_avg_percent,
        na.rm = FALSE,
        maxgap = 4
      ),
    precip_incremental_mm = na.approx(precip_incremental_mm, na.rm =
                                        FALSE, maxgap = 4),
    precip_local_mm = na.approx(precip_local_mm, na.rm =
                                  FALSE, maxgap = 4),
    wind_speed_prop_avg_mps = na.approx(wind_speed_prop_avg_mps, na.rm =
                                          FALSE, maxgap = 4),
    wind_speed_prop_max_mps = na.approx(wind_speed_prop_max_mps, na.rm =
                                          FALSE, maxgap = 4),
    wind_direction_prop_avg_degrees =
      na.approx(
        wind_direction_prop_avg_degrees,
        na.rm = FALSE,
        maxgap = 4
      ),
    wind_speed_sonic_avg_mps = na.approx(
      wind_speed_sonic_avg_mps,
      na.rm = FALSE,
      maxgap = 4
    ),
    wind_speed_sonic_max_mps = na.approx(
      wind_speed_sonic_max_mps,
      na.rm = FALSE,
      maxgap = 4
    ),
    wind_direction_sonic_avg_degrees =
      na.approx(
        wind_direction_sonic_avg_degrees,
        na.rm = FALSE,
        maxgap = 4
      ),
    solar_insolation_avg_Wpm2 = na.approx(
      solar_insolation_avg_Wpm2,
      na.rm = FALSE,
      maxgap = 4
    ),
    station_pressure_max_mbar = na.approx(
      station_pressure_max_mbar,
      na.rm = FALSE,
      maxgap = 4
    ),
    station_pressure_min_mbar = na.approx(
      station_pressure_min_mbar,
      na.rm = FALSE,
      maxgap = 4
    ),
    station_pressure_avg_mbar = na.approx(
      station_pressure_avg_mbar,
      na.rm = FALSE,
      maxgap = 4
    ),
    solar_insolation_total_MJpm2 = na.approx(
      solar_insolation_total_MJpm2,
      na.rm = FALSE,
      maxgap = 4
    )
  )

#Merge with percent ice cover
#*Create a dateTime variable that is noon for each day
IceDataExtraction_sub <-
  IceDataExtraction %>% mutate(DateTime = mdy_hms(paste0(Date, " ", "12:00:00"))) %>%
  dplyr::select(DateTime, IceCover_Percent, IceIn, IceOut)
#Merge with Sensor derived
SensorData_derivedFill <-
  left_join(SensorData_derivedFill, IceDataExtraction_sub, by = "DateTime")

#look at some of the differences in the variables
SensorData_derivedFill <-
  SensorData_derivedFill %>% mutate(
    stability_Jperm2_diff = stability_Jperm2 - lag(stability_Jperm2, default =
                                                     NA),
    buoyancyfrequency_1_s2_diff =
      buoyancyfrequency_1_s2 - lag(buoyancyfrequency_1_s2, default = NA)
  )

#
# ####Downscale to daily values across the board
SensorData_derivedFill <- SensorData_derivedFill %>%
  mutate(DayOrNight = ifelse(solar_insolation_avg_Wpm2 <
                               0.5, "night", "day"))

#Day counter
day_counter <- 1
SensorData_derivedFill$day_count <- NA
#Go through all the rows. If it is the first row, then store 1#
#Otherwise if both that day/night value OR the preceeding one is NA, then store NA
#OTherwise, if it is a day to night transition (sunset), increment the day counter and store that counter
#Otherwise just store the counter
for (dateTime_j in 1:nrow(SensorData_derivedFill)) {
  if (dateTime_j == 1) {
    SensorData_derivedFill$day_count[dateTime_j] <- day_counter
  } else if (is.na(SensorData_derivedFill$DayOrNight[dateTime_j]) |
             is.na(SensorData_derivedFill$DayOrNight[dateTime_j - 1])) {
    SensorData_derivedFill$day_count[dateTime_j] <- NA
  } else if (SensorData_derivedFill$DayOrNight[dateTime_j] == "night" &
             SensorData_derivedFill$DayOrNight[dateTime_j - 1] == "day") {
    day_counter <- day_counter + 1
    SensorData_derivedFill$day_count[dateTime_j] <- day_counter
  } else{
    SensorData_derivedFill$day_count[dateTime_j] <- day_counter
  }
  #print(dateTime_j)
}


#Summarize all columns that are numeric for the mean by day, sunset to sunset
daily_numeric <- SensorData_derivedFill %>% group_by(day_count) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
#Get teh maximum for the air temperature
daily_airtempMax <- SensorData_derivedFill %>% group_by(day_count) %>%
  summarise(temp_2m_max_degC_max = max(temp_2m_max_degC, na.rm = TRUE))
#Get the daily sum of the average air temperature
daily_airtempSum <- SensorData_derivedFill %>% group_by(day_count) %>%
  summarise(temp_2m_avg_degC_sum = sum(temp_2m_avg_degC, na.rm = TRUE))

#SUmmarize all date columns for the mean by day, sunset to sunset
daily_date <- SensorData_derivedFill %>% group_by(day_count) %>%
  summarise_if(is.POSIXct, mean, na.rm = TRUE)
#Summarize the daily cv of stability and bf
daily_cv <-
  SensorData_derivedFill %>% group_by(day_count) %>% summarize(
    stability_Jperm2_dailyCV = sd(stability_Jperm2, na.rm = TRUE) / mean(stability_Jperm2, na.rm =
                                                                           TRUE),
    buoyancyfrequency_1_s2_dailyCV =
      sd(buoyancyfrequency_1_s2, na.rm = TRUE) / mean(buoyancyfrequency_1_s2, na.rm =
                                                        TRUE)
  )




#Merge those together for a daily data frame
dailySensorData_derivedFill <-
  left_join(daily_date, daily_numeric, by = "day_count") %>%
  left_join(., daily_airtempMax, by = "day_count") %>%
  left_join(., daily_airtempSum, by = "day_count") %>%
  left_join(., daily_cv, by = "day_count") %>% #add in the daily CV of stability
  dplyr::select(-stability_Jperm2_diff, -buoyancyfrequency_1_s2_diff) #remove the differencing becuase this is the average of the differencing

#Calculate the daily difference of the stability and bf
dailySensorData_derivedFill <-
  dailySensorData_derivedFill %>% mutate(
    stability_Jperm2_diff = stability_Jperm2 - lag(stability_Jperm2, default =
                                                     NA),
    buoyancyfrequency_1_s2_diff = buoyancyfrequency_1_s2 - lag(buoyancyfrequency_1_s2, default =
                                                                 NA)
  )

#Filter out solid ice cover with 100% cover
stability_diff_icecoverBounds <-
  dailySensorData_derivedFill %>% filter(IceCover_Percent == 100) %>% summarize(
    mean_stability_Jperm2_diff = mean(stability_Jperm2_diff, na.rm = TRUE),
    sd_stability_Jperm2_diff =
      sd(stability_Jperm2_diff, na.rm = TRUE)
  ) %>%
  mutate(sd_stability_Jperm2_diff_3x =
           sd_stability_Jperm2_diff * 3)

#Look for the first one in each winter that is bigger than the 3x diff
dailySensorData_derivedFill <-
  dailySensorData_derivedFill %>% mutate(BruesewitzIceOn = ifelse(
    stability_Jperm2_diff >= (
      stability_diff_icecoverBounds$mean_stability_Jperm2_diff + stability_diff_icecoverBounds$sd_stability_Jperm2_diff_3x
    ),
    "Big",
    "Small"
  ))

#Look for inverse stratification based on Woolway:
#where the surface temp < bottom temp - considered as first ice-free layer when ice is present
#specific density difference threshold between surface and bottom waters was exceeded (0.05 to 0.5 kg m-3)
dailySensorData_derivedFill <-
  dailySensorData_derivedFill %>% mutate(
    temperatureDifference1mvs9m = Temp_1m < Temp_9m,
    #true if lake surface is colder than deep
    waterDensityDiff_kgpm3 = water.density(Temp_9m) -
      water.density(Temp_0m),
    waterDensityDiff_kgpm3_threshold =
      ifelse(waterDensityDiff_kgpm3 > 0.05, TRUE, FALSE),
    inverseStratification = ifelse(
      waterDensityDiff_kgpm3_threshold == TRUE &
        temperatureDifference1mvs9m == TRUE,
      "InvStrat",
      "NoStrat"
    ),
    inverseStratification_numeric =
      ifelse(
        waterDensityDiff_kgpm3_threshold == TRUE &
          temperatureDifference1mvs9m == TRUE,
        1,
        NA
      )
  )



#Get out the Bruesewitz Ice On dates
BruesewitzIceIn <- dailySensorData_derivedFill %>%
  filter(BruesewitzIceOn == "Big") %>%
  mutate(year = year(DateTime)) %>%
  dplyr::select(DateTime, BruesewitzIceOn, year) %>%
  filter(year < 2018) %>%
  filter(row_number() == 1 | row_number() == 3) %>%
  mutate(IceIn_1_date_Bruesewitz = date(DateTime)) %>%
  dplyr::select(year, IceIn_1_date_Bruesewitz)

#Pierson method
#The nex sens t-nodes have a temperature resolution of  ±0.075 °C temperature accuracy so we can use the lower
PiersonIceIn <-
  dailySensorData_derivedFill %>% mutate(
    limit0.1 = ifelse(
      temperatureDifferenceTop0mvsBottom9m < (-0.1),
      "Low0.1",
      "High0.1"
    ),
    limit0.4 = ifelse(
      temperatureDifferenceTop0mvsBottom9m < (-0.4),
      "Low0.4",
      "High0.4"
    )
  ) %>%
  mutate(
    date = date(DateTime),
    year = year(DateTime),
    month = month(DateTime)
  ) %>%
  filter(month == 12) %>%
  dplyr::select(date, year, limit0.1) %>%
  filter(limit0.1 == "Low0.1") %>%
  group_by(year) %>%
  filter(year == 2016 | year == 2017) %>% print(n = Inf) %>%
  filter(row_number() == 1) %>%
  rename(IceIn_1_date_Pierson = date) %>%
  dplyr::select(year, IceIn_1_date_Pierson)

#Pierson method for spring melt
PiersonIceOut <-
  dailySensorData_derivedFill %>% mutate(limit0.1 = ifelse(
    temperatureDifferenceTop0mvsBottom9m < (-0.1),
    "Low0.1",
    "High0.1"
  )) %>%
  mutate(
    date = date(DateTime),
    year = year(DateTime),
    month = month(DateTime)
  ) %>%
  filter(month == 2 | month == 3 | month == 4) %>%
  dplyr::select(date, year, limit0.1) %>%
  #print(n=Inf)%>%
  filter(limit0.1 == "High0.1") %>%
  group_by(year) %>%
  #print(n=Inf)%>%
  filter(row_number() == 1) %>%
  rename(IceOut_1_date_Pierson = date) %>%
  ungroup() %>%
  dplyr::select(IceOut_1_date_Pierson)


#Pull out only the high frequency years
IceOnIceOff_hfYears <- IceOnIceOff %>%
  dplyr::select(IceIn_1_date, IceOut_1_date) %>% #keep the dates
  mutate(year = year(IceIn_1_date - 25)) %>% #find the year, minus 25 to get the water year because there is a january ice on date
  filter(year == 2016 | year == 2017) #pull the high frequency winters

#Add the Bruesewitz to the data frame
IceOnIceOff_hfYears <-
  left_join(IceOnIceOff_hfYears, BruesewitzIceIn, by = "year") %>%
  mutate(IceOut_1_date_Bruesewitz = NA) %>% #unable to ID from the Bruesewitz method as in neither winter are there were not first of two consecutive days with rates of change below the lower bound, indicating a shift to lower stability following ice-off.
  left_join(., PiersonIceIn, by = "year") %>%
  bind_cols(., PiersonIceOut)



# . -----------------------------------------------------------------------


#FIGURE 6 High Frequency Data #####
#*provides a function for a ramped color by calling colFun(10)
colFun <- colorRampPalette(c("light blue", "dark blue"))

#*Pull data for only Winter2016
SensorData_derivedFill_2016 <-
  SensorData_derivedFill %>% filter(
    DateTime >= as.POSIXct("2016-11-24 19:00:00 EST") &
      DateTime <= as.POSIXct("2017-04-19 20:00:00 EDT")
  )
#*Set limits for the graph
lims_2016 <-
  as.POSIXct(strptime(c("2016-11-24 19:00", "2017-04-19 20:00"),
                      format = "%Y-%m-%d %H:%M"))

#*Plot W2016
gg.hf2016 <-
  ggplot(
    data = SensorData_derivedFill_2016 %>% dplyr::select(DateTime:Temp_9m) %>%
      pivot_longer(-1),
    aes(x = DateTime, y = value, color = name)
  ) + geom_line() +
  scale_x_datetime(limits = lims_2016) +
  scale_color_manual(values = colFun(10)) +
  geom_vline(data = IceOnIceOff_hfYears,
             aes(xintercept = as.POSIXct(IceIn_1_date)),
             size = 1.0) +
  geom_vline(data = IceOnIceOff_hfYears,
             aes(xintercept = as.POSIXct(IceOut_1_date)),
             size = 1.0) +
  geom_vline(
    data = IceOnIceOff_hfYears,
    aes(xintercept = as.POSIXct(IceIn_1_date_Pierson)),
    color = "darkgrey",
    linetype = 2,
    linewidth = 0.7
  ) +
  geom_vline(
    data = IceOnIceOff_hfYears,
    aes(xintercept = as.POSIXct(IceOut_1_date_Pierson)),
    color = "darkgrey",
    linetype = 2,
    linewidth = 0.7
  ) +
  geom_point(
    data = SensorData_derivedFill,
    aes(x = DateTime, y = IceCover_Percent / 15),
    color = "black",
    shape = 21,
    fill = "white",
    alpha = 0.5,
    size = 1
  ) +
  scale_y_continuous(
    limits = c(-0.1, 8.2),
    expand = c(0, 0),
    breaks = c(0, 2, 4, 6, 8),
    sec.axis = sec_axis( ~ . * 15, name = "IceCover (%)", breaks = c(0, 25, 50, 75, 100))
  ) +
  ylab(bquote(Water ~ Temp. ~ (degree * C))) +
  xlab("Date") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



#*Pull data for only Winter2017
SensorData_derivedFill_2017 <-
  SensorData_derivedFill %>% filter(
    DateTime >= as.POSIXct("2017-11-24 19:00:00 EST") &
      DateTime <= as.POSIXct("2018-04-19 20:00:00 EDT")
  )
#*Set limits for the graph
lims_2017 <-
  as.POSIXct(strptime(c("2017-11-24 19:00", "2018-04-19 20:00"),
                      format = "%Y-%m-%d %H:%M"))
#Name the labels
legend_labels <- c("0m", "1m", "2m", "3m", "4m", "5m", "6m", "7m", "8m", "9m")

#*Plot W2017
gg.hf2017 <-
  ggplot(
    data = SensorData_derivedFill_2017 %>% dplyr::select(DateTime:Temp_9m) %>%
      pivot_longer(-1),
    aes(x = DateTime, y = value, color = name)
  ) + geom_line() +
  scale_x_datetime(limits = lims_2017) +
  scale_color_manual(values = colFun(10), labels = legend_labels) +
  labs(color = bquote("Water\ntemperature\ndepth")) +
  geom_vline(data = IceOnIceOff_hfYears,
             aes(xintercept = as.POSIXct(IceIn_1_date)),
             size = 1.0) +
  geom_vline(data = IceOnIceOff_hfYears,
             aes(xintercept = as.POSIXct(IceOut_1_date)),
             size = 1.0) +
  geom_vline(
    data = IceOnIceOff_hfYears,
    aes(xintercept = as.POSIXct(IceIn_1_date_Pierson)),
    color = "darkgrey",
    linetype = 2,
    size = 0.7
  ) +
  geom_vline(
    data = IceOnIceOff_hfYears,
    aes(xintercept = as.POSIXct(IceOut_1_date_Pierson)),
    color = "darkgrey",
    linetype = 2,
    size = 0.7
  ) +
  geom_point(
    data = SensorData_derivedFill,
    aes(x = DateTime, y = IceCover_Percent / 15),
    color = "black",
    shape = 21,
    fill = "white",
    alpha = 0.5,
    size = 1
  ) +
  scale_y_continuous(
    limits = c(-0.1, 8.2),
    expand = c(0, 0),
    breaks = c(0, 2, 4, 6, 8),
    sec.axis = sec_axis(~ . * 15, name = "IceCover (%)", breaks = c(0, 25, 50, 75, 100))
  ) +
  ylab(bquote(Water ~ Temp. ~ (degree * C))) +
  xlab("Date") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(linewidth = 3)))

#Work on the legend alone
leg <- get_legend(gg.hf2017) #extract the legend using ggpubr
legend <- as_ggplot(leg)

#Stitch teh panels together
panel.size <- 10
List <- list(
  gg.hf2016 +
    coord_cartesian(ylim = c(-0.1, 8.2), clip = "off") +
    annotate(
      "text",
      x = lims_2016[1],
      y = 8.2,
      label = "(a) 2016-2017",
      hjust = 0.2,
      vjust = -0.9
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none",
      plot.margin = margin(2, 1, 1, 1, 'lines')
    ) +
    xlab(""),
  gg.hf2017 +
    coord_cartesian(ylim = c(-0.1, 8.2), clip = "off") +
    annotate(
      "text",
      x = lims_2017[1],
      y = 8.2,
      label = "(b) 2017-2018",
      hjust = 0.2,
      vjust = -0.9
    ) +
    theme(
      legend.position = "none",
      plot.margin = margin(2, 1, 1, 1, 'lines')
    )
)


#Plot them using patchwork
(gg.hf2columns <-
    wrap_plots(List, ncol = 1, nrow = 2) &
    theme(plot.margin = unit(c(10, 3, 3, 3), "pt")))

#Put the two columsn with the legend
List2 <- list(gg.hf2columns, legend)

(
  gg.hf2columnsLegend <-
    wrap_plots(
      List2,
      ncol = 2,
      nrow = 1,
      widths = c(0.8, 0.2)
    ) & theme(plot.margin = unit(c(10, 3, 3, 3), "pt"))
)

#Could do a 2x1 with width 6, height = 4
ggsave(
  paste("figures/MS/Figure6.HighFrequencyUnderwater.jpg", sep = ""),
  plot = gg.hf2columnsLegend,
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)

# . -----------------------------------------------------------------------
# . -----------------------------------------------------------------------
# . -----------------------------------------------------------------------

#Segmented regressions of stability and temperature difference####
#*Subset for 2016####
segmentedDF_2016<-SensorData_derivedFill%>%mutate(ma_temp_avg_degC=forecast::ma(temp_2m_avg_degC,order=96*3,centre=TRUE))%>%filter(DateTime<=as.POSIXct("2016-12-19 00:00:00 EST")&DateTime>=as.POSIXct("2016-12-01 00:00:00 EST"))%>%
  mutate(row_name=row_number())
#*Segmented regression for temperature difference####
lm.tempDiff.2016<-lm(temperatureDifferenceTop0mvsBottom9m~row_name,data=segmentedDF_2016)

#segmented.mod.tempDiff.2016<-segmented(lm.tempDiff.2016,seg.Z= ~row_name,psi=c(1100,1300))
#Store results from segmented regressions with up to 7 breakpoints#
segmented2016_results_tempDiff<-tibble(breakpoints=rep(NA,7), r.squared=rep(NA,7),sigma=rep(NA,7),bp=rep(NA,7),date_bp=rep(NA,7),optimal=rep(NA,7))

#Go through 1 to 7 breakpoints, find the optimal segmented regression for each####
#Extract the number of breakpoints, r.squared, sigma, breakpoint that precedes the steepest slope, and the dateTime of the breakpoint####
for(seg.index in 1:7){
segmented.mod.tempDiff.2016<-segmented(lm.tempDiff.2016,seg.Z= ~row_name,psi=NA,control=seg.control(display=FALSE,K=seg.index,quant=TRUE))
segmented2016_results_tempDiff$breakpoints[seg.index]<-seg.index
segmented2016_results_tempDiff$r.squared[seg.index]<-summary(segmented.mod.tempDiff.2016)$adj.r.squared
segmented2016_results_tempDiff$sigma[seg.index]<-summary(segmented.mod.tempDiff.2016)$sigma

#find the breakpoint row_numb preceding the steepest slope (abs)####
bp<-round(summary(segmented.mod.tempDiff.2016)$psi[which.min(segmented::slope(segmented.mod.tempDiff.2016)$row_name[,1])-1 ,"Est."],0)
segmented2016_results_tempDiff$bp[seg.index]<-bp
#Find the date of the breakpoint####
segmented2016_results_tempDiff$date_bp[seg.index]<-as.character(segmentedDF_2016[bp,"DateTime"]%>%pull())

#Store the first one as the optimal model, if not check the sigma. If sigma goes down, store that as the optimal model####
if(seg.index==1){
  segmented.OptimalMod.tempDiff.2016<-segmented.mod.tempDiff.2016
  segmented2016_results_tempDiff$optimal[seg.index]<-TRUE
  }else if(segmented2016_results_tempDiff$sigma[seg.index]<segmented2016_results_tempDiff$sigma[seg.index-1]){
  segmented.OptimalMod.tempDiff.2016<-segmented.mod.tempDiff.2016
  segmented2016_results_tempDiff$optimal<-NA
  segmented2016_results_tempDiff$optimal[seg.index]<-TRUE
  }else{}

} #End of for loop

#Look at the optimal model####
summary(segmented.OptimalMod.tempDiff.2016)
#Gets the fits for the broken line
#broken.line(segmented.mod.tempDiff.2016)$fit
#Plot the optimal model
plot(temperatureDifferenceTop0mvsBottom9m~row_name,data=segmentedDF_2016, pch=16)
plot(segmented.OptimalMod.tempDiff.2016, add=T)
abline(v=round(summary(segmented.OptimalMod.tempDiff.2016)$psi[which.min(segmented::slope(segmented.OptimalMod.tempDiff.2016)$row_name[,1])-1 ,"Est."],0))

#*Segmented regression for stability####
lm.stability.2016<-lm(stability_Jperm2~row_name,data=segmentedDF_2016)

#segmented.mod.tempDiff.2016<-segmented(lm.tempDiff.2016,seg.Z= ~row_name,psi=c(1100,1300))
#Store results from segmented regressions with up to 7 breakpoints#
segmented2016_results_stability<-tibble(breakpoints=rep(NA,7), r.squared=rep(NA,7),sigma=rep(NA,7),bp=rep(NA,7),date_bp=rep(NA,7),optimal=rep(NA,7))

#Go through 1 to 7 breakpoints, find the optimal segmented regression for each####
#Extract the number of breakpoints, r.squared, sigma, breakpoint that precedes the steepest slope, and the dateTime of the breakpoint####
for(seg.index in 1:7){
  segmented.mod.stability.2016<-segmented(lm.stability.2016,seg.Z= ~row_name,psi=NA,control=seg.control(display=FALSE,K=seg.index,quant=TRUE))
  segmented2016_results_stability$breakpoints[seg.index]<-seg.index
  segmented2016_results_stability$r.squared[seg.index]<-summary(segmented.mod.stability.2016)$adj.r.squared
  segmented2016_results_stability$sigma[seg.index]<-summary(segmented.mod.stability.2016)$sigma
  
  #find the breakpoint row_numb preceding the steepest slope (abs)####
  bp<-round(summary(segmented.mod.stability.2016)$psi[which.max(segmented::slope(segmented.mod.stability.2016)$row_name[,1])-1 ,"Est."],0)
  segmented2016_results_stability$bp[seg.index]<-bp
  #Find the date of the breakpoint####
  segmented2016_results_stability$date_bp[seg.index]<-as.character(segmentedDF_2016[bp,"DateTime"]%>%pull())
  
  #Store the first one as the optimal model, if not check the sigma. If sigma goes down, store that as the optimal model####
  if(seg.index==1){
    segmented.OptimalMod.stability.2016<-segmented.mod.stability.2016
    segmented2016_results_stability$optimal[seg.index]<-TRUE
  }else if(segmented2016_results_stability$sigma[seg.index]<segmented2016_results_stability$sigma[seg.index-1]){
    segmented.OptimalMod.stability.2016<-segmented.mod.stability.2016 
    segmented2016_results_stability$optimal<-NA
    segmented2016_results_stability$optimal[seg.index]<-TRUE
  }else{}
  
} #End of for loop

#Look at the optimal model####
summary(segmented.OptimalMod.stability.2016)
#Gets the fits for the broken line
#broken.line(segmented.mod.tempDiff.2016)$fit
#Plot the optimal model
plot(stability_Jperm2~row_name,data=segmentedDF_2016, pch=16)
plot(segmented.OptimalMod.stability.2016, add=T)
abline(v=round(summary(segmented.OptimalMod.stability.2016)$psi[which.max(segmented::slope(segmented.OptimalMod.stability.2016)$row_name[,1])-1 ,"Est."],0))


###################################################
#Segmented regressions of stability and temperature difference####
#*Subset for 2017####
segmentedDF_2017<-SensorData_derivedFill%>%mutate(ma_temp_avg_degC=forecast::ma(temp_2m_avg_degC,order=96*3,centre=TRUE))%>%filter(DateTime<=as.POSIXct("2017-12-19 00:00:00 EST")&DateTime>=as.POSIXct("2017-12-01 00:00:00 EST"))%>%
  mutate(row_name=row_number())
#*Segmented regression for temperature difference####
lm.tempDiff.2017<-lm(temperatureDifferenceTop0mvsBottom9m~row_name,data=segmentedDF_2017)

#segmented.mod.tempDiff.2017<-segmented(lm.tempDiff.2017,seg.Z= ~row_name,psi=c(1100,1300))
#Store results from segmented regressions with up to 7 breakpoints#
segmented2017_results_tempDiff<-tibble(breakpoints=rep(NA,7), r.squared=rep(NA,7),sigma=rep(NA,7),bp=rep(NA,7),date_bp=rep(NA,7),optimal=rep(NA,7))

#Go through 1 to 7 breakpoints, find the optimal segmented regression for each####
#Extract the number of breakpoints, r.squared, sigma, breakpoint that precedes the steepest slope, and the dateTime of the breakpoint####
for(seg.index in 1:7){
  segmented.mod.tempDiff.2017<-segmented(lm.tempDiff.2017,seg.Z= ~row_name,psi=NA,control=seg.control(display=FALSE,K=seg.index,quant=TRUE))
  segmented2017_results_tempDiff$breakpoints[seg.index]<-seg.index
  segmented2017_results_tempDiff$r.squared[seg.index]<-summary(segmented.mod.tempDiff.2017)$adj.r.squared
  segmented2017_results_tempDiff$sigma[seg.index]<-summary(segmented.mod.tempDiff.2017)$sigma
  
  #find the breakpoint row_numb preceding the steepest slope (abs)####
  bp<-round(summary(segmented.mod.tempDiff.2017)$psi[which.min(segmented::slope(segmented.mod.tempDiff.2017)$row_name[,1])-1 ,"Est."],0)
  segmented2017_results_tempDiff$bp[seg.index]<-bp
  #Find the date of the breakpoint####
  segmented2017_results_tempDiff$date_bp[seg.index]<-as.character(segmentedDF_2017[bp,"DateTime"]%>%pull())
  
  #Store the first one as the optimal model, if not check the sigma. If sigma goes down, store that as the optimal model####
  if(seg.index==1){
    segmented.OptimalMod.tempDiff.2017<-segmented.mod.tempDiff.2017
    segmented2017_results_tempDiff$optimal[seg.index]<-TRUE
  }else if(segmented2017_results_tempDiff$sigma[seg.index]<segmented2017_results_tempDiff$sigma[seg.index-1]){
    segmented.OptimalMod.tempDiff.2017<-segmented.mod.tempDiff.2017
    segmented2017_results_tempDiff$optimal<-NA
    segmented2017_results_tempDiff$optimal[seg.index]<-TRUE
  }else{}
  
} #End of for loop

#Look at the optimal model####
summary(segmented.OptimalMod.tempDiff.2017)
#Gets the fits for the broken line
#broken.line(segmented.mod.tempDiff.2017)$fit
#Plot the optimal model
plot(temperatureDifferenceTop0mvsBottom9m~row_name,data=segmentedDF_2017, pch=16)
plot(segmented.OptimalMod.tempDiff.2017, add=T)
abline(v=round(summary(segmented.OptimalMod.tempDiff.2017)$psi[which.min(segmented::slope(segmented.OptimalMod.tempDiff.2017)$row_name[,1])-1 ,"Est."],0))

#*Segmented regression for stability####
lm.stability.2017<-lm(stability_Jperm2~row_name,data=segmentedDF_2017)

#segmented.mod.tempDiff.2017<-segmented(lm.tempDiff.2017,seg.Z= ~row_name,psi=c(1100,1300))
#Store results from segmented regressions with up to 7 breakpoints#
segmented2017_results_stability<-tibble(breakpoints=rep(NA,7), r.squared=rep(NA,7),sigma=rep(NA,7),bp=rep(NA,7),date_bp=rep(NA,7),optimal=rep(NA,7))

#Go through 1 to 7 breakpoints, find the optimal segmented regression for each####
#Extract the number of breakpoints, r.squared, sigma, breakpoint that precedes the steepest slope, and the dateTime of the breakpoint####
for(seg.index in 1:7){
  segmented.mod.stability.2017<-segmented(lm.stability.2017,seg.Z= ~row_name,psi=NA,control=seg.control(display=FALSE,K=seg.index,quant=TRUE))
  segmented2017_results_stability$breakpoints[seg.index]<-seg.index
  segmented2017_results_stability$r.squared[seg.index]<-summary(segmented.mod.stability.2017)$adj.r.squared
  segmented2017_results_stability$sigma[seg.index]<-summary(segmented.mod.stability.2017)$sigma
  
  #find the breakpoint row_numb preceding the steepest slope (abs)####
  bp<-round(summary(segmented.mod.stability.2017)$psi[which.max(segmented::slope(segmented.mod.stability.2017)$row_name[,1])-1 ,"Est."],0)
  segmented2017_results_stability$bp[seg.index]<-bp
  #Find the date of the breakpoint####
  segmented2017_results_stability$date_bp[seg.index]<-as.character(segmentedDF_2017[bp,"DateTime"]%>%pull())
  
  #Store the first one as the optimal model, if not check the sigma. If sigma goes down, store that as the optimal model####
  if(seg.index==1){
    segmented.OptimalMod.stability.2017<-segmented.mod.stability.2017
    segmented2017_results_stability$optimal[seg.index]<-TRUE
  }else if(segmented2017_results_stability$sigma[seg.index]<segmented2017_results_stability$sigma[seg.index-1]){
    segmented.OptimalMod.stability.2017<-segmented.mod.stability.2017
    segmented2017_results_stability$optimal<-NA
    segmented2017_results_stability$optimal[seg.index]<-TRUE
  }else{}
  
} #End of for loop

#Look at the optimal model####
summary(segmented.OptimalMod.stability.2017)
#Gets the fits for the broken line
#broken.line(segmented.mod.tempDiff.2017)$fit
#Plot the optimal model
plot(stability_Jperm2~row_name,data=segmentedDF_2017, pch=16)
plot(segmented.OptimalMod.stability.2017, add=T)
abline(v=round(summary(segmented.OptimalMod.stability.2017)$psi[which.max(segmented::slope(segmented.OptimalMod.stability.2017)$row_name[,1])-1 ,"Est."],0))

##############PLOT ICE ON WITH FITS#########################

#global formatting
size_points<-1
fill_points<-alpha("light blue",0.6)
size_regression<-0.6
color_regression<-"darkgrey"
color_bp<-"black"
tempDiff.limits<-c(-3,0.5)
tempDiff.breaks<-c(-3,-2,-1,0)
stability.limits<-c(-0.1,9.75)
stability.breaks<-c(0,3,6,9)
date.breaks2016<-as.POSIXct(strptime(c("2016-12-01 00:00","2016-12-08 00:00","2016-12-15 00:00"),format = "%Y-%m-%d %H:%M"))
date.breaks2016_lots<-seq.POSIXt(from=as.POSIXct(strptime(c("2016-12-01 00:00"),format = "%Y-%m-%d %H:%M")),to=as.POSIXct(strptime(c("2016-12-31 00:00"),format = "%Y-%m-%d %H:%M")),by="1 days")
date.breaks2017<-as.POSIXct(strptime(c("2017-12-01 00:00","2017-12-08 00:00","2017-12-15 00:00","2017-12-22 00:00","2017-12-29 00:00"),format = "%Y-%m-%d %H:%M"))
date.breaks2017_lots<-seq.POSIXt(from=as.POSIXct(strptime(c("2017-12-01 00:00"),format = "%Y-%m-%d %H:%M")),to=as.POSIXct(strptime(c("2017-12-31 00:00"),format = "%Y-%m-%d %H:%M")),by="1 days")
date.labels<-c("01 Dec","08 Dec", "15 Dec", "22 Dec","29 Dec")
date.labels_lots<-c("01 Dec",rep("",6),"08 Dec", rep("",6),"15 Dec",rep("",6) ,"22 Dec",rep("",6),"29 Dec","","")
limsIceOn_2016 <- as.POSIXct(strptime(c("2016-12-01 00:00", "2016-12-30 00:00"), 
                                 format = "%Y-%m-%d %H:%M"))
limsIceOn_2017 <- as.POSIXct(strptime(c("2017-12-01 00:00", "2017-12-30 00:00"), 
                                      format = "%Y-%m-%d %H:%M"))

#*Draw in 2016 tempDiff####
gg.2016temp<-ggplot()+
  geom_rect(aes(xmin=as.POSIXct(strptime(c("2016-12-16 00:00"),format = "%Y-%m-%d %H:%M")), #Add a box for the ice in day
                xmax=as.POSIXct(strptime(c("2016-12-16 23:59"),format = "%Y-%m-%d %H:%M")), 
                ymin=-Inf, 
                ymax=Inf),
            fill=alpha("grey",1))+
  geom_point(data=SensorData_derivedFill, aes(x=DateTime,y=temperatureDifferenceTop0mvsBottom9m),shape=21,color="black",fill=fill_points,size=size_points)+
  geom_line(data=segmentedDF_2016%>%mutate(segmentedFit=broken.line(segmented.mod.tempDiff.2016)$fit),aes(x=DateTime,y=segmentedFit),color=color_regression,size=size_regression)+
  geom_vline(data=segmented2016_results_tempDiff%>%filter(optimal==TRUE),aes(xintercept=ymd_hms(date_bp)),color=color_bp)+
  scale_y_continuous(limits=tempDiff.limits,breaks=tempDiff.breaks)+
  scale_x_datetime(breaks=date.breaks2016_lots,label=date.labels_lots,limits=limsIceOn_2016)+
  ylab(bquote(Temp.~Diff.~(degree*C)))+
  xlab("Date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(color = c("black",rep("light grey",6)))) #This will set the tick marks to black and then light grey for each day in between

#*Draw in 2016 stability####
gg.2016stability<-ggplot()+
  geom_rect(aes(xmin=as.POSIXct(strptime(c("2016-12-16 00:00"),format = "%Y-%m-%d %H:%M")), #Add a box for the ice in day
                xmax=as.POSIXct(strptime(c("2016-12-16 23:59"),format = "%Y-%m-%d %H:%M")), 
                ymin=-Inf, 
                ymax=Inf),
            fill=alpha("grey",1))+
  geom_point(data=SensorData_derivedFill, aes(x=DateTime,y=stability_Jperm2),shape=21,color="black",fill=fill_points,size=size_points)+
  geom_line(data=segmentedDF_2016%>%mutate(segmentedFit=broken.line(segmented.mod.stability.2016)$fit),aes(x=DateTime,y=segmentedFit),color=color_regression,size=size_regression)+
  geom_vline(data=segmented2016_results_stability%>%filter(optimal==TRUE),aes(xintercept=ymd_hms(date_bp)),color=color_bp)+
  scale_y_continuous(limits=stability.limits,breaks=stability.breaks)+
  scale_x_datetime(breaks=date.breaks2016_lots,label=date.labels_lots,limits=limsIceOn_2016)+
  ylab(bquote(Schmidt~stability~(J~m^-2)))+
  xlab("Date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(color = c("black",rep("light grey",6))))

#*Draw in 2017 tempDiff####
gg.2017temp<-ggplot()+
  geom_rect(aes(xmin=as.POSIXct(strptime(c("2017-12-27 00:00"),format = "%Y-%m-%d %H:%M")), #Add a box for the ice in day
                xmax=as.POSIXct(strptime(c("2017-12-27 23:59"),format = "%Y-%m-%d %H:%M")), 
                ymin=-Inf, 
                ymax=Inf),
            fill=alpha("grey",1))+
  geom_point(data=SensorData_derivedFill, aes(x=DateTime,y=temperatureDifferenceTop0mvsBottom9m),shape=21,color="black",fill=fill_points,size=size_points)+
  geom_line(data=segmentedDF_2017%>%mutate(segmentedFit=broken.line(segmented.mod.tempDiff.2017)$fit),aes(x=DateTime,y=segmentedFit),color=color_regression,size=size_regression)+
  geom_vline(data=segmented2017_results_tempDiff%>%filter(optimal==TRUE),aes(xintercept=ymd_hms(date_bp)),color=color_bp)+
  scale_y_continuous(limits=tempDiff.limits,breaks=tempDiff.breaks)+
  scale_x_datetime(breaks=date.breaks2017_lots,label=date.labels_lots,limits=limsIceOn_2017)+
  ylab(bquote(Temp.~Diff.~(degree*C)))+
  xlab("Date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(color = c("black",rep("light grey",6))))

#*Draw in 2017 stability####
gg.2017stability<-ggplot()+
  geom_rect(aes(xmin=as.POSIXct(strptime(c("2017-12-27 00:00"),format = "%Y-%m-%d %H:%M")), #Add a box for the ice in day
                xmax=as.POSIXct(strptime(c("2017-12-27 23:59"),format = "%Y-%m-%d %H:%M")), 
                ymin=-Inf, 
                ymax=Inf),
            fill=alpha("grey",1))+
  geom_point(data=SensorData_derivedFill, aes(x=DateTime,y=stability_Jperm2),shape=21,color="black",fill=fill_points,size=size_points)+
  geom_line(data=segmentedDF_2017%>%mutate(segmentedFit=broken.line(segmented.mod.stability.2017)$fit),aes(x=DateTime,y=segmentedFit),color=color_regression,size=size_regression)+
  geom_vline(data=segmented2017_results_stability%>%filter(optimal==TRUE),aes(xintercept=ymd_hms(date_bp)),color=color_bp)+
  scale_y_continuous(limits=stability.limits,breaks=stability.breaks)+
  scale_x_datetime(breaks=date.breaks2017_lots,label=date.labels_lots,limits=limsIceOn_2017)+
  ylab(bquote(Schmidt~stability~(J~m^-2)))+
  xlab("Date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(color = c("black",rep("light grey",6))))

List<-list(gg.2016temp+
             xlab("")+
             theme(axis.text.x=element_blank(),
                   #axis.ticks.x=element_blank()
                   )+
             annotate("text",x=as.POSIXct(strptime(c("2016-12-30 00:00"),format = "%Y-%m-%d %H:%M")),y=0.34,label="(a)",hjust=0.5,vjust=0.1)+
             annotate("text",x=as.POSIXct(strptime(c("2016-12-01 00:00"),format = "%Y-%m-%d %H:%M")),y=-3,label="2016",hjust=0.1,vjust=0.1),
           gg.2017temp+
             xlab("")+
             ylab("")+
             theme(axis.text.x=element_blank(),
                   #axis.ticks.x=element_blank(),
                   axis.text.y=element_blank(),
                   #axis.ticks.y=element_blank()
                   )+
             annotate("text",x=as.POSIXct(strptime(c("2017-12-30 00:00"),format = "%Y-%m-%d %H:%M")),y=0.34,label="(b)",hjust=0.5,vjust=0.1)+
             annotate("text",x=as.POSIXct(strptime(c("2017-12-01 00:00"),format = "%Y-%m-%d %H:%M")),y=-3,label="2017",hjust=0.1,vjust=0.1),
           gg.2016stability+
             annotate("text",x=as.POSIXct(strptime(c("2016-12-30 00:00"),format = "%Y-%m-%d %H:%M")),y=9.3,label="(c)",hjust=0.5,vjust=0.2)+
             annotate("text",x=as.POSIXct(strptime(c("2016-12-01 00:00"),format = "%Y-%m-%d %H:%M")),y=9.4,label="2016",hjust=0.1,vjust=0.1),
           gg.2017stability+
             ylab("")+
             theme(axis.text.y=element_blank(),
                   #axis.ticks.y=element_blank()
             )+
             annotate("text",x=as.POSIXct(strptime(c("2017-12-30 00:00"),format = "%Y-%m-%d %H:%M")),y=9.3,label="(d)",hjust=0.5,vjust=0.2)+
             annotate("text",x=as.POSIXct(strptime(c("2017-12-01 00:00"),format = "%Y-%m-%d %H:%M")),y=9.4,label="2017",hjust=0.1,vjust=0.1)
           )

#Put the plots on a 2x2 matrix####
(gg.hficeon<-wrap_plots(List,ncol=2,nrow=2)&theme(plot.margin = unit(c(3,3,3,3),"pt")))

#Could do a 2x1 with width 6, height = 4
ggsave(paste("figures/MohonkWinterLimno-FigureX-HighFrequencyIceOnSegmented.jpg",sep=""), plot=gg.hficeon, width=6, height=4,units="in", dpi=300)



########################Look at meteorological predictors of ice on and off#############################################

##Plot temperature graphs with different ice phenology####

#General formats for the plots - others are pulled from above graphs####
airTemp.limits<-c(-18,12)
airTemp.breaks<-c(-15,-10,-5,0,5,10)
windSpeed.limits<-c(0,4.2)
windSpeed.breaks<-c(0,1,2,3,4)
pressure.limits<-c(970,1020)
pressure.breaks<-c(970,980,990,1000,1010,1020)


#plot the 2016 met data####

#Temperature in 2016 with ice phenology####
gg.2016airTemp<-ggplot()+
  geom_hline(yintercept=0)+
  geom_rect(aes(xmin=as.POSIXct(strptime(c("2016-12-16 00:00"),format = "%Y-%m-%d %H:%M")), #Add a box for the ice in day
              xmax=as.POSIXct(strptime(c("2016-12-16 23:59"),format = "%Y-%m-%d %H:%M")), 
              ymin=-Inf, 
              ymax=Inf),
          fill=alpha("grey",1))+
  geom_vline(data=segmented2016_results_stability%>%filter(optimal==TRUE),aes(xintercept=ymd_hms(date_bp)),color=color_bp)+
  geom_point(data=SensorData_derivedFill,aes(x=DateTime,y=temp_2m_avg_degC),shape=21,fill=alpha("wheat",0.5),color=alpha("wheat",0.5),size=0.8)+
  geom_line(data=SensorData_derivedFill%>%mutate(ma_temp_avg_degC=forecast::ma(temp_2m_avg_degC,order=96*3,centre=FALSE)),aes(x=DateTime,y=ma_temp_avg_degC))+ #Calculate a 3 day moving average
  scale_x_datetime(breaks=date.breaks2016_lots,label=date.labels_lots,limits=limsIceOn_2016)+
  scale_y_continuous(limits=airTemp.limits,breaks=airTemp.breaks)+
  ylab(bquote(Air~temp.~(degree*C)))+
  xlab("Date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(color = c("black",rep("light grey",6))))

#Temperature in 2017 with ice phenology####
gg.2017airTemp<-ggplot()+
  geom_hline(yintercept=0)+
  geom_rect(aes(xmin=as.POSIXct(strptime(c("2017-12-27 00:00"),format = "%Y-%m-%d %H:%M")), #Add a box for the ice in day
                xmax=as.POSIXct(strptime(c("2017-12-27 23:59"),format = "%Y-%m-%d %H:%M")), 
                ymin=-Inf, 
                ymax=Inf),
            fill=alpha("grey",1))+
  geom_vline(data=segmented2017_results_stability%>%filter(optimal==TRUE),aes(xintercept=ymd_hms(date_bp)),color=color_bp)+
  geom_point(data=SensorData_derivedFill,aes(x=DateTime,y=temp_2m_avg_degC),shape=21,fill=alpha("wheat",0.5),color=alpha("wheat",0.5),size=0.8)+
  geom_line(data=SensorData_derivedFill%>%mutate(ma_temp_avg_degC=forecast::ma(temp_2m_avg_degC,order=96*3,centre=FALSE)),aes(x=DateTime,y=ma_temp_avg_degC))+ #Calculate a 3 day moving average
  scale_x_datetime(breaks=date.breaks2017_lots,label=date.labels_lots,limits=limsIceOn_2017)+
  scale_y_continuous(limits=airTemp.limits,breaks=airTemp.breaks)+
  ylab(bquote(Air~temp.~(degree*C)))+
  xlab("Date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(color = c("black",rep("light grey",6))))

#Wind in 2016 with ice phenology####
gg.2016windSpeed<-ggplot()+
  geom_hline(yintercept=0)+
  geom_rect(aes(xmin=as.POSIXct(strptime(c("2016-12-16 00:00"),format = "%Y-%m-%d %H:%M")), #Add a box for the ice in day
                xmax=as.POSIXct(strptime(c("2016-12-16 23:59"),format = "%Y-%m-%d %H:%M")), 
                ymin=-Inf, 
                ymax=Inf),
            fill=alpha("grey",1))+
  geom_vline(data=segmented2016_results_stability%>%filter(optimal==TRUE),aes(xintercept=ymd_hms(date_bp)),color=color_bp)+
  geom_point(data=SensorData_derivedFill,aes(x=DateTime,y=wind_speed_sonic_avg_mps),shape=21,fill=alpha("thistle",0.5),color=alpha("thistle",0.5),size=0.8)+
  geom_line(data=SensorData_derivedFill%>%mutate(ma_wind_speed_sonic_avg_mps=forecast::ma(wind_speed_sonic_avg_mps,order=96*3,centre=FALSE)),aes(x=DateTime,y=ma_wind_speed_sonic_avg_mps))+ #Calculate a 3 day moving average
  scale_x_datetime(breaks=date.breaks2016_lots,label=date.labels_lots,limits=limsIceOn_2016)+
  scale_y_continuous(limits=windSpeed.limits,breaks=windSpeed.breaks)+
  ylab(bquote(Wind~speed~(m/s)))+
  xlab("Date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(color = c("black",rep("light grey",6))))

#Wind in 2017 with ice phenology####
gg.2017windSpeed<-ggplot()+
  geom_hline(yintercept=0)+
  geom_rect(aes(xmin=as.POSIXct(strptime(c("2017-12-27 00:00"),format = "%Y-%m-%d %H:%M")), #Add a box for the ice in day
                xmax=as.POSIXct(strptime(c("2017-12-27 23:59"),format = "%Y-%m-%d %H:%M")), 
                ymin=-Inf, 
                ymax=Inf),
            fill=alpha("grey",1))+
  geom_vline(data=segmented2017_results_stability%>%filter(optimal==TRUE),aes(xintercept=ymd_hms(date_bp)),color=color_bp)+
  geom_point(data=SensorData_derivedFill,aes(x=DateTime,y=wind_speed_sonic_avg_mps),shape=21,fill=alpha("thistle",0.5),color=alpha("thistle",0.5),size=0.8)+
  geom_line(data=SensorData_derivedFill%>%mutate(ma_wind_speed_sonic_avg_mps=forecast::ma(wind_speed_sonic_avg_mps,order=96*3,centre=FALSE)),aes(x=DateTime,y=ma_wind_speed_sonic_avg_mps))+ #Calculate a 3 day moving average
  scale_x_datetime(breaks=date.breaks2017_lots,label=date.labels_lots,limits=limsIceOn_2017)+
  scale_y_continuous(limits=windSpeed.limits,breaks=windSpeed.breaks)+
  ylab(bquote(Wind~speed~(m/s)))+
  xlab("Date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(color = c("black",rep("light grey",6))))

#Barometric pressure in 2016 with ice phenology####
gg.2016pressure<-ggplot()+
  geom_hline(yintercept=0)+
  geom_rect(aes(xmin=as.POSIXct(strptime(c("2016-12-16 00:00"),format = "%Y-%m-%d %H:%M")), #Add a box for the ice in day
                xmax=as.POSIXct(strptime(c("2016-12-16 23:59"),format = "%Y-%m-%d %H:%M")), 
                ymin=-Inf, 
                ymax=Inf),
            fill=alpha("grey",1))+
  geom_vline(data=segmented2016_results_stability%>%filter(optimal==TRUE),aes(xintercept=ymd_hms(date_bp)),color=color_bp)+
  geom_point(data=SensorData_derivedFill,aes(x=DateTime,y=station_pressure_avg_mbar),shape=21,fill=alpha("light blue",0.5),color=alpha("light blue",0.5),size=0.8)+
  geom_line(data=SensorData_derivedFill%>%mutate(ma_station_pressure_avg_mbar=forecast::ma(station_pressure_avg_mbar,order=96*3,centre=FALSE)),aes(x=DateTime,y=ma_station_pressure_avg_mbar))+ #Calculate a 3 day moving average
  scale_x_datetime(breaks=date.breaks2016_lots,label=date.labels_lots,limits=limsIceOn_2016)+
  scale_y_continuous(limits=pressure.limits,breaks=pressure.breaks)+
  ylab(bquote(Barometric~pressure~(mBar)))+
  xlab("Date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(color = c("black",rep("light grey",6))))

#Barometric pressure in 2017 with ice phenology####
gg.2017pressure<-ggplot()+
  geom_hline(yintercept=0)+
  geom_rect(aes(xmin=as.POSIXct(strptime(c("2017-12-27 00:00"),format = "%Y-%m-%d %H:%M")), #Add a box for the ice in day
                xmax=as.POSIXct(strptime(c("2017-12-27 23:59"),format = "%Y-%m-%d %H:%M")), 
                ymin=-Inf, 
                ymax=Inf),
            fill=alpha("grey",1))+
  geom_vline(data=segmented2017_results_stability%>%filter(optimal==TRUE),aes(xintercept=ymd_hms(date_bp)),color=color_bp)+
  geom_point(data=SensorData_derivedFill,aes(x=DateTime,y=station_pressure_avg_mbar),shape=21,fill=alpha("light blue",0.5),color=alpha("light blue",0.5),size=0.8)+
  geom_line(data=SensorData_derivedFill%>%mutate(ma_station_pressure_avg_mbar=forecast::ma(station_pressure_avg_mbar,order=96*3,centre=FALSE)),aes(x=DateTime,y=ma_station_pressure_avg_mbar))+ #Calculate a 3 day moving average
  scale_x_datetime(breaks=date.breaks2017_lots,label=date.labels_lots,limits=limsIceOn_2017)+
  scale_y_continuous(limits=pressure.limits,breaks=pressure.breaks)+
  ylab(bquote(Barometric~pressure~(mBar)))+
  xlab("Date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(color = c("black",rep("light grey",6))))

#Combine the files in the list####
List_meteo<-list(gg.2016airTemp+
             xlab("")+
             theme(axis.text.x=element_blank(),
                   #axis.ticks.x=element_blank()
             )+
             annotate("text",x=as.POSIXct(strptime(c("2016-12-30 00:00"),format = "%Y-%m-%d %H:%M")),y=11.0,label="(a)",hjust=0.5,vjust=0.1)+
             annotate("text",x=as.POSIXct(strptime(c("2016-12-01 00:00"),format = "%Y-%m-%d %H:%M")),y=11.0,label="2016",hjust=0.1,vjust=0.1),
           gg.2017airTemp+
             xlab("")+
             ylab("")+
             theme(axis.text.x=element_blank(),
                   #axis.ticks.x=element_blank(),
                   axis.text.y=element_blank(),
                   #axis.ticks.y=element_blank()
             )+
             annotate("text",x=as.POSIXct(strptime(c("2017-12-30 00:00"),format = "%Y-%m-%d %H:%M")),y=11.00,label="(b)",hjust=0.5,vjust=0.1)+
             annotate("text",x=as.POSIXct(strptime(c("2017-12-01 00:00"),format = "%Y-%m-%d %H:%M")),y=11.0,label="2017",hjust=0.1,vjust=0.1),
           gg.2016windSpeed+
             xlab("")+
             theme(axis.text.x=element_blank(),
                   #axis.ticks.x=element_blank()
             )+
             annotate("text",x=as.POSIXct(strptime(c("2016-12-30 00:00"),format = "%Y-%m-%d %H:%M")),y=4.0,label="(c)",hjust=0.5,vjust=0.1)+
             annotate("text",x=as.POSIXct(strptime(c("2016-12-01 00:00"),format = "%Y-%m-%d %H:%M")),y=4.0,label="2016",hjust=0.1,vjust=0.1),
           gg.2017windSpeed+
             xlab("")+
             ylab("")+
             theme(axis.text.x=element_blank(),
                   #axis.ticks.x=element_blank(),
                   axis.text.y=element_blank(),
                   #axis.ticks.y=element_blank()
             )+
             annotate("text",x=as.POSIXct(strptime(c("2017-12-30 00:00"),format = "%Y-%m-%d %H:%M")),y=4.0,label="(d)",hjust=0.5,vjust=0.1)+
             annotate("text",x=as.POSIXct(strptime(c("2017-12-01 00:00"),format = "%Y-%m-%d %H:%M")),y=4.0,label="2017",hjust=0.1,vjust=0.1),
           gg.2016pressure+
             annotate("text",x=as.POSIXct(strptime(c("2016-12-30 00:00"),format = "%Y-%m-%d %H:%M")),y=1018,label="(e)",hjust=0.5,vjust=0.2)+
             annotate("text",x=as.POSIXct(strptime(c("2016-12-01 00:00"),format = "%Y-%m-%d %H:%M")),y=1018,label="2016",hjust=0.1,vjust=0.1),
           gg.2017pressure+
             ylab("")+
             theme(axis.text.y=element_blank(),
                   #axis.ticks.y=element_blank()
             )+
             annotate("text",x=as.POSIXct(strptime(c("2017-12-30 00:00"),format = "%Y-%m-%d %H:%M")),y=1018,label="(f)",hjust=0.5,vjust=0.2)+
             annotate("text",x=as.POSIXct(strptime(c("2017-12-01 00:00"),format = "%Y-%m-%d %H:%M")),y=1018,label="2017",hjust=0.1,vjust=0.1)
)

#Put the plots on a 2x2 matrix####
(gg.hfmeteo<-wrap_plots(List_meteo,ncol=2,nrow=3)&theme(plot.margin = unit(c(3,3,3,3),"pt")))

#Could do a 2x1 with width 6, height = 4
ggsave(paste("figures/MohonkWinterLimno-FigureX-HighFrequencyMeteorology.jpg",sep=""), plot=gg.hfmeteo, width=6, height=6,units="in", dpi=300)



