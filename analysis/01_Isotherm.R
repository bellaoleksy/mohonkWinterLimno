# Created 2022-07-07 by IAO with script copied over from DCR et al.'s Forecasting_MohonkLake github repository.

#Run the main script to bring in all data and functions####
source('analysis/00_main.R')


## IAO was running out of memory on my machine so this is the solution I tried
## https://stackoverflow.com/questions/51295402/r-on-macos-error-vector-memory-exhausted-limit-reached
# if(!require(usethis)){install.packages("usethis")}
# library(usethis) 
# usethis::edit_r_environ()

gc()

# Set theme ---------------------------------------------------------------


theme_MS <- function () { 
  ggthemes::theme_base(base_size=8) %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="white", colour=NA, linewidth=1.0),
      plot.title=element_text(face="plain",hjust=0.5),
      plot.subtitle = element_text(color="dimgrey", hjust=0, size=8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text.y = element_text(size=8, angle=270),
      strip.text.x = element_text(size=8),
      panel.spacing=grid::unit(0,"lines"),
      axis.ticks.length = unit(0.1, "cm")
    )
}

theme_set(theme_MS())





#Calculate Isotherms for Ice In ------------------------------------------


#Define dataframe for the function
temp_IceIn_forfunction <- 
  MohonkDailyWeatherFull %>% 
  select(Date, water_year, TempMax_degC:TempMean_degC) %>%
  rename(WaterYear=water_year) %>% #to match syntax below
  filter(WaterYear >= 1932) %>%
  mutate(MonthNumber = month(Date), #create month column using lubridate
         Month = month(MonthNumber, label=TRUE),
         WaterYear_date = hydro.day(Date)) %>% #create column with month names using factor to create labels for different levels 
  select(WaterYear, Month, Date, WaterYear_date, TempMax_degC, TempMin_degC, TempMean_degC) 

#Define variables to use in isotherm function
airTemp <- data.frame(TempMax_degC = temp_IceIn_forfunction$TempMax_degC, TempMin_degC = temp_IceIn_forfunction$TempMin_degC, TempMean_degC = temp_IceIn_forfunction$TempMean_degC)
mdays <- c(1:30) 
threshold <- c(0:5)


#Water year dates
icein_wateryear_dates = list()
i.list = 1
for(i.airTemp in 1:length(airTemp)){
  for(i.mdays in 1:length(mdays)){
    for(i.threshold in 1:length(threshold)){
      temp <- isotherm_mdays_thresh_IceIn_WaterYear_date(airTemp[i.airTemp], mdays[i.mdays], threshold[i.threshold])
      colnames(temp)[2] <- paste("isotherm", names(airTemp)[i.airTemp], mdays[i.mdays], "days", threshold[i.threshold], "degC", "WaterYear", "date", sep = "_")
      icein_wateryear_dates[[i.list]] <- temp
      i.list <- i.list + 1
    }
  }
}

#Join together datasets in list
#IAO -- I was having memory issues so I subdivided. Clunky, I know :( 
slice1 <- icein_wateryear_dates[1:50]
slice2 <- icein_wateryear_dates[51:100]
slice3 <- icein_wateryear_dates[101:150]
slice4 <- icein_wateryear_dates[151:200]
slice5 <- icein_wateryear_dates[201:250]
slice6 <- icein_wateryear_dates[251:300]
slice7 <- icein_wateryear_dates[301:350]
slice8 <- icein_wateryear_dates[351:400]
slice9 <- icein_wateryear_dates[401:450]
slice10 <- icein_wateryear_dates[451:500]
slice11 <- icein_wateryear_dates[501:540]

Isotherm_WaterYear_dates_IceIn <-  bind_rows(
  reduce(slice1, full_join, by = "WaterYear"),
  reduce(slice2, full_join, by = "WaterYear"),
  reduce(slice3, full_join, by = "WaterYear"),
  reduce(slice4, full_join, by = "WaterYear"),
  reduce(slice5, full_join, by = "WaterYear"),
  reduce(slice6, full_join, by = "WaterYear"),
  reduce(slice7, full_join, by = "WaterYear"),
  reduce(slice8, full_join, by = "WaterYear"),
  reduce(slice9, full_join, by = "WaterYear"),
  reduce(slice10, full_join, by = "WaterYear"),
  reduce(slice11, full_join, by = "WaterYear")
)


# Isotherm_WaterYear_dates_IceIn <- reduce(icein_wateryear_dates, full_join, by = "WaterYear")
Isotherm_WaterYear_dates_IceIn <- full_join(Isotherm_WaterYear_dates_IceIn, MohonkIce, by = c("WaterYear"="Year"))
Isotherm_WaterYear_dates_IceIn <-
  Isotherm_WaterYear_dates_IceIn %>%
  select(WaterYear, IceInDate, IceInDayofYear, IceInDayofYear_fed, isotherm_TempMax_degC_1_days_0_degC_WaterYear_date:isotherm_TempMean_degC_30_days_5_degC_WaterYear_date)


save(Isotherm_WaterYear_dates_IceIn, file = "data/isotherm_dataframes.RData")
# To load the data again
# load("data/isotherm_dataframes.RData")

#Calculate Isotherms for Ice Out ------------------------------------------

#Define dataframe for the function
temp_IceOut_forfunction <- 
  MohonkDailyWeatherFull %>% 
  select(Date, water_year, TempMax_degC:TempMean_degC) %>%
  rename(WaterYear=water_year) %>% #to match syntax below
  filter(WaterYear >= 1932) %>%
  mutate(MonthNumber = month(Date), #create month column using lubridate
         Month = month(MonthNumber, label=TRUE),
         WaterYear_date = hydro.day(Date)) %>% #create column with month names using factor to create labels for different levels 
  select(WaterYear, Month, Date, WaterYear_date, TempMax_degC, TempMin_degC, TempMean_degC)


#Define variables to use in isotherm function
airTemp <- data.frame(TempMax_degC = temp_IceOut_forfunction$TempMax_degC, TempMin_degC = temp_IceOut_forfunction$TempMin_degC, TempMean_degC = temp_IceOut_forfunction$TempMean_degC)
mdays <- c(1:30) 
threshold <- c(0:5)


#Water year dates
#Run for loop over all combinations of variables and create large list
iceout_wateryear_dates = list()
i.list = 1
for(i.airTemp in 1:length(airTemp)){
  for(i.mdays in 1:length(mdays)){
    for(i.threshold in 1:length(threshold)){
      temp <- isotherm_mdays_thresh_IceOut_WaterYear_date(airTemp[i.airTemp], mdays[i.mdays], threshold[i.threshold])
      colnames(temp)[2] <- paste("isotherm", names(airTemp)[i.airTemp], mdays[i.mdays], "days", threshold[i.threshold], "degC", "WaterYear", "date", sep = "_")
      iceout_wateryear_dates[[i.list]] <- temp
      i.list <- i.list + 1
    }
  }
}

#Join together datasets in list
#IAO -- I was having memory issues so I subdivided. Clunky, I know :( 
slice1 <- iceout_wateryear_dates[1:10]
slice2 <- iceout_wateryear_dates[11:20]
slice3 <- iceout_wateryear_dates[21:30]
slice4 <- iceout_wateryear_dates[31:40]
slice5 <- iceout_wateryear_dates[41:50]
slice6 <- iceout_wateryear_dates[51:60]
slice7 <- iceout_wateryear_dates[61:70]
slice8 <- iceout_wateryear_dates[71:80]
slice9 <- iceout_wateryear_dates[81:90]
slice10 <- iceout_wateryear_dates[91:100]
slice11 <- iceout_wateryear_dates[111:120]
slice12 <- iceout_wateryear_dates[121:130]
slice13 <- iceout_wateryear_dates[131:140]
slice14 <- iceout_wateryear_dates[141:150]
slice15 <- iceout_wateryear_dates[151:160]
slice16 <- iceout_wateryear_dates[161:170]
slice17 <- iceout_wateryear_dates[171:180]
slice18 <- iceout_wateryear_dates[181:190]



Isotherm_WaterYear_dates_IceOut <-  bind_rows(
  reduce(slice1, full_join, by = "WaterYear"),
  reduce(slice2, full_join, by = "WaterYear"),
  reduce(slice3, full_join, by = "WaterYear"),
  reduce(slice4, full_join, by = "WaterYear"),
  reduce(slice5, full_join, by = "WaterYear"),
  reduce(slice6, full_join, by = "WaterYear"),
  reduce(slice7, full_join, by = "WaterYear"),
  reduce(slice8, full_join, by = "WaterYear"),
  reduce(slice9, full_join, by = "WaterYear"),
  reduce(slice10, full_join, by = "WaterYear"),
  reduce(slice11, full_join, by = "WaterYear"),
  reduce(slice12, full_join, by = "WaterYear"),
  reduce(slice13, full_join, by = "WaterYear"),
  reduce(slice14, full_join, by = "WaterYear"),
  reduce(slice15, full_join, by = "WaterYear"),
  reduce(slice16, full_join, by = "WaterYear"),
  reduce(slice17, full_join, by = "WaterYear"),
  reduce(slice18, full_join, by = "WaterYear"),
)



slices <- list()

for (i in 1:54) {
  start_index <- (i - 1) * 10 + 1
  end_index <- i * 10
  slice <- iceout_wateryear_dates[start_index:end_index]
  slices[[paste0("slice", i)]] <- slice
}

Isotherm_WaterYear_dates_IceOut <- bind_rows(lapply(slices, function(slice) {
  reduce(slice, full_join, by = "WaterYear")
}))

# Isotherm_WaterYear_dates_IceOut <- reduce(iceout_wateryear_dates, full_join, by = "WaterYear")
Isotherm_WaterYear_dates_IceOut <- full_join(Isotherm_WaterYear_dates_IceOut, MohonkIce, by = c("WaterYear"="Year"))
Isotherm_WaterYear_dates_IceOut <-
  Isotherm_WaterYear_dates_IceOut %>%
  select(WaterYear, IceOutDate, IceOutDayofYear, IceOutDayofYear_fed, isotherm_TempMax_degC_1_days_0_degC_WaterYear_date:isotherm_TempMean_degC_30_days_5_degC_WaterYear_date)

#Remove datasets, lists, and values that are no longer necessary
rm(temp_IceIn_forfunction,
   icein_wateryear_dates,
   temp_IceOut_forfunction,
   iceout_wateryear_dates,
   airTemp,
   mdays,
   threshold,
   i.airTemp,
   i.mdays,
   i.threshold,
   i.list,
   temp)



# Run Isotherm Ice In regressions -----------------------------------------

#Isotherm Ice In
#*Create isotherm summary dataframe
#Define variables to use in isotherm function
IsothermFormula <- 
  Isotherm_WaterYear_dates_IceIn %>%
  select(contains("isotherm"))
IceIn_ObservedDates <- data.frame(Isotherm_WaterYear_dates_IceIn$IceInDayofYear_fed)

#Run for loop over all combinations of variables and create large list
summary_icein = list()
i.list = 1
for(i.IsothermFormula in 1:length(IsothermFormula)){
  for(i.IceIn_ObservedDates in 1:length(IceIn_ObservedDates)){
    summary <- isotherm_summary(IsothermFormula[i.IsothermFormula], IceIn_ObservedDates[i.IceIn_ObservedDates])
    summary$IsothermFormula_names <- paste(names(IsothermFormula)[i.IsothermFormula])
    summary <- summary[, c(4, 3, 1, 2)]
    summary_icein[[i.list]] <- summary
    i.list <- i.list + 1
  }
}

#Join together dataframe in list
IsothermSummary_IceIn <- bind_rows(summary_icein)
IsothermSummary_IceIn <-
  IsothermSummary_IceIn %>%
  arrange(desc(r.squared))

head(IsothermSummary_IceIn)

#~~~Exploring the relationships with highest r squared values####
head(IsothermSummary_IceIn)

ggplot(data = Isotherm_WaterYear_dates_IceIn, aes(x = IceInDayofYear_fed, y = isotherm_TempMax_degC_17_days_0_degC_WaterYear_date)) +
  geom_point() +
  labs(x = "Ice In Observed Date",
       y = "Ice In Predicted Date",
       title = "Observed vs Predicted Ice In Dates",
       subtitle = "Using Isotherm Formula: TempMax in degC, 17 day window, 0 degC threshold") +
  scale_x_continuous(breaks = c(60, 80, 100, 120), labels = c("Nov 29", "Dec 19", "Jan 8", "Jan 28")) +
  scale_y_continuous(breaks = c(60, 80, 100, 120), labels = c("Nov 29", "Dec 19", "Jan 8", "Jan 28")) +
  geom_abline(intercept=0, slope=1)

ggplot(data = Isotherm_WaterYear_dates_IceIn, aes(x = IceInDayofYear_fed, y = isotherm_TempMax_degC_18_days_0_degC_WaterYear_date)) +
  geom_point() + 
  labs(x = "Ice In Observed Date",
       y = "Ice In Predicted Date",
       title = "Observed vs Predicted Ice In Dates",
       subtitle = "Using Isotherm Formula: TempMax in degC, 18 day window, 0 degC threshold") +
  scale_x_continuous(breaks = c(60, 80, 100, 120), labels = c("Nov 29", "Dec 19", "Jan 8", "Jan 28")) + 
  scale_y_continuous(breaks = c(60, 80, 100, 120), labels = c("Nov 29", "Dec 19", "Jan 8", "Jan 28"))  +
  geom_abline(intercept=0, slope=1)

ggplot(data = Isotherm_WaterYear_dates_IceIn, aes(x = IceInDayofYear_fed, y = isotherm_TempMax_degC_30_days_0_degC_WaterYear_date)) +
  geom_point() +
  labs(x = "Ice In Observed Date",
       y = "Ice In Predicted Date",
       title = "Observed vs Predicted Ice In Dates",
       subtitle = "Using Isotherm Formula: TempMax in degC, 30 day window, 0 degC threshold") +
  scale_x_continuous(breaks = c(60, 80, 100, 120), labels = c("Nov 29", "Dec 19", "Jan 8", "Jan 28")) + 
  scale_y_continuous(breaks = c(60, 80, 100, 120), labels = c("Nov 29", "Dec 19", "Jan 8", "Jan 28"))  +
  geom_abline(intercept=0, slope=1)



# Run Isotherm Ice Out regressions -----------------------------------------

#*Create isotherm summary dataframe
#Define variables to use in isotherm function
IsothermFormula <- 
  Isotherm_WaterYear_dates_IceOut %>%
  select(contains("isotherm"))
IceOut_ObservedDates <- data.frame(Isotherm_WaterYear_dates_IceOut$IceOutDayofYear_fed)

#Run for loop over all combinations of variables and create large list
summary_iceout = list()
i.list = 1
for(i.IsothermFormula in 1:length(IsothermFormula)){
  for(i.IceOut_ObservedDates in 1:length(IceOut_ObservedDates)){
    summary <- isotherm_summary(IsothermFormula[i.IsothermFormula], IceOut_ObservedDates[i.IceOut_ObservedDates])
    summary$IsothermFormula_names <- paste(names(IsothermFormula)[i.IsothermFormula])
    summary <- summary[, c(4, 3, 1, 2)]
    summary_iceout[[i.list]] <- summary
    i.list <- i.list + 1
  }
}

#Join together dataframes in list
IsothermSummary_IceOut <- bind_rows(summary_iceout)
IsothermSummary_IceOut <-
  IsothermSummary_IceOut %>%
  arrange(desc(r.squared))


#~~~Exploring the relationships with highest r squared values####
head(IsothermSummary_IceOut)
IsothermSummary_IceOut[1:10,]


ggplot(data = Isotherm_WaterYear_dates_IceOut, aes(x = IceOutDayofYear_fed, y = isotherm_TempMean_degC_29_days_4_degC_WaterYear_date)) +
  geom_point() +
  labs(x = "Ice Out Observed Date",
       y = "Ice Out Predicted Date",
       title = "Observed vs Predicted Ice Out Dates",
       subtitle = "Using Isotherm Formula: TempAvg in degC, 29 day window, 4 degC threshold") +
  scale_x_continuous(breaks = c(160, 180, 200, 220), labels = c("Mar 9", "Mar 29", "Apr 18", "May 8")) + 
  scale_y_continuous(breaks = c(160, 180, 200, 220), labels = c("Mar 9", "Mar 29", "Apr 18", "May 8"))  +
  geom_abline(intercept=0, slope=1)

ggplot(data = Isotherm_WaterYear_dates_IceOut, aes(x = IceOutDayofYear_fed, y = isotherm_TempMean_degC_30_days_3_degC_WaterYear_date)) +
  geom_point() +
  labs(x = "Ice Out Observed Date",
       y = "Ice Out Predicted Date",
       title = "Observed vs Predicted Ice Out Dates",
       subtitle = "Using Isotherm Formula: TempAvg in degC, 30 day window, 3 degC threshold") +
  scale_x_continuous(breaks = c(160, 180, 200, 220), labels = c("Mar 9", "Mar 29", "Apr 18", "May 8")) + 
  scale_y_continuous(breaks = c(160, 180, 200, 220), labels = c("Mar 9", "Mar 29", "Apr 18", "May 8"))  +
  geom_abline(intercept=0, slope=1)

ggplot(data = Isotherm_WaterYear_dates_IceOut, aes(x = IceOutDayofYear_fed, y = isotherm_TempMean_degC_28_days_4_degC_WaterYear_date)) +
  geom_point() +
  labs(x = "Ice Out Observed Date",
       y = "Ice Out Predicted Date",
       title = "Observed vs Predicted Ice Out Dates",
       subtitle = "Using Isotherm Formula: TempAvg in degC, 28 day window, 4 degC threshold") +
  scale_x_continuous(breaks = c(160, 180, 200, 220), labels = c("Mar 9", "Mar 29", "Apr 18", "May 8")) + 
  scale_y_continuous(breaks = c(160, 180, 200, 220), labels = c("Mar 9", "Mar 29", "Apr 18", "May 8"))  +
  geom_abline(intercept=0, slope=1)


ggplot(data = Isotherm_WaterYear_dates_IceOut, aes(x = IceOutDayofYear_fed, y = isotherm_TempMin_degC_25_days_0_degC_WaterYear_date)) +
  geom_point() +
  labs(x = "Ice Out Observed Date",
       y = "Ice Out Predicted Date",
       title = "Observed vs Predicted Ice Out Dates",
       subtitle = "Using Isotherm Formula: TempAvg in degC, 25 day window, 0 degC threshold") +
  # scale_x_continuous(breaks = c(160, 180, 200, 220), labels = c("Mar 9", "Mar 29", "Apr 18", "May 8")) + 
  # scale_y_continuous(breaks = c(160, 180, 200, 220), labels = c("Mar 9", "Mar 29", "Apr 18", "May 8"))  +
  geom_abline(intercept=0, slope=1)

as.Date(160-91, origin="2014-01-02")



#Remove dataframes, lists, and values that are no longer necessary
rm(IsothermFormula,
   IceIn_ObservedDates,
   IceOut_ObservedDates,
   summary_icein,
   summary_iceout,
   summary,
   i.list,
   i.IsothermFormula,
   i.IceIn_ObservedDates,
   i.IceOut_ObservedDates)

