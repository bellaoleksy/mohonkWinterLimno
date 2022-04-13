#Script 04-VariabilityRollingWindow.R####
#Explore the rolling window in ice phenology variability for Mohonk Lake
#Created 16Mar2022, by David Richardson (DCR)

#Run the main script to bring in all data and functions####
source('00_main.R')

#Libraries
if (!require(zoo)) {install.packages("zoo")}

#Try using zoo package
library(zoo)



#Rolling CV using zoo package####
#Code snagged from Gavin in this response:
#https://stackoverflow.com/questions/13195442/moving-variance-in-r

##Ice Duration
iceDuration_days_withNAs<-MohonkIce %>%
  select(LengthOfIceCover_days,Year) 


temp.sensSlope<-MTCC.sensSlope(x=iceDuration_days_withNAs$Year,y=iceDuration_days_withNAs$LengthOfIceCover_days)

iceDuration_days_withNAs%>%mutate(sensSlope_fit=as.numeric(temp.sensSlope$coefficients["Intercept"])+as.numeric(temp.sensSlope$coefficients["Year"])*Year)
ggplot(data=iceDuration_days_withNAs,aes(y=LengthOfIceCover_days,x=Year))+geom_point()+
  geom_abline(intercept=as.numeric(temp.sensSlope$coefficients["Intercept"]),slope=as.numeric(temp.sensSlope$coefficients["Year"]))

plot(temp.sensSlope$residuals~iceDuration_days_withNAs$Year)

#Rolling window calculations of CV or standard deviation####
RW_length<-15 #set the rolling window length globally (in years)

#Calculate rolling window and store year as well as median of that same window####
iceDuration_variability<-
  tibble(year_median=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(Year), width =RW_length, FUN = median)),
         LengthOfIceCover_days_sd=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(LengthOfIceCover_days), width =RW_length, FUN = function(x, na.rm = TRUE)  {sd(x, na.rm = na.rm)})), #rolling window standard deviation of length RW_length
         LengthOfIceCover_days_cv=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(LengthOfIceCover_days), width =RW_length, FUN = cv)), #rolling window coefficient of variation of length RW_length
         RollingWindow_years=RW_length) #Record the Rolling window length in a column

#Plot the variability####
ggplot(data=iceDuration_variability,
       aes(x=year_median,y=LengthOfIceCover_days_cv))+
  geom_point()+
  xlab(bquote(Year~median))+
  ylab("Ice duration (c.v.)")+
  geom_smooth(method="gam", color="black", size=0.5)

#Plot the sd####
ggplot(data=iceDuration_variability,
       aes(x=year_median,y=LengthOfIceCover_days_sd))+
  geom_point()+
  xlab(bquote(Year~median))+
  ylab("Ice duration (s.d.)")+
  geom_smooth(method="gam", color="black", size=0.5)
#Effectively the same pattern. 

#Create dataframe all rolling windows####
datalist=list()  #initialize empty list for storing a bunch of ice duration variabilities of different lengths

#*Loop through all rolling window sizes from 3 to 30 years####
for(RW_length_i in 3:30){
  iceDuration_temporary<-
    tibble(year_median=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(Year), width =RW_length_i, FUN = median)),
           LengthOfIceCover_days_sd=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(LengthOfIceCover_days), width =RW_length_i, FUN = function(x, na.rm = TRUE)  {sd(x, na.rm = na.rm)})), #rolling window standard deviation of length RW_length
           LengthOfIceCover_days_cv=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(LengthOfIceCover_days), width =RW_length_i, FUN = cv)), #rolling window coefficient of variation of length RW_length
           LengthOfIceCover_days_mean=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(LengthOfIceCover_days), width =RW_length_i, FUN = function(x, na.rm = TRUE)  {mean(x, na.rm = na.rm)})), #rolling window mean of length RW_length
           RollingWindow_years=RW_length_i) #Record the Rolling window length in a column
  
    #Calculate slope
      sensSlope<-MTCC.sensSlope(iceDuration_temporary$year_median,iceDuration_temporary$LengthOfIceCover_days_sd)
      sensSlope$coefficients
    #Store the residuals and other sens slope fit stats in iceDuration_temporary  
      iceDuration_temporary<-iceDuration_temporary%>%
                              mutate(sensSlope_fit=year_median*sensSlope$coefficients["Year"]+sensSlope$coefficients["Intercept"],
                                     sensSlope_slope=sensSlope$coefficients["Year"],
                                     sensSlope_intercept=sensSlope$coefficients["Intercept"],
                                     sensSlope_residuals=sensSlope$residuals,
                                     sensSlope_pval=sensSlope$pval,
                                     sensSlope_z_stat=sensSlope$z_stat,
                                     sensSlope_n=sensSlope$n)
    #Export each Rollingwindow length to the datalist  
      datalist[[RW_length_i]]<-iceDuration_temporary  #Store the temporary data frame in a list
      
    }   

#*compile them all in one data frame####    
iceDuration_variability_all<-do.call(bind_rows,datalist) 

#*Rolling sd vs. year facet wrapped by all rolling window sizes####
ggplot(data=iceDuration_variability_all,
       aes(x=year_median,y=LengthOfIceCover_days_sd))+
  geom_point()+
  geom_line(aes(y=sensSlope_fit),size=1.5,col="blue")+
  xlab(bquote(Year~median))+
  ylab("Ice duration (s.d.)")+
  geom_smooth(method="gam", color="black", size=0.5)+
  facet_wrap(~RollingWindow_years) #, scales="free_y"
    
#*Rolling sd residuals from sens slopes vs. year facet wrapped by all rolling window sizes####
ggplot(data=iceDuration_variability_all,
       aes(x=year_median,y=sensSlope_residuals))+
  geom_point()+
  xlab(bquote(Year~median))+
  ylab("Ice duration (s.d.)")+
  geom_smooth(method="gam", color="black", size=0.5)+
  facet_wrap(~RollingWindow_years) #, scales="free_y"

#*Rolling cv vs. year facet wrapped by all rolling window sizes####
ggplot(data=iceDuration_variability_all,
       aes(x=year_median,y=LengthOfIceCover_days_cv))+
  geom_point()+
  xlab(bquote(Year~median))+
  ylab("Ice duration (c.v.)")+
  geom_smooth(method="gam", color="black", size=0.5)+
  facet_wrap(~RollingWindow_years)

#*Rolling mean vs. year facet wrapped by all rolling window sizes####
ggplot(data=iceDuration_variability_all,
       aes(x=year_median,y=LengthOfIceCover_days_mean))+
  geom_point()+
  xlab(bquote(Year~median))+
  ylab("Ice duration (mean)")+
  geom_smooth(method="gam", color="black", size=0.5)+
  facet_wrap(~RollingWindow_years)

#Find periodicity in each plot#####
#https://stackoverflow.com/questions/17788859/acf-plot-with-ggplot2-setting-width-of-geom-bar
#probably have to loop through each one rolling window size, create acf data frame and plot
#Or create acf data frame as in here: https://stackoverflow.com/questions/44697596/using-ggplots-facet-wrap-with-autocorrelation-plot  
library(purrr)

df_acf <- iceDuration_variability_all %>% 
  group_by(RollingWindow_years) %>% 
  summarise(list_acf=list(acf(sensSlope_residuals, plot=FALSE,na.action = na.pass))) %>%
  mutate(acf_vals=purrr::map(list_acf, ~as.numeric(.x$acf))) %>% 
  select(-list_acf) %>% 
  unnest(cols=c(acf_vals)) %>% 
  group_by(RollingWindow_years) %>% 
  mutate(lag=row_number() - 1)

df_ci <- iceDuration_variability_all %>% 
  group_by(RollingWindow_years) %>% 
  summarise(ci = qnorm((1 + 0.95)/2)/sqrt(n()))

ggplot(df_acf, aes(x=lag, y=acf_vals)) +
  geom_bar(stat="identity", width=.05) +
  geom_hline(yintercept = 0) +
  geom_hline(data = df_ci, aes(yintercept = -ci), color="blue", linetype="dotted") +
  geom_hline(data = df_ci, aes(yintercept = ci), color="blue", linetype="dotted") +
  labs(x="Lag", y="ACF") +
  facet_wrap(~RollingWindow_years)


#Is the variability increasing?#####
#This tests the slope of each time series and determines significance correcting for 27 comparisons
iceDuration_variability_summary<-iceDuration_variability_all%>%group_by(RollingWindow_years)%>%summarize(sensSlope_pval=mean(sensSlope_pval),sensSlope_slope=mean(sensSlope_slope))%>%mutate(significance=ifelse(sensSlope_pval<0.05/27,"*","NS"))%>%print(n=Inf)
#Is the slope different depending on the rolling window
ggplot(data=iceDuration_variability_summary,aes(y=sensSlope_slope,x=RollingWindow_years))+geom_point()

#STOPPED HERE######
#Questions remain:
  #periodicity in the residuals? Is it a residuals of the rolling window?
  #GARCH models in econ get at volatility: https://www.idrisstsafack.com/post/garch-models-with-r-programming-a-practical-example-with-tesla-stock