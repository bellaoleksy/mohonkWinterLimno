#Script 04-VariabilityRollingWindow.R####
#Explore the rolling window in ice phenology variability for Mohonk Lake
#Created 16Mar2022, by David Richardson (DCR)

#Run the main script to bring in all data and functions####
source('00_main.R')

#Libraries
if (!require(zoo)) {install.packages("zoo")}
if(!require(patchwork)){install.packages("patchwork")}
if(!require(forecast)){install.packages("forecast")}
if(!require(corrplot)){install.packages("corrplot")}

#Try using zoo package
library(zoo)
library(patchwork) #laying out multipanel plots with the same size
library(forecast)
library(corrplot)


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
           LengthOfIceCover_days_n=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(LengthOfIceCover_days), width =RW_length_i, FUN = function(x, na.rm = TRUE)  {sum(!is.na(x))})), #rolling window count the number of nonNA measurements that went into length RW_length
           RollingWindow_years=RW_length_i) #Record the Rolling window length in a column
   #Figure out proportion of days used for rolling stats
    iceDuration_temporary<-iceDuration_temporary%>%mutate(proportion_used=LengthOfIceCover_days_n/RollingWindow_years)%>%
                            mutate(LengthOfIceCover_days_sd=ifelse(proportion_used>=0.75,LengthOfIceCover_days_sd,NA),
                                   LengthOfIceCover_days_cv=ifelse(proportion_used>=0.75,LengthOfIceCover_days_cv,NA),
                                   LengthOfIceCover_days_mean=ifelse(proportion_used>=0.75,LengthOfIceCover_days_mean,NA))
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
    
    #Fit auto.arima with xreg as year
      #auto.arima(iceDuration_temporary$LengthOfIceCover_days_sd,
                 #xreg=c(iceDuration_temporary$year_median),
                 # xreg=cbind(AnnualData$GlobalTempAnomoly_C,
                 # AnnualData$Year),
                 #seasonal=FALSE,allowdrift = FALSE,
                 #stationary=TRUE)
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

#Calculate the acf for various lags for each one
df_acf <- iceDuration_variability_all %>% 
  group_by(RollingWindow_years) %>% 
  summarise(list_acf=list(acf(sensSlope_residuals, plot=FALSE,na.action = na.pass))) %>%
  mutate(acf_vals=purrr::map(list_acf, ~as.numeric(.x$acf))) %>% 
  select(-list_acf) %>% 
  unnest(cols=c(acf_vals)) %>% 
  group_by(RollingWindow_years) %>% 
  mutate(lag=row_number() - 1)

#Calculate confidence intervals for significance
df_ci <- iceDuration_variability_all %>% 
  group_by(RollingWindow_years) %>% 
  summarise(ci = qnorm((1 + 0.95)/2)/sqrt(n()))

#Merge the CI back in with df_acf
df_acf<-left_join(df_acf,df_ci,by="RollingWindow_years")%>%mutate(significant_acf=ifelse(abs(acf_vals)>ci,"*","NS"))

#Plot the acfs by rolling lag
ggplot(df_acf, aes(x=lag, y=acf_vals)) +
  geom_bar(stat="identity", width=.05) +
  geom_hline(yintercept = 0) +
  geom_hline(data = df_ci, aes(yintercept = -ci), color="blue", linetype="dotted") +
  geom_hline(data = df_ci, aes(yintercept = ci), color="blue", linetype="dotted") +
  labs(x="Lag", y="ACF") +
  facet_wrap(~RollingWindow_years)

#Summarize which lags are signficant
df_acf_summary<-df_acf%>%filter(significant_acf=="*")%>%ungroup()%>%group_by(lag)%>%dplyr::summarize(common_lag=n())
  df_acf%>%filter(significant_acf=="*")%>%print(n=Inf)
  #This finds the largest correlation for bigger than 6 year lag
  df_acf%>%filter(significant_acf=="*")%>%filter(lag>6)%>%group_by(RollingWindow_years)%>%slice(which.max(abs(acf_vals)))%>%print(n=Inf)
ggplot(data=df_acf_summary,aes(x=lag,y=common_lag))+geom_point()



#Is the variability increasing?#####
#This tests the slope of each time series and determines significance correcting for 27 comparisons
iceDuration_variability_summary<-iceDuration_variability_all%>%group_by(RollingWindow_years)%>%dplyr::summarize(sensSlope_pval=mean(sensSlope_pval),sensSlope_slope=mean(sensSlope_slope),sensSlope_intercept=mean(sensSlope_intercept),sensSlope_z_stat=mean(sensSlope_z_stat),sensSlope_n=mean(sensSlope_n))%>%mutate(significance=ifelse(sensSlope_pval<0.05/27,"*","NS"))%>%print(n=Inf)
#Is the slope different depending on the rolling window
ggplot(data=iceDuration_variability_summary,aes(y=sensSlope_slope,x=RollingWindow_years))+geom_point()


#Merge for a single plot by selecting a specifc rolling window and merging with original data
#Calculate Bollinger Bands by doing the moving average +/- moving sd for that window
window_select<-4 #set window here
Merge_singleRollingWindow<-left_join(tibble(iceDuration_days_withNAs),iceDuration_variability_all%>%filter(RollingWindow_years==window_select)%>%mutate(Year=trunc(year_median)),by="Year")%>%mutate(max_sd=LengthOfIceCover_days_mean+LengthOfIceCover_days_sd,min_sd=LengthOfIceCover_days_mean-LengthOfIceCover_days_sd)

  #pacf
  pacf(Merge_singleRollingWindow$sensSlope_residuals,na.action = na.pass)

#time series of the residuals
auto.arima(Merge_singleRollingWindow$sensSlope_residuals)

#Graph a single plot with rolling window SDs as shaded region
#These are bollinger band in econ/finance: https://www.investopedia.com/terms/b/bollingerbands.asp
gg.duration<-ggplot(data=Merge_singleRollingWindow)+
  geom_ribbon(aes(x=Year,ymin=min_sd,ymax=max_sd),color="light grey",fill="sky blue")+
  geom_line(aes(x=Year,y=LengthOfIceCover_days_mean),color="light grey",size=0.5)+ #Moving average
  #geom_errorbar(aes(x=Year,ymin=min_sd,ymax=max_sd),size=1)+
  geom_point(aes(x=Year,y=LengthOfIceCover_days),shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+ #actual days of ice cover
  ylab("Ice duration (days)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#*Rolling sd vs. year####
gg.rollingwindow<-ggplot(data=Merge_singleRollingWindow)+
  geom_line(aes(x=year_median,y=sensSlope_fit),color="light grey",size=0.5)+
  geom_point(aes(x=year_median,y=LengthOfIceCover_days_sd),shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
  xlab(bquote(Year))+
  ylab("Ice duration (s.d.)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
#*Rolling sd residuals from sens slopes vs. year####
gg.residuals<-ggplot(data=Merge_singleRollingWindow,
       aes(x=year_median,y=sensSlope_residuals))+
  geom_smooth(method="gam", size=0.5,color=rgb(186,182,170,max=255),fill="sky blue")+
  geom_point(shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
  geom_errorbarh(aes(xmin=1953,xmax=1953+10,y=8))+
  xlab(bquote(Year))+
  ylab("Ice duration (s.d.)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
 
#goes left to right
#panel letter size
panel.size<-10
List<-list(gg.duration+
             #theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
             #scale_y_continuous(limits=c(10,10000),trans="log10",breaks=c(10,100,1000,10000),labels=trans_format("log10",math_format(10^.x)))+
             #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="d",fontface="bold"))+
             ggtitle("(a)"),
           gg.rollingwindow+
             #theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
             #scale_y_continuous(limits=c(1,10000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+ 
             ggtitle("(b)"),
           gg.residuals+
             #theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_text(size=10),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
             #scale_y_continuous(breaks=c(3,6,9))+ 
             #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="b",fontface="bold"))+
             ggtitle("(c)")
           )

#Plot them using patchwork####
(gg.3panel.patchwork.3x1<-wrap_plots(List,ncol = 3,nrow = 1)&theme(plot.margin = unit(c(3,3,3,3),"pt")))
#Could do a 3x3 with width 6, height = 5
ggsave(paste("figures/mohonkWinterLimno-FigureX-RollingWindow",window_select,"Years.jpg",sep=""), plot=gg.3panel.patchwork.3x1, width=6, height=2,units="in", dpi=300)

#Variance contribution calculation####
#Create dataframe all rolling windows####
datalist.variance=list()  #initialize empty list for storing a bunch of ice duration variabilities of different lengths

#Calculate a metric that is the residual^2 for each year, relative to the mean std####  
#*Loop through all the years####
  #debug: year_i<-71
for(year_i in 1:length(iceDuration_days_withNAs$Year)){
  
  #find the overall variance
  overall_var<-var(iceDuration_days_withNAs$LengthOfIceCover_days,na.rm=TRUE)
  #drop a year
  dropped_year<-tibble(iceDuration_days_withNAs)%>%filter(Year!=iceDuration_days_withNAs$Year[year_i])
  #calculate new variance - if the year had an NA value, then it should be the same
  dropped_var<-var(dropped_year$LengthOfIceCover_days,na.rm=TRUE)
  #Calculate teh residual for each year
  residual<-iceDuration_days_withNAs$LengthOfIceCover_days[year_i]-mean(iceDuration_days_withNAs$LengthOfIceCover_days,na.rm=TRUE)
  residual_squared<-residual^2
  
  #Calculate some fits
  #Figure out delta var (+ is increase, - is decrease)
  if(is.na(iceDuration_days_withNAs$LengthOfIceCover_days[year_i])){delta_var<-NA}else{delta_var<-overall_var-dropped_var}
  #Figure out % change
  percent_change_var<-100*(delta_var/overall_var)
  
  #Export each year to the datalist  
  #Store the temporary data frame in a list
  datalist.variance[[year_i]]<-tibble(year=iceDuration_days_withNAs$Year[year_i],overall_var=overall_var,dropped_var=dropped_var,delta_var=delta_var,percent_change_var=percent_change_var,residual=residual,residual_squared=residual_squared)  
  
}   

#*compile them all in one data frame####    
iceDuration_variability_metric<-do.call(bind_rows,datalist.variance) 
ggplot(data=iceDuration_variability_metric,aes(x=year,y=delta_var))+geom_point()
ggplot(data=iceDuration_variability_metric,aes(x=year,y=residual_squared))+geom_point()

#See the relationship between teleconnections and residuals
#Find the NAO simple moving average at the same scale as the window above
NAO_SMA<-tibble(
          year=as.vector(rollapply(NAO_summary%>%dplyr::select(water_year), width =window_select, FUN = function(x, na.rm = TRUE)  {median(x, na.rm = na.rm)})),
          NAO_SMA=as.vector(rollapply(NAO_summary%>%dplyr::select(NAO_index_winter), width =window_select, FUN = function(x, na.rm = TRUE)  {mean(x, na.rm = na.rm)})))
#Find the NAO simple moving average at the same scale as the window above
ENSO_SMA<-tibble(
  year=as.vector(rollapply(ENSO_summary%>%dplyr::select(water_year), width =window_select, FUN = function(x, na.rm = TRUE)  {median(x, na.rm = na.rm)})),
  ENSO_SMA=as.vector(rollapply(ENSO_summary%>%dplyr::select(ENSO_index_winter), width =window_select, FUN = function(x, na.rm = TRUE)  {mean(x, na.rm = na.rm)})))

#Plot the residuals plus the points and line for NAO and ENSO####
ggplot(data=NAO_summary,aes(x=water_year,y=NAO_index_winter))+geom_point()+
  geom_line(data=NAO_SMA,aes(x=year,y=NAO_SMA))+
  geom_line(data=ENSO_SMA,aes(x=year,y=ENSO_SMA*100),color="red")+
  geom_point(data=ENSO_summary,aes(x=water_year,y=ENSO_index_winter*100),color="red")+
  #geom_point(data=Merge_singleRollingWindow,aes(x=Year,y=sensSlope_residuals*10),color="blue")+
  geom_smooth(data=Merge_singleRollingWindow,aes(x=Year,y=sensSlope_residuals*10),method="gam")



#Sens slope for each of the three metrics####
sensSlope<-MTCC.sensSlope(iceDuration_temporary$year_median,iceDuration_temporary$LengthOfIceCover_days_sd)

#*Create a new data frame with extra columns####
MohonkIce_resids<-MohonkIce

#*Sens slope for each metric####
duration.sensSlope<-MTCC.sensSlope(x=MohonkIce_resids$Year,y=MohonkIce_resids$LengthOfIceCover_days)
iceIn.sensSlope<-MTCC.sensSlope(x=MohonkIce_resids$Year,y=MohonkIce_resids$IceInDayofYear_fed)
iceOut.sensSlope<-MTCC.sensSlope(x=MohonkIce_resids$Year,y=MohonkIce_resids$IceOutDayofYear_fed)

#*Calculate residuals from the mean
duration.resids<-MohonkIce_resids$LengthOfIceCover_days-mean(MohonkIce_resids$LengthOfIceCover_days,na.rm=TRUE)
iceIn.resids<-MohonkIce_resids$IceInDayofYear_fed-mean(MohonkIce_resids$IceInDayofYear_fed,na.rm=TRUE)
iceOut.resids<-MohonkIce_resids$IceOutDayofYear_fed-mean(MohonkIce_resids$IceOutDayofYear_fed,na.rm=TRUE)

#Bind the residuals from the sens slopes on as extra columns####
MohonkIce_resids<-bind_cols(tibble(MohonkIce_resids),
          tibble(duration_sens_resids=duration.sensSlope$residuals),
          tibble(iceIn_sens_resids=iceIn.sensSlope$residuals),
          tibble(iceOut_sens_resids=iceOut.sensSlope$residuals),
          tibble(duration_resids=duration.resids),
          tibble(iceIn_resids=iceIn.resids),
          tibble(iceOut_resids=iceOut.resids))

#*Visualize the sens slope residuals from all three metrics####
ggplot(data=MohonkIce_resids,aes(x=Year,y=iceIn_sens_resids))+geom_point()+
  geom_point(aes(y=iceOut_sens_resids),col="red")+
  geom_point(aes(y=duration_sens_resids),col="blue")+
  theme_bw()

#*Visualize the residuals from the mean from all three metrics####
ggplot(data=MohonkIce_resids,aes(x=Year,y=iceIn_resids))+geom_point()+
  geom_point(aes(y=iceOut_resids),col="red")+
  geom_point(aes(y=duration_resids),col="blue")+
  theme_bw()

#Next steps. Figure out ENSO/NAO for fall and spring####
  #Calculate fall,winter, Oct/Nov/Dec,Nov/Dec, Feb/Mar/Apr,Feb/Mar averages
ENSO_reduced<-ENSO_summary%>%
  dplyr::select(Year,water_year,ENSO_index_fall,ENSO_index_winter,ENSO_Oct,ENSO_Nov,ENSO_Dec,ENSO_Feb,ENSO_Mar,ENSO_Apr)%>%
  mutate(ENSO_OND=(ENSO_Oct+ENSO_Nov+ENSO_Dec)/3,ENSO_ND=(ENSO_Nov+ENSO_Dec)/2,ENSO_FMA=(ENSO_Feb+ENSO_Mar+ENSO_Apr)/3,ENSO_FM=(ENSO_Feb+ENSO_Mar)/2)
#NAO only has seasonal
NAO_reduced<-NAO_summary%>%
  dplyr::select(Year,water_year,NAO_index_fall,NAO_index_winter,NAO_index_spring)
#Convert the daily values into monthly averages
NAO_monthly<-NAO_daily%>%group_by(Year,Month)%>%summarize(NAO_index_monthly=mean(NAO_index))%>%
  mutate(water_year=ifelse(Month>=10,Year,Year-1))%>%print(n=25)

#Get out some needed months ONDFMA
NAO_Oct<-NAO_monthly%>%ungroup()%>%filter(Month==10)%>%mutate(NAO_Oct=NAO_index_monthly)%>%dplyr::select(water_year,NAO_Oct)
NAO_Nov<-NAO_monthly%>%ungroup()%>%filter(Month==11)%>%mutate(NAO_Nov=NAO_index_monthly)%>%dplyr::select(water_year,NAO_Nov)
NAO_Dec<-NAO_monthly%>%ungroup()%>%filter(Month==12)%>%mutate(NAO_Dec=NAO_index_monthly)%>%dplyr::select(water_year,NAO_Dec)
NAO_Feb<-NAO_monthly%>%ungroup()%>%filter(Month==2)%>%mutate(NAO_Feb=NAO_index_monthly)%>%dplyr::select(water_year,NAO_Feb)
NAO_Mar<-NAO_monthly%>%ungroup()%>%filter(Month==3)%>%mutate(NAO_Mar=NAO_index_monthly)%>%dplyr::select(water_year,NAO_Mar)
NAO_Apr<-NAO_monthly%>%ungroup()%>%filter(Month==4)%>%mutate(NAO_Apr=NAO_index_monthly)%>%dplyr::select(water_year,NAO_Apr)

#merge all those months together
NAO_Monthly_wide<-left_join(NAO_Oct,NAO_Nov,by="water_year")%>%
  left_join(.,NAO_Dec,by="water_year")%>%
  left_join(.,NAO_Feb,by="water_year")%>%
  left_join(.,NAO_Mar,by="water_year")%>%
  left_join(.,NAO_Apr,by="water_year")%>%
  mutate(NAO_OND=(NAO_Oct+NAO_Nov+NAO_Nov)/3,NAO_ND=(NAO_Nov+NAO_Nov)/2,NAO_FMA=(NAO_Feb+NAO_Mar+NAO_Apr)/3,NAO_FM=(NAO_Feb+NAO_Mar)/2) #Create new multi-month indices 


#Global climate anomaly
ClimateAnom_Oct<-NOAA_anomaly_monthly%>%ungroup()%>%filter(Month==10)%>%mutate(Anomaly_Oct=GlobalTempanomaly_C,water_year=ifelse(Month>=10,Year,Year-1))%>%dplyr::select(water_year,Anomaly_Oct)
ClimateAnom_Nov<-NOAA_anomaly_monthly%>%ungroup()%>%filter(Month==11)%>%mutate(Anomaly_Nov=GlobalTempanomaly_C,water_year=ifelse(Month>=10,Year,Year-1))%>%dplyr::select(water_year,Anomaly_Nov)
ClimateAnom_Dec<-NOAA_anomaly_monthly%>%ungroup()%>%filter(Month==12)%>%mutate(Anomaly_Dec=GlobalTempanomaly_C,water_year=ifelse(Month>=10,Year,Year-1))%>%dplyr::select(water_year,Anomaly_Dec)
ClimateAnom_Jan<-NOAA_anomaly_monthly%>%ungroup()%>%filter(Month==1)%>%mutate(Anomaly_Jan=GlobalTempanomaly_C,water_year=ifelse(Month>=10,Year,Year-1))%>%dplyr::select(water_year,Anomaly_Jan)
ClimateAnom_Feb<-NOAA_anomaly_monthly%>%ungroup()%>%filter(Month==2)%>%mutate(Anomaly_Feb=GlobalTempanomaly_C,water_year=ifelse(Month>=10,Year,Year-1))%>%dplyr::select(water_year,Anomaly_Feb)
ClimateAnom_Mar<-NOAA_anomaly_monthly%>%ungroup()%>%filter(Month==3)%>%mutate(Anomaly_Mar=GlobalTempanomaly_C,water_year=ifelse(Month>=10,Year,Year-1))%>%dplyr::select(water_year,Anomaly_Mar)
ClimateAnom_Apr<-NOAA_anomaly_monthly%>%ungroup()%>%filter(Month==4)%>%mutate(Anomaly_Apr=GlobalTempanomaly_C,water_year=ifelse(Month>=10,Year,Year-1))%>%dplyr::select(water_year,Anomaly_Apr)

#merge all those months together
ClimateAnom_Monthly_wide<-left_join(ClimateAnom_Oct,ClimateAnom_Nov,by="water_year")%>%
  left_join(.,ClimateAnom_Dec,by="water_year")%>%
  left_join(.,ClimateAnom_Jan,by="water_year")%>%
  left_join(.,ClimateAnom_Feb,by="water_year")%>%
  left_join(.,ClimateAnom_Mar,by="water_year")%>%
  left_join(.,ClimateAnom_Apr,by="water_year")%>%
  mutate(Anomaly_OND=(Anomaly_Oct+Anomaly_Nov+Anomaly_Nov)/3,Anomaly_ND=(Anomaly_Nov+Anomaly_Nov)/2,Anomaly_FMA=(Anomaly_Feb+Anomaly_Mar+Anomaly_Apr)/3,Anomaly_FM=(Anomaly_Feb+Anomaly_Mar)/2) #Create new multi-month indices 

###########BELLA NEEDS TO CHECK HERE TO MAKE SURE I AM DOING THE WATER YEAR STUFF CONSISTENTLY#####################
#Merge with the seasonal data
NAO_reduced2<-left_join(NAO_reduced,NAO_Monthly_wide,by="water_year")

#Join the resids together with the ENSO and NAO results####
MohonkIce_resids_tele<-left_join(MohonkIce_resids%>%mutate(water_year=Year-1),ENSO_reduced,by=c("water_year","Year"))%>%
  left_join(.,NAO_reduced2,by=c("water_year","Year"))%>%
  left_join(.,ClimateAnom_Monthly_wide,by=c("water_year"))

#Visualize some of the relationships
#Check Ice In residuals vs. fall teleconnections
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_ND,y=iceIn_sens_resids))+geom_point()+geom_smooth(method="lm")
cor.test(MohonkIce_resids_tele$iceIn_sens_resids,MohonkIce_resids_tele$NAO_ND)
  #For Ice In - look at ENSO_ND and NAO_ND in auto_arima
#Check Ice out residuals vs. spring teleconnections
ggplot(data=MohonkIce_resids_tele,aes(x=ENSO_FM,y=iceOut_sens_resids))+geom_point()+geom_smooth(method="lm")
cor.test(MohonkIce_resids_tele$iceIn_sens_resids,MohonkIce_resids_tele$ENSO_Feb)
#For Ice In - look at ENSO_FM, ENSO_Feb and NAO_Apr in auto_arima

#Check duration residuals vs. spring teleconnections
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_Nov,y=duration_sens_resids))+geom_point()+geom_smooth(method="lm")
cor.test(MohonkIce_resids_tele$duration_sens_resids,MohonkIce_resids_tele$NAO_Nov)
#For Ice In - look at ENSO_FM, ENSO_Feb and NAO_Apr in auto_arima

#Ice in Fit auto.arima with xreg as year####
arima.IceIn<-auto.arima(MohonkIce_resids_tele$iceIn_sens_resids,
#xreg=c(iceDuration_temporary$year_median),
 xreg=cbind(MohonkIce_resids_tele$ENSO_ND,
            MohonkIce_resids_tele$NAO_ND),
seasonal=FALSE,allowdrift = FALSE,
stationary=TRUE)

arima.IceIn #print out model
(1-pnorm(abs(arima.IceIn$coef)/sqrt(diag(arima.IceIn$var.coef))))*2 #P-values of coefficients

#Biplot with two different indices and color as Ice In residuals
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_ND,y=ENSO_ND,fill=iceIn_sens_resids,size=iceIn_sens_resids))+geom_point(shape=21,color="black")+
  scale_fill_gradientn(colours=c("blue","cyan","white", "yellow","red"))


#Ice out Fit auto.arima with xreg as year####
arima.IceOut<-auto.arima(MohonkIce_resids_tele$iceOut_sens_resids,
           #xreg=c(iceDuration_temporary$year_median),
           xreg=cbind(MohonkIce_resids_tele$ENSO_Apr,
                      MohonkIce_resids_tele$NAO_Apr),
           seasonal=FALSE,allowdrift = FALSE,
           stationary=TRUE)
arima.IceOut
(1-pnorm(abs(arima.IceOut$coef)/sqrt(diag(arima.IceOut$var.coef))))*2


#Ice out Fit auto.arima with xreg as year####
arima.duration<-auto.arima(MohonkIce_resids_tele$duration_sens_resids,
                         #xreg=c(iceDuration_temporary$year_median),
                         xreg=cbind(
                                    MohonkIce_resids_tele$NAO_Nov,
                                    
                                    
                                    
                                    ),
                         seasonal=FALSE,allowdrift = FALSE,
                         stationary=TRUE)
arima.duration
(1-pnorm(abs(arima.duration$coef)/sqrt(diag(arima.duration$var.coef))))*2

#Biplot with two different indices and color as Ice In residuals
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_ND,y=ENSO_ND,fill=duration_sens_resids,size=duration_sens_resids))+geom_point(shape=21,color="black")+
  scale_fill_gradientn(colours=rev(c("blue","cyan","white", "yellow","red")))



#Examine all the pairwise correlations of each of the different plots with our climate metrics####
#*Duration correlations####
duration.correlation <- MohonkIce_resids_tele %>% 
  select_if(is.numeric) %>%
  gather(Column, Value, -duration_resids) %>%
  group_by(Column) %>%
  nest() %>%
  mutate(Cor = map(data, ~cor.test(.x$Value, .x$duration_resids, method = "spearman"))) %>%
  mutate(Estimate = round(map_dbl(Cor, "estimate"), 2), 
         P_Value = map_dbl(Cor, "p.value"))%>% 
  select(-data, -Cor)%>%
  mutate(Significance = case_when(
    P_Value < 0.001  ~ "*** <0,001",
    P_Value < 0.01   ~ "** <0,01",
    P_Value < 0.05   ~ "*<0,05",
    TRUE             ~ "Not Significant"
  ))%>%arrange(-abs(Estimate))%>%print(n=Inf) #Rank by the correlation coefficient

#*put in some of the candidates for auto.arima####
#Ice out Fit auto.arima with xreg as year####
arima.duration<-auto.arima(MohonkIce_resids_tele$duration_resids,
                           #xreg=c(iceDuration_temporary$year_median),
                           xreg=cbind(
                             MohonkIce_resids_tele$NAO_ND,
                             MohonkIce_resids_tele$Anomaly_FMA
                             
                             
                           ),
                           seasonal=FALSE,allowdrift = FALSE,
                           stationary=TRUE)
arima.duration
(1-pnorm(abs(arima.duration$coef)/sqrt(diag(arima.duration$var.coef))))*2

#Biplot with two different indices and color as ice duration residuals
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_ND,y=Anomaly_FMA,fill=duration_resids,size=duration_resids))+geom_point(shape=21,color="black")+
  scale_fill_gradientn(colours=rev(c("blue","cyan","white", "yellow","red")))+theme_bw()
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_ND,y=duration_resids))+geom_point()+theme_bw()
ggplot(data=MohonkIce_resids_tele,aes(x=Anomaly_FMA,y=duration_resids))+geom_point()+theme_bw()


#*Ice In correlations####
iceIn.correlation <- MohonkIce_resids_tele %>% 
  select_if(is.numeric) %>%
  gather(Column, Value, -iceIn_resids) %>%
  group_by(Column) %>%
  nest() %>%
  mutate(Cor = map(data, ~cor.test(.x$Value, .x$iceIn_resids, method = "spearman"))) %>%
  mutate(Estimate = round(map_dbl(Cor, "estimate"), 2), 
         P_Value = map_dbl(Cor, "p.value"))%>% 
  select(-data, -Cor)%>%
  mutate(Significance = case_when(
    P_Value < 0.001  ~ "*** <0,001",
    P_Value < 0.01   ~ "** <0,01",
    P_Value < 0.05   ~ "*<0,05",
    TRUE             ~ "Not Significant"
  ))%>%arrange(-abs(Estimate))%>%print(n=Inf) #Rank by the correlation coefficient

#*put in some of the candidates for auto.arima####
#Ice out Fit auto.arima with xreg as year####
arima.iceIn<-auto.arima(MohonkIce_resids_tele$iceIn_resids,
                           #xreg=c(iceDuration_temporary$year_median),
                           xreg=cbind(
                             MohonkIce_resids_tele$NAO_Nov,
                             MohonkIce_resids_tele$NAO_Dec
                             
                             
                           ),
                           seasonal=FALSE,allowdrift = FALSE,
                           stationary=TRUE)
arima.iceIn
(1-pnorm(abs(arima.iceIn$coef)/sqrt(diag(arima.iceIn$var.coef))))*2

#Biplot with two different indices and color as ice duration residuals
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_Nov,y=NAO_Dec,fill=iceIn_resids,size=iceIn_resids))+geom_point(shape=21,color="black")+
  scale_fill_gradientn(colours=c("blue","cyan","white", "yellow","red"))+theme_bw()
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_Nov,y=iceIn_resids))+geom_point()+theme_bw()
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_Dec,y=iceIn_resids))+geom_point()+theme_bw()


#*Ice out correlations####
iceOut.correlation <- MohonkIce_resids_tele %>% 
  select_if(is.numeric) %>%
  gather(Column, Value, -iceOut_resids) %>%
  group_by(Column) %>%
  nest() %>%
  mutate(Cor = map(data, ~cor.test(.x$Value, .x$iceOut_resids, method = "spearman"))) %>%
  mutate(Estimate = round(map_dbl(Cor, "estimate"), 2), 
         P_Value = map_dbl(Cor, "p.value"))%>% 
  select(-data, -Cor)%>%
  mutate(Significance = case_when(
    P_Value < 0.001  ~ "*** <0,001",
    P_Value < 0.01   ~ "** <0,01",
    P_Value < 0.05   ~ "*<0,05",
    TRUE             ~ "Not Significant"
  ))%>%arrange(-abs(Estimate))%>%print(n=Inf) #Rank by the correlation coefficient

#*put in some of the candidates for auto.arima####
#Ice out Fit auto.arima with xreg as year####
arima.iceIn<-auto.arima(MohonkIce_resids_tele$iceIn_resids,
                        #xreg=c(iceDuration_temporary$year_median),
                        xreg=cbind(
                          MohonkIce_resids_tele$NAO_Nov,
                          MohonkIce_resids_tele$NAO_Dec
                          
                          
                        ),
                        seasonal=FALSE,allowdrift = FALSE,
                        stationary=TRUE)
arima.iceIn
(1-pnorm(abs(arima.iceIn$coef)/sqrt(diag(arima.iceIn$var.coef))))*2

#Biplot with two different indices and color as ice duration residuals
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_Nov,y=NAO_Dec,fill=iceIn_resids,size=iceIn_resids))+geom_point(shape=21,color="black")+
  scale_fill_gradientn(colours=c("blue","cyan","white", "yellow","red"))+theme_bw()
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_Nov,y=iceIn_resids))+geom_point()+theme_bw()
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_Dec,y=iceIn_resids))+geom_point()+theme_bw()

#STOPPED HERE######
#**GENERATE NAO and ENSO indices for certain months
#**CORRELATE THE SEN SLOPE OR MEAN RESIDUALS WITH THOSE INDICES
#**INCLUDE CLIMATE ANOMALY??

#Questions remain:
  #periodicity in the residuals? Is it a residuals of the rolling window?
  #GARCH models in econ get at volatility: https://www.idrisstsafack.com/post/garch-models-with-r-programming-a-practical-example-with-tesla-stock