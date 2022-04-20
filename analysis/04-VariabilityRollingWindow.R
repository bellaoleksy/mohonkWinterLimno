#Script 04-VariabilityRollingWindow.R####
#Explore the rolling window in ice phenology variability for Mohonk Lake
#Created 16Mar2022, by David Richardson (DCR)

#Run the main script to bring in all data and functions####
source('00_main.R')

#Libraries
if (!require(zoo)) {install.packages("zoo")}
if(!require(patchwork)){install.packages("patchwork")}

#Try using zoo package
library(zoo)
library(patchwork) #laying out multipanel plots with the same size



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
  df_acf%>%filter(significant_acf=="*")%>%filter(acf_vals<0.98)%>%group_by(RollingWindow_years)%>%slice(which.max(abs(acf_vals)))
ggplot(data=df_acf_summary,aes(x=lag,y=common_lag))+geom_point()



#Is the variability increasing?#####
#This tests the slope of each time series and determines significance correcting for 27 comparisons
iceDuration_variability_summary<-iceDuration_variability_all%>%group_by(RollingWindow_years)%>%dplyr::summarize(sensSlope_pval=mean(sensSlope_pval),sensSlope_slope=mean(sensSlope_slope),sensSlope_intercept=mean(sensSlope_intercept),sensSlope_z_stat=mean(sensSlope_z_stat),sensSlope_n=mean(sensSlope_n))%>%mutate(significance=ifelse(sensSlope_pval<0.05/27,"*","NS"))%>%print(n=Inf)
#Is the slope different depending on the rolling window
ggplot(data=iceDuration_variability_summary,aes(y=sensSlope_slope,x=RollingWindow_years))+geom_point()


#Merge for a single plot by selecting a specifc rolling window and merging with original data
#Calculate Bollinger Bands by doing the moving average +/- moving sd for that window
window_select<-10 #set window here
Merge_singleRollingWindow<-left_join(tibble(iceDuration_days_withNAs),iceDuration_variability_all%>%filter(RollingWindow_years==window_select)%>%mutate(Year=trunc(year_median)),by="Year")%>%mutate(max_sd=LengthOfIceCover_days_mean+LengthOfIceCover_days_sd,min_sd=LengthOfIceCover_days_mean-LengthOfIceCover_days_sd)

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
NAO_SMA<-tibble(
          year=as.vector(rollapply(NAO_summary%>%dplyr::select(water_year), width =window_select, FUN = function(x, na.rm = TRUE)  {median(x, na.rm = na.rm)})),
          NAO_SMA=as.vector(rollapply(NAO_summary%>%dplyr::select(NAO_index_winter), width =window_select, FUN = function(x, na.rm = TRUE)  {median(x, na.rm = na.rm)})))
ggplot(data=NAO_summary,aes(x=water_year,y=NAO_index_winter))+geom_point()+
  geom_line(data=NAO_SMA,aes(x=year,y=NAO_SMA))+
  #geom_point(data=Merge_singleRollingWindow,
             #aes(x=year_median,y=sensSlope_residuals*20),color="blue")+
  geom_point(data=iceDuration_variability_metric,aes(x=year,y=residual_squared/20),color="red")

#STOPPED HERE######
#Questions remain:
  #periodicity in the residuals? Is it a residuals of the rolling window?
  #GARCH models in econ get at volatility: https://www.idrisstsafack.com/post/garch-models-with-r-programming-a-practical-example-with-tesla-stock