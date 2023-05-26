#Script 04-VariabilityRollingWindow.R####
#Explore the rolling window iice phenology variability for Mohonk Lake
#Created 16Mar2022, by David Richardson (DCR)

#Run the main script to bring in all data and functions####
source('00_main.R')

#Libraries
if (!require(zoo)) {install.packages("zoo")}
if(!require(patchwork)){install.packages("patchwork")}
if(!require(forecast)){install.packages("forecast")}
if(!require(corrplot)){install.packages("corrplot")}
if(!require(MTS)){install.packages("MTS")}

#Try using zoo package
library(zoo)
library(patchwork) #laying out multipanel plots with the same size
library(forecast)
library(corrplot)
library(MTS)

summarize <- dplyr::summarize

#Calculate moving average and bollinger bounds for each of teh ice phenology metrics####
#First calculate as days since Oct 1####
window_size<-10

#*Create a dataframe with the rolling mean and sd for rolling window size specified above####
singleRollingWindow<-tibble(year_median=as.vector(rollapply(MohonkIce%>%dplyr::select(Year), width=window_size, FUN = median)), #median year for that window
       rw_duration_days_sd=as.vector(rollapply(MohonkIce%>%dplyr::select(LengthOfIceCover_days), width =window_size, FUN = function(x, na.rm = TRUE)  {sd(x, na.rm = na.rm)})), #rolling window standard deviation of length RW_length
       rw_iceIn_doy_wateryear_sd=as.vector(rollapply(MohonkIce%>%dplyr::select(IceInDayofYear_fed), width =window_size, FUN = function(x, na.rm = TRUE)  {sd(x, na.rm = na.rm)})), #rolling window standard deviation of length RW_length
       rw_iceOut_doy_wateryear_sd=as.vector(rollapply(MohonkIce%>%dplyr::select(IceOutDayofYear_fed), width =window_size, FUN = function(x, na.rm = TRUE)  {sd(x, na.rm = na.rm)})), #rolling window standard deviation of length RW_length
       rw_duration_days_mean=as.vector(rollapply(MohonkIce%>%dplyr::select(LengthOfIceCover_days), width =window_size, FUN = function(x, na.rm = TRUE)  {mean(x, na.rm = na.rm)})), #rolling window mean of length RW_length
       rw_iceIn_doy_wateryear_mean=as.vector(rollapply(MohonkIce%>%dplyr::select(IceInDayofYear_fed), width =window_size, FUN = function(x, na.rm = TRUE)  {mean(x, na.rm = na.rm)})), #rolling window standard deviation of length RW_length
       rw_iceOut_doy_wateryear_mean=as.vector(rollapply(MohonkIce%>%dplyr::select(IceOutDayofYear_fed), width =window_size, FUN = function(x, na.rm = TRUE)  {mean(x, na.rm = na.rm)})), #rolling window standard deviation of length RW_length
        )

#*Duration Rolling Window Graph a single plot with rolling window SDs as shaded region####
#These are bollinger band in econ/finance: https://www.investopedia.com/terms/b/bollingerbands.asp
gg.duration_RW<-ggplot(data=singleRollingWindow)+
  geom_ribbon(aes(x=year_median,ymin=rw_duration_days_mean-rw_duration_days_sd,ymax=rw_duration_days_mean+rw_duration_days_sd),color="#dcdcdc",fill="#dcdcdc")+
  geom_line(aes(x=year_median,y=rw_duration_days_mean),color="black",size=1)+ #Moving average
  #geom_errorbar(aes(x=Year,ymin=min_sd,ymax=max_sd),size=1)+
  geom_point(data=MohonkIce,aes(x=Year,y=LengthOfIceCover_days),shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+ #actual days of ice cover
  ylab("Ice duration (days)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))

#*Ice In Rolling Window Graph a single plot with rolling window SDs as shaded region####
#These are bollinger band in econ/finance: https://www.investopedia.com/terms/b/bollingerbands.asp
gg.iceIn_RW<-ggplot(data=singleRollingWindow)+
  geom_ribbon(aes(x=year_median,ymin=rw_iceIn_doy_wateryear_mean-rw_iceIn_doy_wateryear_sd,ymax=rw_iceIn_doy_wateryear_mean+rw_iceIn_doy_wateryear_sd),color="#dcdcdc",fill="#dcdcdc")+
  geom_line(aes(x=year_median,y=rw_iceIn_doy_wateryear_mean),color="black",size=1)+ #Moving average
  #geom_errorbar(aes(x=Year,ymin=min_sd,ymax=max_sd),size=1)+
  geom_point(data=MohonkIce,aes(x=Year,y=IceInDayofYear_fed),shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+ #actual days of ice cover
  ylab("Ice-on date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))

#*Ice Out Rolling Window Graph a single plot with rolling window SDs as shaded region####
#These are bollinger band in econ/finance: https://www.investopedia.com/terms/b/bollingerbands.asp
gg.iceOut_RW<-ggplot(data=singleRollingWindow)+
  geom_ribbon(aes(x=year_median,ymin=rw_iceOut_doy_wateryear_mean-rw_iceOut_doy_wateryear_sd,ymax=rw_iceOut_doy_wateryear_mean+rw_iceOut_doy_wateryear_sd),color="#dcdcdc",fill="#dcdcdc")+
  geom_line(aes(x=year_median,y=rw_iceOut_doy_wateryear_mean),color="black",size=1)+ #Moving average
  #geom_errorbar(aes(x=Year,ymin=min_sd,ymax=max_sd),size=1)+
  geom_point(data=MohonkIce,aes(x=Year,y=IceOutDayofYear_fed),shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+ #actual days of ice cover
  ylab("Ice-off date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))



#consecutive ensemble analysis######

#Create a data list####
#Create dataframe non-rolling windows####
datalist_sequential=list()  #initialize empty list for storing a bunch of ice duration variabilities of different lengths
datalist_index=1 #initialize counter to tick through datalist

  #Creates a grouping variable that is a wee bit bigger than the original by rounding up
  grouping_var<-tibble(grouping_var=rep(1:ceiling((length(MohonkIce$Year)+window_size-1)/window_size), each=window_size))
  #loop through all possible arrangements of the segment_length
  #debug j.index=1
  for(j.index in 1:window_size){
    #start at j.index, remove the last segment_length-j.index
    grouping_var_sub<-slice(grouping_var,(j.index):(length(MohonkIce$Year)+j.index-1))
    #Merge with the data of interest
    temp_segments<-bind_cols(MohonkIce,grouping_var_sub)%>%
      group_by(grouping_var)%>%
      summarize(segment_start_year=min(Year),segment_end_year=max(Year),
                duration_sd_segment=sd(LengthOfIceCover_days,na.rm=TRUE),duration_number_nonNA=sum(!is.na(LengthOfIceCover_days)),duration_prop_nonNA=duration_number_nonNA/window_size,
                iceIn_sd_segment=sd(IceInDayofYear_fed,na.rm=TRUE),iceIn_number_nonNA=sum(!is.na(IceInDayofYear_fed)),iceIn_prop_nonNA=iceIn_number_nonNA/window_size,
                iceOut_sd_segment=sd(IceOutDayofYear_fed,na.rm=TRUE),iceOut_number_nonNA=sum(!is.na(IceOutDayofYear_fed)),iceOut_prop_nonNA=iceOut_number_nonNA/window_size)%>%
      mutate(duration_sd_segment=ifelse(duration_prop_nonNA>=0.75,duration_sd_segment,NA),
             iceIn_sd_segment=ifelse(iceIn_prop_nonNA>=0.75,iceIn_sd_segment,NA),
             iceOut_sd_segment=ifelse(iceOut_prop_nonNA>=0.75,iceOut_sd_segment,NA),
             segment_midpoint_year=(segment_start_year+segment_end_year)/2)
    #ggplot(data=temp_segments,aes(x=segment_midpoint_year,y=sd_segment))+geom_point()
    
    
    #Calculate slope using sen slopes; can always change here to lm
    sensSlope_duration<-MTCC.sensSlope(temp_segments$segment_midpoint_year,temp_segments$duration_sd_segment)
    sensSlope_iceIn<-MTCC.sensSlope(temp_segments$segment_midpoint_year,temp_segments$iceIn_sd_segment)
    sensSlope_iceOut<-MTCC.sensSlope(temp_segments$segment_midpoint_year,temp_segments$iceOut_sd_segment)
                        
    #Store the residuals and other sens slope fit stats in iceDuration_temporary  
    sensSlopes_temporaryStats<-temp_segments%>%
      mutate(duration_sensSlope_fit=segment_midpoint_year*sensSlope_duration$coefficients["Year"]+sensSlope_duration$coefficients["Intercept"],
             duration_sensSlope_slope=sensSlope_duration$coefficients["Year"],
             duration_sensSlope_intercept=sensSlope_duration$coefficients["Intercept"],
             duration_sensSlope_residuals=sensSlope_duration$residuals,
             duration_sensSlope_pval=sensSlope_duration$pval,
             duration_sensSlope_z_stat=sensSlope_duration$z_stat,
             duration_sensSlope_n=sensSlope_duration$n,
             iceIn_sensSlope_fit=segment_midpoint_year*sensSlope_iceIn$coefficients["Year"]+sensSlope_iceIn$coefficients["Intercept"],
             iceIn_sensSlope_slope=sensSlope_iceIn$coefficients["Year"],
             iceIn_sensSlope_intercept=sensSlope_iceIn$coefficients["Intercept"],
             iceIn_sensSlope_residuals=sensSlope_iceIn$residuals,
             iceIn_sensSlope_pval=sensSlope_iceIn$pval,
             iceIn_sensSlope_z_stat=sensSlope_iceIn$z_stat,
             iceIn_sensSlope_n=sensSlope_iceIn$n,
             iceOut_sensSlope_fit=segment_midpoint_year*sensSlope_iceOut$coefficients["Year"]+sensSlope_iceOut$coefficients["Intercept"],
             iceOut_sensSlope_slope=sensSlope_iceOut$coefficients["Year"],
             iceOut_sensSlope_intercept=sensSlope_iceOut$coefficients["Intercept"],
             iceOut_sensSlope_residuals=sensSlope_iceOut$residuals,
             iceOut_sensSlope_pval=sensSlope_iceOut$pval,
             iceOut_sensSlope_z_stat=sensSlope_iceOut$z_stat,
             iceOut_sensSlope_n=sensSlope_iceOut$n,
             segment_length=window_size,
             starting_index=j.index)
    #Export each Rollingwindow length to the datalist  
    datalist_sequential[[datalist_index]]<-sensSlopes_temporaryStats
    datalist_index<-datalist_index+1 #increment datalist forward by 1
  } #End of looping through the various starting positions

#*compile them all in one data frame####    
variability_sequential<-do.call(bind_rows,datalist_sequential) 
  
  #*Summarize one row for each segment_length and starting index####
  variability_sequential_fits<-
    variability_sequential%>%
    group_by(starting_index)%>%slice(1)

  datalist_sensfits=list()  #initialize empty list for storing a bunch of ice duration variabilities of different lengths
  #model.index=1
  #loop through to generate sens slope fits for each year, each segment_length, each fit location####
  for(model.index in 1:length(variability_sequential_fits$grouping_var)){
    year=seq(min(MohonkIce$Year),max(MohonkIce$Year))
    datalist_sensfits[[model.index]]<-tibble(year=year,
                                             duration_sensSlope_fit_interpolate=variability_sequential_fits$duration_sensSlope_slope[model.index]*year+variability_sequential_fits$duration_sensSlope_intercept[model.index],
                                             iceIn_sensSlope_fit_interpolate=variability_sequential_fits$iceIn_sensSlope_slope[model.index]*year+variability_sequential_fits$iceIn_sensSlope_intercept[model.index],
                                             iceOut_sensSlope_fit_interpolate=variability_sequential_fits$iceOut_sensSlope_slope[model.index]*year+variability_sequential_fits$iceOut_sensSlope_intercept[model.index],segment_length=variability_sequential_fits$segment_length[model.index],starting_index=variability_sequential_fits$starting_index[model.index]
                                             )
  }
  
  #*bind all the interpolated sens slope fits####
  sensSlopeFitsInterpolated_sequential<-do.call(bind_rows,datalist_sensfits)
  #calculate fit stats for each segment length
  #median, and percentiles
  sensSlopeFitsInterpolated_sequential<-sensSlopeFitsInterpolated_sequential%>%
    group_by(year)%>%
    summarize(duration_median_sensSlope_fit=median(duration_sensSlope_fit_interpolate,na.rm=TRUE),
              duration_q5_sensSlope_fit=quantile(duration_sensSlope_fit_interpolate,probs=0.05,na.rm=TRUE),
              duration_q95_sensSlope_fit=quantile(duration_sensSlope_fit_interpolate,probs=0.95,na.rm=TRUE),
              iceIn_median_sensSlope_fit=median(iceIn_sensSlope_fit_interpolate,na.rm=TRUE),
              iceIn_q5_sensSlope_fit=quantile(iceIn_sensSlope_fit_interpolate,probs=0.05,na.rm=TRUE),
              iceIn_q95_sensSlope_fit=quantile(iceIn_sensSlope_fit_interpolate,probs=0.95,na.rm=TRUE),
              iceOut_median_sensSlope_fit=median(iceOut_sensSlope_fit_interpolate,na.rm=TRUE),
              iceOut_q5_sensSlope_fit=quantile(iceOut_sensSlope_fit_interpolate,probs=0.05,na.rm=TRUE),
              iceOut_q95_sensSlope_fit=quantile(iceOut_sensSlope_fit_interpolate,probs=0.95,na.rm=TRUE))
  
  #Summarize some basic stats####
  #percentage increase from start to end of data set
  ((sensSlopeFitsInterpolated_sequential%>%filter(year==2022)/sensSlopeFitsInterpolated_sequential%>%filter(year==1932))-1)*100
        #median percentage increase of SD: duration increased by 132%, ice In by 84%, ice Out by 48%
        #proportion SD: duration 2.3 (more than doubled), ice In 1.8 (almost doubled), ice out 1.5 (increased by half) 
        #absolute increases: duration sd went from 11.8 to 27.3; ice In from 8.6 to 15.9, Ice out 8.2 to 12.2
  
  #*Duration: plot the median sens slope fits surrounded by 5 and 95 credible intervals####
  gg.duration_SD_sequential<-ggplot(sensSlopeFitsInterpolated_sequential,aes(x=year,y=duration_median_sensSlope_fit))+
    geom_ribbon(aes(ymin=duration_q5_sensSlope_fit,ymax=duration_q95_sensSlope_fit),fill="#dcdcdc",color="#dcdcdc")+
    #geom_ribbon(aes(ymin=q25_sensSlope_fit,ymax=q75_sensSlope_fit),alpha=0.1,fill="light grey",color="grey")+
    geom_line(size=1)+
    ylab("Duration sd (days)")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))
  
  #*Ice in: plot the median sens slope fits surrounded by 5 and 95 credible intervals####
  gg.iceIn_SD_sequential<-ggplot(sensSlopeFitsInterpolated_sequential,aes(x=year,y=iceIn_median_sensSlope_fit))+
    geom_ribbon(aes(ymin=iceIn_q5_sensSlope_fit,ymax=iceIn_q95_sensSlope_fit),fill="#dcdcdc",color="#dcdcdc")+
    #geom_ribbon(aes(ymin=q25_sensSlope_fit,ymax=q75_sensSlope_fit),alpha=0.1,fill="light grey",color="grey")+
    geom_line(size=1)+
    ylab("Ice-on sd (days)")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))
  
  #*Ice out: plot the median sens slope fits surrounded by 5 and 95 credible intervals####
  gg.iceOut_SD_sequential<-ggplot(sensSlopeFitsInterpolated_sequential,aes(x=year,y=iceOut_median_sensSlope_fit))+
    geom_ribbon(aes(ymin=iceOut_q5_sensSlope_fit,ymax=iceOut_q95_sensSlope_fit),fill="#dcdcdc",color="#dcdcdc")+
    #geom_ribbon(aes(ymin=q25_sensSlope_fit,ymax=q75_sensSlope_fit),alpha=0.1,fill="light grey",color="grey")+
    geom_line(size=1)+
    ylab("Ice-off sd (days)")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))

  #doy conversion to dates
  #october 1 is day 274 (1 water year doy) non leap year
  #Nov 1 is day 305 (32 water year doy)
  #Dec 1 is day 335 (62 water year doy)
  #Jan 1 is day 1 (93 water year doy)
  #Feb 1 is day 32 (124 water year doy)
  #Mar 1 is day 60 (152 water year doy)
  #Apr 1 is day 91 (183 water year doy)
  #May 1 is day 121 (213 water year doy)
  
  #goes left to right
  #panel letter size
  panel.size<-10
  List<-list(gg.iceIn_RW+
               #theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
               #scale_y_continuous(limits=c(10,10000),trans="log10",breaks=c(10,100,1000,10000),labels=trans_format("log10",math_format(10^.x)))+
               #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="d",fontface="bold"))+
               scale_x_continuous(limits=c(1931,2023),breaks = seq(1940, 2020, by = 20))+
               scale_y_continuous(limits=c(60,130),breaks=c(62,93,124),labels=c("01-Dec","01-Jan","01-Feb"))+
               theme(axis.text.x=element_blank(),axis.title.x=element_blank(),
                     plot.margin=unit(c(1.5,0.5,0.5,0.5), "lines"))+
               geom_text(aes(x=-Inf,y=Inf,hjust=-0.5,vjust=1.5,label="a")),
             gg.iceOut_RW+
               #theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
               #scale_y_continuous(limits=c(1,10000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+ 
               scale_x_continuous(limits=c(1931,2023),breaks = seq(1940, 2020, by = 20))+
               scale_y_continuous(limits=c(150,214),breaks=c(152,183,213),labels=c("01-Mar","01-Apr","01-May"))+
               theme(axis.text.x=element_blank(),axis.title.x=element_blank(),
                     plot.margin=unit(c(1.5,0.5,0.5,0.5), "lines"))+
               geom_text(aes(x=-Inf,y=Inf,hjust=-0.5,vjust=1.5,label="b")),
             gg.duration_RW+
               #theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_text(size=10),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
               #scale_y_continuous(breaks=c(3,6,9))+ 
               #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="b",fontface="bold"))+
               scale_x_continuous(limits=c(1931,2023),breaks = seq(1940, 2020, by = 20))+
               theme(axis.text.x=element_blank(),axis.title.x=element_blank(),
                     plot.margin=unit(c(1.5,0.5,0.5,0.5), "lines"))+
               geom_text(aes(x=-Inf,y=Inf,hjust=-0.5,vjust=1.5,label="c")),
             gg.iceIn_SD_sequential+
               scale_y_continuous(limits=c(5,28),breaks=c(5,15,25))+
               scale_x_continuous(limits=c(1931,2023),breaks = seq(1940, 2020, by = 20))+
               xlab("Year")+
               geom_text(aes(x=-Inf,y=Inf,hjust=-0.5,vjust=1.5,label="d"))+
               theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "lines")),
             gg.iceOut_SD_sequential+
               scale_y_continuous(limits=c(5,28),breaks=c(5,15,25))+
               scale_x_continuous(limits=c(1931,2023),breaks = seq(1940, 2020, by = 20))+
               xlab("Year")+
               geom_text(aes(x=-Inf,y=Inf,hjust=-0.5,vjust=1.5,label="e"))+
               theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "lines")),
             gg.duration_SD_sequential+
               scale_y_continuous(limits=c(5,28),breaks=c(5,15,25))+
               scale_x_continuous(limits=c(1931,2023),breaks = seq(1940, 2020, by = 20))+
               xlab("Year")+
               geom_text(aes(x=-Inf,y=Inf,hjust=-0.5,vjust=1.5,label="f"))+
               theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "lines"))
             )
  
  #Plot them using patchwork####
  (gg.3panel.variability<-wrap_plots(List,ncol = 3,nrow = 2)&theme(plot.margin = unit(c(4,3,3,3),"pt")))
  #Could do a 3x3 with width 6, height = 5
  ggsave(paste("figures/Fig2.Variability6panels.jpg",sep=""), plot=gg.3panel.variability, width=7, height=4,units="in", dpi=300)
  
  
  
  
  
  
    
  
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
  dplyr::select(water_year,ENSO_index_fall,ENSO_index_winter,ENSO_Oct,ENSO_Nov,ENSO_Dec,ENSO_Feb,ENSO_Mar,ENSO_Apr)%>%
  mutate(ENSO_OND=(ENSO_Oct+ENSO_Nov+ENSO_Dec)/3,ENSO_ND=(ENSO_Nov+ENSO_Dec)/2,ENSO_FMA=(ENSO_Feb+ENSO_Mar+ENSO_Apr)/3,ENSO_FM=(ENSO_Feb+ENSO_Mar)/2)
#NAO only has seasonal
NAO_reduced<-NAO_summary%>%
  dplyr::select(water_year,NAO_index_fall,NAO_index_winter,NAO_index_spring)
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

#Merge with the seasonal data
NAO_reduced2<-left_join(NAO_reduced,NAO_Monthly_wide,by="water_year")

#Join the resids together with the ENSO and NAO results####
MohonkIce_resids_tele<-left_join(MohonkIce_resids%>%mutate(water_year=Year-1),ENSO_reduced,by=c("water_year"))%>%
  left_join(.,NAO_reduced2,by=c("water_year"))%>%
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
                                    MohonkIce_resids_tele$NAO_Nov
                                    
                                    
                                    
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

#Alternative window analysis######
  #consecutive windows##

#Create a data list####
#Create dataframe non-rolling windows####
datalist_sequential=list()  #initialize empty list for storing a bunch of ice duration variabilities of different lengths
datalist_index=1 #initialize counter to tick through datalist

#how many years are in each segment
  #segment_length=30
for(segment_length in 3:30){
#Creates a grouping variable that is a wee bit bigger than the original by rounding up
grouping_var<-tibble(grouping_var=rep(1:ceiling((length(iceDuration_days_withNAs$Year)+segment_length-1)/segment_length), each=segment_length))
#loop through all possible arrangements of the segment_length
#debug j.index=20
for(j.index in 1:segment_length){
  #start at j.index, remove the last segment_length-j.index
  grouping_var_sub<-slice(grouping_var,(j.index):(length(iceDuration_days_withNAs$Year)+j.index-1))
  #Merge with the data of interest
  temp_segments<-bind_cols(iceDuration_days_withNAs,grouping_var_sub)%>%
                  group_by(grouping_var)%>%
                  summarize(segment_start_year=min(Year),segment_end_year=max(Year),sd_segment=sd(LengthOfIceCover_days,na.rm=TRUE),number_nonNA=sum(!is.na(LengthOfIceCover_days)),prop_nonNA=number_nonNA/segment_length)%>%
                  mutate(sd_segment=ifelse(prop_nonNA>=0.75,sd_segment,NA),
                         segment_midpoint_year=(segment_start_year+segment_end_year)/2)
  #ggplot(data=temp_segments,aes(x=segment_midpoint_year,y=sd_segment))+geom_point()
  
  
  #Calculate slope
  sensSlope_sequential<-MTCC.sensSlope(temp_segments$segment_midpoint_year,temp_segments$sd_segment)
  #Store the residuals and other sens slope fit stats in iceDuration_temporary  
  sensSlopes_temporaryStats<-temp_segments%>%
    mutate(sensSlope_fit=segment_midpoint_year*sensSlope_sequential$coefficients["Year"]+sensSlope_sequential$coefficients["Intercept"],
           sensSlope_slope=sensSlope_sequential$coefficients["Year"],
           sensSlope_intercept=sensSlope_sequential$coefficients["Intercept"],
           sensSlope_residuals=sensSlope_sequential$residuals,
           sensSlope_pval=sensSlope_sequential$pval,
           sensSlope_z_stat=sensSlope_sequential$z_stat,
           sensSlope_n=sensSlope_sequential$n,
           segment_length=segment_length,
           starting_index=j.index)
  #Export each Rollingwindow length to the datalist  
  datalist_sequential[[datalist_index]]<-sensSlopes_temporaryStats
  datalist_index<-datalist_index+1 #increment datalist forward by 1
} #End of looping through the various starting positions
} #End of looping through the different segment lengths
#*compile them all in one data frame####    
iceDuration_variability_sequential<-do.call(bind_rows,datalist_sequential) 

#Unpack some results
#Plot segment sd with starting position as a color
#this kind of gets at the rolling sd with all of them together
iceDuration_variability_sequential%>%filter(segment_length==4)%>%ggplot(.,aes(x=segment_midpoint_year,y=sd_segment,color=as.factor(starting_index)))+geom_point()
iceDuration_variability_sequential%>%filter(segment_length==9)%>%ggplot(.,aes(x=segment_midpoint_year,y=sd_segment,color=as.factor(starting_index)))+geom_point()
iceDuration_variability_sequential%>%filter(segment_length==20)%>%ggplot(.,aes(x=segment_midpoint_year,y=sd_segment,color=as.factor(starting_index)))+geom_point()

#Plot segment sd fit with starting position as a color
iceDuration_variability_sequential%>%filter(segment_length==4)%>%ggplot(.,aes(x=segment_midpoint_year,y=sensSlope_fit,color=as.factor(starting_index)))+geom_line()
iceDuration_variability_sequential%>%filter(segment_length==9)%>%ggplot(.,aes(x=segment_midpoint_year,y=sensSlope_fit,color=as.factor(starting_index)))+geom_line()
iceDuration_variability_sequential%>%filter(segment_length==20)%>%ggplot(.,aes(x=segment_midpoint_year,y=sensSlope_fit,color=as.factor(starting_index)))+geom_line()

#Make into 
iceDuration_variability_sequential_4years<-left_join(iceDuration_variability_sequential%>%filter(segment_length==4),iceDuration_variability_sequential%>%filter(grouping_var==1,segment_length==4)%>%mutate(YearOfStart=segment_end_year+1)%>%dplyr::select(starting_index,YearOfStart),by="starting_index")  #%>%arrange(Min_segment_start_year,grouping_var)
iceDuration_variability_sequential_9years<-left_join(iceDuration_variability_sequential%>%filter(segment_length==9),iceDuration_variability_sequential%>%filter(grouping_var==1,segment_length==9)%>%mutate(YearOfStart=segment_end_year+1)%>%dplyr::select(starting_index,YearOfStart),by="starting_index")  
iceDuration_variability_sequential_13years<-left_join(iceDuration_variability_sequential%>%filter(segment_length==13),iceDuration_variability_sequential%>%filter(grouping_var==1,segment_length==13)%>%mutate(YearOfStart=segment_end_year+1)%>%dplyr::select(starting_index,YearOfStart),by="starting_index")  


#Plot segment sd with facet wrap by starting position
#plot with points and slope together
#13 seems to be the cutoff for having 5+ points
(gg.seq4<-ggplot(iceDuration_variability_sequential_4years,aes(x=segment_midpoint_year,y=sd_segment))+geom_point()+geom_line(aes(y=sensSlope_fit))+facet_wrap(vars(YearOfStart))+theme_bw()+ylab("Duration sd (days)")+xlab("Year"))
(gg.seq9<-ggplot(iceDuration_variability_sequential_9years,aes(x=segment_midpoint_year,y=sd_segment))+geom_point()+geom_line(aes(y=sensSlope_fit))+facet_wrap(vars(YearOfStart))+theme_bw()+ylab("Duration sd (days)")+xlab("Year"))
(gg.seq13<-ggplot(iceDuration_variability_sequential_13years,aes(x=segment_midpoint_year,y=sd_segment))+geom_point()+geom_line(aes(y=sensSlope_fit))+facet_wrap(vars(YearOfStart))+theme_bw()+ylab("Duration sd (days)")+xlab("Year"))

#Print out each of these
ggsave(paste("figures/FigXSupplement_Sequential4years.jpg",sep=""), plot=gg.seq4, width=6, height=5,units="in", dpi=300)
ggsave(paste("figures/FigXSupplement_Sequential9years.jpg",sep=""), plot=gg.seq9, width=6, height=5,units="in", dpi=300)
ggsave(paste("figures/FigXSupplement_Sequential13years.jpg",sep=""), plot=gg.seq13, width=6, height=5,units="in", dpi=300)


#Find the significant sens slopes
  #note, there are only 2
iceDuration_variability_sequential%>%filter(sensSlope_pval<0.05)%>%ggplot(.,aes(x=segment_midpoint_year,y=sensSlope_fit,color=as.factor(segment_length)))+geom_line()

#Graph them all, facet_wrap by segment length, filter to 13 years segment length or less
iceDuration_variability_sequential%>%filter(segment_length<=13)%>%ggplot(.,aes(x=segment_midpoint_year,y=sensSlope_fit,color=as.factor(starting_index)))+geom_line()+facet_wrap(vars(segment_length))

#Summarize the stats and visualize
iceDuration_variability_sequential%>%
  group_by(segment_length)%>%
  summarize(sensSlope_median=median(sensSlope_slope,na.rm=TRUE))%>%
  print(n=Inf)%>%ggplot(.,aes(x=segment_length,y=sensSlope_median))+geom_point()

#Group by segment_length and starting index, 
#slice takes the first row of each to avoid redundancy as there are many slopes in each
#cut to 3 to 13 segment lengths, longer and there are just too few points in each slope
iceDuration_variability_sequential%>%
  group_by(segment_length,starting_index)%>%slice(1)%>%
  filter(segment_length<=13)%>%
  ggplot(.,aes(x=as.factor(segment_length),y=sensSlope_slope))+geom_boxplot()+geom_jitter()+theme_bw()

#Summarize one row for each segment_length and starting index
iceDuration_variability_sequential_fits<-
  iceDuration_variability_sequential%>%
  group_by(segment_length,starting_index)%>%slice(1)%>%
  filter(segment_length<=13)

datalist_sensfits=list()  #initialize empty list for storing a bunch of ice duration variabilities of different lengths
#model.index=1
#loop through to generate sens slope fits for each year, each segment_length, each fit location####
for(model.index in 1:length(iceDuration_variability_sequential_fits$sensSlope_slope)){
  year=seq(min(iceDuration_days_withNAs$Year),max(iceDuration_days_withNAs$Year))
  datalist_sensfits[[model.index]]<-tibble(year=year,sensSlope_fit_interpolate=iceDuration_variability_sequential_fits$sensSlope_slope[model.index]*year+iceDuration_variability_sequential_fits$sensSlope_intercept[model.index],segment_length=iceDuration_variability_sequential_fits$segment_length[model.index],starting_index=iceDuration_variability_sequential_fits$starting_index[model.index])
}

#*bind all the interpolated sens slope fits####
iceDuration_sensSlopeFitsInterpolated_sequential<-do.call(bind_rows,datalist_sensfits)
#calculate fit stats for each segment length
#median, and percentiles
temp<-iceDuration_sensSlopeFitsInterpolated_sequential%>%
  group_by(segment_length,year)%>%
  summarize(median_sensSlope_fit=median(sensSlope_fit_interpolate,na.rm=TRUE),
            q5_sensSlope_fit=quantile(sensSlope_fit_interpolate,probs=0.05,na.rm=TRUE),
            q25_sensSlope_fit=quantile(sensSlope_fit_interpolate,probs=0.25,na.rm=TRUE),
            q75_sensSlope_fit=quantile(sensSlope_fit_interpolate,probs=0.75,na.rm=TRUE),
            q95_sensSlope_fit=quantile(sensSlope_fit_interpolate,probs=0.95,na.rm=TRUE))
#*plot the median sens slope fits surrounded by 25, 75 and 5 and 95 ribbons####
ggplot(temp,aes(x=year,y=median_sensSlope_fit))+
  facet_wrap(vars(segment_length))+
  geom_ribbon(aes(ymin=q5_sensSlope_fit,ymax=q95_sensSlope_fit),alpha=0.1,fill="grey",color="dark grey")+
  #geom_ribbon(aes(ymin=q25_sensSlope_fit,ymax=q75_sensSlope_fit),alpha=0.1,fill="light grey",color="grey")+
  geom_line(size=1.2)+
  theme_bw()


#Questions remain:
  #periodicity in the residuals? Is it a residuals of the rolling window?
  #GARCH models in econ get at volatility: https://www.idrisstsafack.com/post/garch-models-with-r-programming-a-practical-example-with-tesla-stock