#FIGURE 2 -- ROLLING WINDOWS ######
#Explore the rolling window iice phenology variability for Mohonk Lake
#Created 16Mar2022, by David Richardson (DCR)

# >Run the main script to bring in all data and functions if you haven't already####
source('analysis/00_main.R')



# > Calculate moving average and bollinger bounds for each of teh ice phenology metrics####
#First calculate as days since Oct 1
window_size<-10

# > Create a dataframe with the rolling mean and sd for rolling window size specified above####
singleRollingWindow<-tibble(year_median=as.vector(rollapply(MohonkIce%>%dplyr::select(Year), width=window_size, FUN = median)), #median year for that window
       rw_duration_days_sd=as.vector(rollapply(MohonkIce%>%dplyr::select(LengthOfIceCover_days), width =window_size, FUN = function(x, na.rm = TRUE)  {sd(x, na.rm = na.rm)})), #rolling window standard deviation of length RW_length
       rw_iceIn_doy_wateryear_sd=as.vector(rollapply(MohonkIce%>%dplyr::select(IceInDayofYear_fed), width =window_size, FUN = function(x, na.rm = TRUE)  {sd(x, na.rm = na.rm)})), #rolling window standard deviation of length RW_length
       rw_iceOut_doy_wateryear_sd=as.vector(rollapply(MohonkIce%>%dplyr::select(IceOutDayofYear_fed), width =window_size, FUN = function(x, na.rm = TRUE)  {sd(x, na.rm = na.rm)})), #rolling window standard deviation of length RW_length
       rw_duration_days_mean=as.vector(rollapply(MohonkIce%>%dplyr::select(LengthOfIceCover_days), width =window_size, FUN = function(x, na.rm = TRUE)  {mean(x, na.rm = na.rm)})), #rolling window mean of length RW_length
       rw_iceIn_doy_wateryear_mean=as.vector(rollapply(MohonkIce%>%dplyr::select(IceInDayofYear_fed), width =window_size, FUN = function(x, na.rm = TRUE)  {mean(x, na.rm = na.rm)})), #rolling window standard deviation of length RW_length
       rw_iceOut_doy_wateryear_mean=as.vector(rollapply(MohonkIce%>%dplyr::select(IceOutDayofYear_fed), width =window_size, FUN = function(x, na.rm = TRUE)  {mean(x, na.rm = na.rm)})), #rolling window standard deviation of length RW_length
        )

# > Duration Rolling Window Graph a single plot with rolling window SDs as shaded region####
#These are bollinger band in econ/finance: https://www.investopedia.com/terms/b/bollingerbands.asp
gg.duration_RW<-ggplot(data=singleRollingWindow)+
  geom_ribbon(aes(x=year_median,ymin=rw_duration_days_mean-rw_duration_days_sd,ymax=rw_duration_days_mean+rw_duration_days_sd),color="darkgrey",fill="lightgrey")+
  geom_line(aes(x=year_median,y=rw_duration_days_mean),color="black",size=1)+ #Moving average
  #geom_errorbar(aes(x=Year,ymin=min_sd,ymax=max_sd),size=1)+
  geom_point(data=MohonkIce,aes(x=Year,y=LengthOfIceCover_days),shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+ #actual days of ice cover
  ylab("Ice duration (days)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))

# > Ice In Rolling Window Graph a single plot with rolling window SDs as shaded region####
#These are bollinger band in econ/finance: https://www.investopedia.com/terms/b/bollingerbands.asp
gg.iceIn_RW<-ggplot(data=singleRollingWindow)+
  geom_ribbon(aes(x=year_median,ymin=rw_iceIn_doy_wateryear_mean-rw_iceIn_doy_wateryear_sd,ymax=rw_iceIn_doy_wateryear_mean+rw_iceIn_doy_wateryear_sd),color="darkgrey",fill="lightgrey")+
  geom_line(aes(x=year_median,y=rw_iceIn_doy_wateryear_mean),color="black",size=1)+ #Moving average
  #geom_errorbar(aes(x=Year,ymin=min_sd,ymax=max_sd),size=1)+
  geom_point(data=MohonkIce,aes(x=Year,y=IceInDayofYear_fed),shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+ #actual days of ice cover
  ylab("Ice-on date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))

# > Ice Out Rolling Window Graph a single plot with rolling window SDs as shaded region####
#These are bollinger band in econ/finance: https://www.investopedia.com/terms/b/bollingerbands.asp
gg.iceOut_RW<-ggplot(data=singleRollingWindow)+
  geom_ribbon(aes(x=year_median,ymin=rw_iceOut_doy_wateryear_mean-rw_iceOut_doy_wateryear_sd,ymax=rw_iceOut_doy_wateryear_mean+rw_iceOut_doy_wateryear_sd),color="darkgrey",fill="lightgrey")+
  geom_line(aes(x=year_median,y=rw_iceOut_doy_wateryear_mean),color="black",size=1)+ #Moving average
  #geom_errorbar(aes(x=Year,ymin=min_sd,ymax=max_sd),size=1)+
  geom_point(data=MohonkIce,aes(x=Year,y=IceOutDayofYear_fed),shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+ #actual days of ice cover
  ylab("Ice-off date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))



# > consecutive ensemble analysis######

#Create a data list
#Create dataframe non-rolling windows
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

#*compile them all in one data frame
variability_sequential<-do.call(bind_rows,datalist_sequential) 
  
  #*Summarize one row for each segment_length and starting index
  variability_sequential_fits<-
    variability_sequential%>%
    group_by(starting_index)%>%slice(1)

  datalist_sensfits=list()  #initialize empty list for storing a bunch of ice duration variabilities of different lengths
  #model.index=1
  #loop through to generate sens slope fits for each year, each segment_length, each fit location
  for(model.index in 1:length(variability_sequential_fits$grouping_var)){
    year=seq(min(MohonkIce$Year),max(MohonkIce$Year))
    datalist_sensfits[[model.index]]<-tibble(year=year,
                                             duration_sensSlope_fit_interpolate=variability_sequential_fits$duration_sensSlope_slope[model.index]*year+variability_sequential_fits$duration_sensSlope_intercept[model.index],
                                             iceIn_sensSlope_fit_interpolate=variability_sequential_fits$iceIn_sensSlope_slope[model.index]*year+variability_sequential_fits$iceIn_sensSlope_intercept[model.index],
                                             iceOut_sensSlope_fit_interpolate=variability_sequential_fits$iceOut_sensSlope_slope[model.index]*year+variability_sequential_fits$iceOut_sensSlope_intercept[model.index],segment_length=variability_sequential_fits$segment_length[model.index],starting_index=variability_sequential_fits$starting_index[model.index]
                                             )
  }
  
  #*bind all the interpolated sens slope fits
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
  
  #Summarize some basic stats
  #percentage increase from start to end of data set
  ((sensSlopeFitsInterpolated_sequential%>%filter(year==2022)/sensSlopeFitsInterpolated_sequential%>%filter(year==1932))-1)*100
        #median percentage increase of SD: duration increased by 132%, ice In by 84%, ice Out by 48%
        #proportion SD: duration 2.3 (more than doubled), ice In 1.8 (almost doubled), ice out 1.5 (increased by half) 
        #absolute increases: duration sd went from 11.8 to 27.3; ice In from 8.6 to 15.9, Ice out 8.2 to 12.2
  
  # ** Duration panel: plot the median sens slope fits surrounded by 5 and 95 credible intervals####
  gg.duration_SD_sequential<-ggplot(sensSlopeFitsInterpolated_sequential,aes(x=year,y=duration_median_sensSlope_fit))+
    geom_ribbon(aes(ymin=duration_q5_sensSlope_fit,ymax=duration_q95_sensSlope_fit),fill="#dcdcdc",color="#dcdcdc")+
    #geom_ribbon(aes(ymin=q25_sensSlope_fit,ymax=q75_sensSlope_fit),alpha=0.1,fill="light grey",color="grey")+
    geom_line(size=1)+
    ylab("Duration sd (days)")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))
  
  #** Ice in panel: plot the median sens slope fits surrounded by 5 and 95 credible intervals####
  gg.iceIn_SD_sequential<-ggplot(sensSlopeFitsInterpolated_sequential,aes(x=year,y=iceIn_median_sensSlope_fit))+
    geom_ribbon(aes(ymin=iceIn_q5_sensSlope_fit,ymax=iceIn_q95_sensSlope_fit),fill="#dcdcdc",color="#dcdcdc")+
    #geom_ribbon(aes(ymin=q25_sensSlope_fit,ymax=q75_sensSlope_fit),alpha=0.1,fill="light grey",color="grey")+
    geom_line(size=1)+
    ylab("Ice-on sd (days)")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))
  
  #** Ice out panel: plot the median sens slope fits surrounded by 5 and 95 credible intervals####
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
  

