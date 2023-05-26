#Script 05-TrendsInWinterWaterTemperature.R####
#Explore the winter water temperature and associated metrics for Mohonk Lake
#Created 15Jul2022, by David Richardson (DCR)

#Run the main script to bring in all data and functions####
source('analysis/04-VariabilityRollingWindow.R')

#Libraries
if (!require(zoo)) {install.packages("zoo")}
if(!require(patchwork)){install.packages("patchwork")}
if(!require(forecast)){install.packages("forecast")}
if(!require(corrplot)){install.packages("corrplot")}
if(!require(MTS)){install.packages("MTS")}
if(!require(ggpubr)){install.packages("ggpubr")}

#Try using zoo package
library(zoo)
library(patchwork) #laying out multipanel plots with the same size
library(forecast)
library(corrplot)
library(MTS)
library(ggpubr)

#consecutive ensemble analysis######

#Create a data list####
#Create dataframe non-rolling windows####
datalist_sequential=list()  #initialize empty list for storing a bunch of ice duration variabilities of different lengths
datalist_index=1 #initialize counter to tick through datalist


for(window_size in 4:30){
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
} #End of looping through the window_sizes

#*compile them all in one data frame####    
variability_sequential<-do.call(bind_rows,datalist_sequential) 

#*Summarize one row for each segment_length and starting index####
variability_sequential_fits<-
  variability_sequential%>%
  group_by(starting_index,segment_length)%>%slice(1)

#Graph the median for each window size vs. the window size####
ggplot(data=variability_sequential_fits%>%ungroup()%>%group_by(segment_length)%>%summarize(median_iceIn_sensSlope_slope=median(iceIn_sensSlope_slope,na.rm=TRUE)),aes(x=segment_length,y=median_iceIn_sensSlope_slope))+geom_point()+geom_hline(yintercept=median(variability_sequential_fits$iceIn_sensSlope_slope,na.rm=TRUE))

ggplot(data=variability_sequential_fits%>%ungroup()%>%group_by(segment_length)%>%summarize(median_iceOut_sensSlope_slope=median(iceOut_sensSlope_slope,na.rm=TRUE)),aes(x=segment_length,y=median_iceOut_sensSlope_slope))+geom_point()+geom_hline(yintercept=median(variability_sequential_fits$iceOut_sensSlope_slope,na.rm=TRUE))

ggplot(data=variability_sequential_fits%>%ungroup()%>%group_by(segment_length)%>%summarize(median_duration_sensSlope_slope=median(duration_sensSlope_slope,na.rm=TRUE)),aes(x=segment_length,y=median_duration_sensSlope_slope))+geom_point()+geom_hline(yintercept=median(variability_sequential_fits$duration_sensSlope_slope,na.rm=TRUE))


#Calculate the mean or median of each segment length and then the mean of those####
summary_slopes<-variability_sequential_fits%>%
  ungroup()%>%
  group_by(segment_length)%>%
  summarize(median_iceIn_sensSlope_slope=median(iceIn_sensSlope_slope,na.rm=TRUE),
            median_iceOut_sensSlope_slope=median(iceOut_sensSlope_slope,na.rm=TRUE),
            median_duration_sensSlope_slope=median(duration_sensSlope_slope,na.rm=TRUE),
            mean_iceIn_sensSlope_slope=mean(iceIn_sensSlope_slope,na.rm=TRUE),
            mean_iceOut_sensSlope_slope=mean(iceOut_sensSlope_slope,na.rm=TRUE),
            mean_duration_sensSlope_slope=mean(duration_sensSlope_slope,na.rm=TRUE),
            mean_iceIn_sensSlope_intercept=mean(iceIn_sensSlope_intercept,na.rm=TRUE),
            mean_iceOut_sensSlope_intercept=mean(iceOut_sensSlope_intercept,na.rm=TRUE),
            mean_duration_sensSlope_intercept=mean(duration_sensSlope_intercept,na.rm=TRUE)
            )

#Check to see if the mean slope and intercept combo is representative####
#Ice in: mean slopes by segment####
ggplot() + geom_abline(data = summary_slopes,
  aes(slope = mean_iceIn_sensSlope_slope, intercept = mean_iceIn_sensSlope_intercept,
      color=segment_length, alpha=segment_length))+
  scale_x_continuous(limits = c(1932, 2022), expand = c(0, 0)) +
  scale_y_continuous(limits =c(5, 25))+
  geom_abline(data = summary_slopes,aes(slope=mean(mean_iceIn_sensSlope_slope),intercept=mean(mean_iceIn_sensSlope_intercept)),color="black",size=2)+
  xlab("Year")+
  ylab("Ice-on sd (days)")

#Ice out: mean slopes by segment####
ggplot() + geom_abline(data = summary_slopes,
                       aes(slope = mean_iceOut_sensSlope_slope, intercept = mean_iceOut_sensSlope_intercept,
                           color=segment_length, alpha=segment_length))+
  scale_x_continuous(limits = c(1932, 2022), expand = c(0, 0)) +
  scale_y_continuous(limits =c(5, 25))+
  geom_abline(data = summary_slopes,aes(slope=mean(mean_iceOut_sensSlope_slope),intercept=mean(mean_iceOut_sensSlope_intercept)),color="black",size=2)+
  xlab("Year")+
  ylab("Ice-off sd (days)")  

#Duration: mean slopes by segment####
ggplot() + geom_abline(data = summary_slopes,
                       aes(slope = mean_duration_sensSlope_slope, intercept = mean_duration_sensSlope_intercept,
                           color=segment_length, alpha=segment_length))+
  scale_x_continuous(limits = c(1932, 2022), expand = c(0, 0)) +
  scale_y_continuous(limits =c(5, 25))+
  geom_abline(data = summary_slopes,aes(slope=mean(mean_duration_sensSlope_slope),intercept=mean(mean_duration_sensSlope_intercept)),color="black",size=2)+
  xlab("Year")+
  ylab("Duration sd (days)") 


#Extrapolate the slopes####
summary_slopes$segment_length

#create a tibble with segment_length repeated for each year
summary_slopes_interpolation<-
  tibble(year=rep(seq(min(MohonkIce$Year),max(MohonkIce$Year)),times=length(summary_slopes$segment_length)),segment_length=rep(4:30,each=length(seq(min(MohonkIce$Year),max(MohonkIce$Year)))))%>%  #Create a data frame of years and segment legnths for each segment length
  left_join(.,summary_slopes%>%dplyr::select(-median_iceIn_sensSlope_slope,-median_iceOut_sensSlope_slope,-median_duration_sensSlope_slope),by="segment_length")%>% #merge with the sens slopes
  mutate(interpolated_iceIn_sd=year*mean_iceIn_sensSlope_slope + mean_iceIn_sensSlope_intercept, #create interpolated values from the average regression line
         interpolated_iceOut_sd=year*mean_iceOut_sensSlope_slope + mean_iceOut_sensSlope_intercept, #create interpolated values from the average regression line
         interpolated_duration_sd=year*mean_duration_sensSlope_slope + mean_duration_sensSlope_intercept #create interpolated values from the average regression line
          )

summary_slopes_interpolation_byYear<-
  summary_slopes_interpolation%>%
  group_by(year)%>%
  summarize(max_interpolated_iceIn_sd=max(interpolated_iceIn_sd),
            min_interpolated_iceIn_sd=min(interpolated_iceIn_sd),
            max_interpolated_iceOut_sd=max(interpolated_iceOut_sd),
            min_interpolated_iceOut_sd=min(interpolated_iceOut_sd),
            max_interpolated_duration_sd=max(interpolated_duration_sd),
            min_interpolated_duration_sd=min(interpolated_duration_sd)
            )

ggplot(data=summary_slopes_interpolation%>%group_by(segment_length))+geom_line(aes(x=year,y=interpolated_iceIn_sd,group=segment_length,color=segment_length))+scale_y_continuous(limits=c(5,25))
ggplot(data=summary_slopes_interpolation_byYear)+geom_ribbon(aes(x=year,ymax=max_interpolated_iceIn_sd,ymin=min_interpolated_iceIn_sd),fill="lightgrey",color="darkgrey")+scale_y_continuous(limits=c(5,25))

#MS partial figure
#Ice in: mean slopes by segment####
gg.iceIn_SD_abline_summary<-ggplot() + 
  geom_ribbon(data=summary_slopes_interpolation_byYear,aes(x=year,ymax=max_interpolated_iceIn_sd,ymin=min_interpolated_iceIn_sd),fill="lightgrey",color="darkgrey")+
  scale_x_continuous(limits = c(1932, 2022), expand = c(0, 0)) +
  scale_y_continuous(limits =c(5, 28))+
  geom_segment(data = summary_slopes,aes(x=1932,xend=2022,y=mean(mean_iceIn_sensSlope_slope)*1932+mean(mean_iceIn_sensSlope_intercept),yend=mean(mean_iceIn_sensSlope_slope)*2022+mean(mean_iceIn_sensSlope_intercept)),color="black",size=1)+
  #geom_abline(data = summary_slopes,aes(slope=mean(mean_iceIn_sensSlope_slope),intercept=mean(mean_iceIn_sensSlope_intercept)),color="black",size=2)+
  xlab("Year")+
  ylab("Ice-on sd (days)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))

#Ice out: mean slopes by segment####
gg.iceOut_SD_abline_summary<-ggplot() + 
  geom_ribbon(data=summary_slopes_interpolation_byYear,aes(x=year,ymax=max_interpolated_iceOut_sd,ymin=min_interpolated_iceOut_sd),fill="lightgrey",color="darkgrey")+
  scale_x_continuous(limits = c(1932, 2022), expand = c(0, 0)) +
  scale_y_continuous(limits =c(5, 28))+
  geom_segment(data = summary_slopes,aes(x=1932,xend=2022,y=mean(mean_iceOut_sensSlope_slope)*1932+mean(mean_iceOut_sensSlope_intercept),yend=mean(mean_iceOut_sensSlope_slope)*2022+mean(mean_iceOut_sensSlope_intercept)),color="black",size=1)+
  #geom_abline(data = summary_slopes,aes(slope=mean(mean_iceOut_sensSlope_slope),intercept=mean(mean_iceOut_sensSlope_intercept)),color="black",size=2)+
  xlab("Year")+
  ylab("Ice-off sd (days)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))

#Duration: mean slopes by segment####
gg.duration_SD_abline_summary<-ggplot() + 
  geom_ribbon(data=summary_slopes_interpolation_byYear,aes(x=year,ymax=max_interpolated_duration_sd,ymin=min_interpolated_duration_sd),fill="lightgrey",color="darkgrey")+
  scale_x_continuous(limits = c(1932, 2022), expand = c(0, 0)) +
  scale_y_continuous(limits =c(5, 28))+
  geom_segment(data = summary_slopes,aes(x=1932,xend=2022,y=mean(mean_duration_sensSlope_slope)*1932+mean(mean_duration_sensSlope_intercept),yend=mean(mean_duration_sensSlope_slope)*2022+mean(mean_duration_sensSlope_intercept)),color="black",size=1)+
  #geom_abline(data = summary_slopes,aes(slope=mean(mean_duration_sensSlope_slope),intercept=mean(mean_duration_sensSlope_intercept)),color="black",size=2)+
  xlab("Year")+
  ylab("Duration sd (days)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))


#Check to see if the mean and median are similar####
ggplot(data=summary_slopes,aes(x=median_iceIn_sensSlope_slope,y=mean_iceIn_sensSlope_slope))+geom_point()+geom_abline(slope=1,intercept=0)
ggplot(data=summary_slopes,aes(x=median_iceOut_sensSlope_slope,y=mean_iceOut_sensSlope_slope))+geom_point()+geom_abline(slope=1,intercept=0)
ggplot(data=summary_slopes,aes(x=median_duration_sensSlope_slope,y=mean_duration_sensSlope_slope))+geom_point()+geom_abline(slope=1,intercept=0)

#Densities of the different slopes####
  #Ice In: Density of all the slopes treating them equal####
ggplot(data=variability_sequential_fits,aes(x=iceIn_sensSlope_slope))+
  geom_density()+
  geom_histogram(aes(y=..density..), alpha=0.5,position="identity")+
  geom_vline(xintercept=mean(variability_sequential_fits$iceIn_sensSlope_slope))+
  scale_x_continuous(limits=c(-0.05,0.32))+
  theme_bw()+
  theme(plot.margin=unit(c(1,1,1,1), "lines"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))

  #Ice in: Density of all the slopes treating them equal####
ggplot(data=summary_slopes,aes(x=mean_iceIn_sensSlope_slope))+geom_density()+
  geom_histogram(aes(y=..density..), alpha=0.5,position="identity")+
  geom_vline(xintercept=mean(summary_slopes$mean_iceIn_sensSlope_slope))+
  scale_x_continuous(limits=c(-0.05,0.32))+  
  theme_bw()+
  theme(plot.margin=unit(c(1,1,1,1), "lines"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))

#Ice Out: Density of all the slopes treating them equal####
ggplot(data=variability_sequential_fits,aes(x=iceOut_sensSlope_slope))+
  geom_density()+
  geom_histogram(aes(y=..density..), alpha=0.5,position="identity")+
  geom_vline(xintercept=mean(variability_sequential_fits$iceOut_sensSlope_slope))+
  scale_x_continuous(limits=c(-0.05,0.12))+
  theme_bw()+
  theme(plot.margin=unit(c(1,1,1,1), "lines"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))

#Ice Out: Density of all the slopes treating them equal####
ggplot(data=summary_slopes,aes(x=mean_iceOut_sensSlope_slope))+geom_density()+
  geom_histogram(aes(y=..density..), alpha=0.5,position="identity")+
  geom_vline(xintercept=mean(summary_slopes$mean_iceOut_sensSlope_slope))+
  scale_x_continuous(limits=c(-0.05,0.12))+  
  theme_bw()+
  theme(plot.margin=unit(c(1,1,1,1), "lines"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))

#Ice Duration: Density of all the slopes treating them equal####
ggplot(data=variability_sequential_fits,aes(x=duration_sensSlope_slope))+
  geom_density()+
  geom_histogram(aes(y=..density..), alpha=0.5,position="identity")+
  geom_vline(xintercept=mean(variability_sequential_fits$duration_sensSlope_slope))+
  scale_x_continuous(limits=c(0.0,0.4))+
  theme_bw()+
  theme(plot.margin=unit(c(1,1,1,1), "lines"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))

#Ice duration: Density of all the slopes treating them equal####
ggplot(data=summary_slopes,aes(x=mean_duration_sensSlope_slope))+geom_density()+
  geom_histogram(aes(y=..density..), alpha=0.5,position="identity")+
  geom_vline(xintercept=mean(summary_slopes$mean_duration_sensSlope_slope))+
  scale_x_continuous(limits=c(0.0,0.4))+  
  theme_bw()+
  theme(plot.margin=unit(c(1,1,1,1), "lines"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))

#Does segment size matter####
#Ice in: segment size vs. sens slope mean for that value
ggplot(data=summary_slopes,aes(x=segment_length,y=mean_iceIn_sensSlope_slope))+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  geom_hline(yintercept=mean(summary_slopes$mean_iceIn_sensSlope_slope))+
  geom_hline(yintercept=quantile(summary_slopes$mean_iceIn_sensSlope_slope,c(0.05,0.95)),color="blue")+ #add in the 5th and 95th percentile lines
  geom_hline(yintercept=mean(summary_slopes$mean_iceIn_sensSlope_slope)+sd(summary_slopes$mean_iceIn_sensSlope_slope),color="red")+ #add in the plus 1 sd
  geom_hline(yintercept=mean(summary_slopes$mean_iceIn_sensSlope_slope)-sd(summary_slopes$mean_iceIn_sensSlope_slope),color="red") #add in the minus 1 sd

#Ice out: segment size vs. sens slope mean for that value
ggplot(data=summary_slopes,aes(x=segment_length,y=mean_iceOut_sensSlope_slope))+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  geom_hline(yintercept=mean(summary_slopes$mean_iceOut_sensSlope_slope))+
  geom_hline(yintercept=quantile(summary_slopes$mean_iceOut_sensSlope_slope,c(0.05,0.95)),color="blue")+ #add in the 5th and 95th percentile lines
  geom_hline(yintercept=mean(summary_slopes$mean_iceOut_sensSlope_slope)+sd(summary_slopes$mean_iceOut_sensSlope_slope),color="red")+ #add in the plus 1 sd
  geom_hline(yintercept=mean(summary_slopes$mean_iceOut_sensSlope_slope)-sd(summary_slopes$mean_iceOut_sensSlope_slope),color="red") #add in the minus 1 sd

#Ice duration: segment size vs. sens slope mean for that value
ggplot(data=summary_slopes,aes(x=segment_length,y=mean_duration_sensSlope_slope))+
  geom_point()+
  geom_abline(slope=1,intercept=0)+
  geom_hline(yintercept=mean(summary_slopes$mean_duration_sensSlope_slope))+
  geom_hline(yintercept=quantile(summary_slopes$mean_duration_sensSlope_slope,c(0.05,0.95)),color="blue")+ #add in the 5th and 95th percentile lines
  geom_hline(yintercept=mean(summary_slopes$mean_duration_sensSlope_slope)+sd(summary_slopes$mean_duration_sensSlope_slope),color="red")+ #add in the plus 1 sd
  geom_hline(yintercept=mean(summary_slopes$mean_duration_sensSlope_slope)-sd(summary_slopes$mean_duration_sensSlope_slope),color="red") #add in the minus 1 sd


#Summarize stats for the manuscript####
#Ice in summary with p-value####
iceIn_summary_slopes<-summary_slopes%>%ungroup()%>%
  summarize(variable="iceIn",
            overall_mean_sensSlope_slope=mean(mean_iceIn_sensSlope_slope),
            overall_sd_mean_sensSlope_slope=sd(mean_iceIn_sensSlope_slope),
            overall_max_mean_sensSlope_slope=max(mean_iceIn_sensSlope_slope),
            overall_min_mean_sensSlope_slope=min(mean_iceIn_sensSlope_slope),
            overall_tstat_mean_sensSlope_slope=overall_mean_sensSlope_slope/(overall_sd_mean_sensSlope_slope/sqrt(n())),
            overall_pvalue_mean_sensSlope_slope=pt(q=overall_tstat_mean_sensSlope_slope, df=n(), lower.tail=FALSE),
            overall_df_mean_sensSlope_slope=n()-1
            )

#Ice out summary with p-value####
iceOut_summary_slopes<-summary_slopes%>%ungroup()%>%
  summarize(variable="iceOut",
            overall_mean_sensSlope_slope=mean(mean_iceOut_sensSlope_slope),
            overall_sd_mean_sensSlope_slope=sd(mean_iceOut_sensSlope_slope),
            overall_max_mean_sensSlope_slope=max(mean_iceOut_sensSlope_slope),
            overall_min_mean_sensSlope_slope=min(mean_iceOut_sensSlope_slope),
            overall_tstat_mean_sensSlope_slope=overall_mean_sensSlope_slope/(overall_sd_mean_sensSlope_slope/sqrt(n())),
            overall_pvalue_mean_sensSlope_slope=pt(q=overall_tstat_mean_sensSlope_slope, df=n(), lower.tail=FALSE),
            overall_df_mean_sensSlope_slope=n()-1
  )  

#Ice duration summary with p-value####
duration_summary_slopes<-summary_slopes%>%ungroup()%>%
  summarize(variable="duration",
            overall_mean_sensSlope_slope=mean(mean_duration_sensSlope_slope),
            overall_sd_mean_sensSlope_slope=sd(mean_duration_sensSlope_slope),
            overall_max_mean_sensSlope_slope=max(mean_duration_sensSlope_slope),
            overall_min_mean_sensSlope_slope=min(mean_duration_sensSlope_slope),
            overall_tstat_mean_sensSlope_slope=overall_mean_sensSlope_slope/(overall_sd_mean_sensSlope_slope/sqrt(n())),
            overall_pvalue_mean_sensSlope_slope=pt(q=overall_tstat_mean_sensSlope_slope, df=n(), lower.tail=FALSE),
            overall_df_mean_sensSlope_slope=n()-1
  )  

#Merge all three together####
overall_summary_slopes<-bind_rows(iceIn_summary_slopes,iceOut_summary_slopes,duration_summary_slopes)

#Export that summary table####
write_csv(overall_summary_slopes,"output/Overall_summary_slopes_sd.csv")

datalist_sensfits=list()  #initialize empty list for storing a bunch of ice duration variabilities of different lengths
#Set the segment length here####
choice_segment_length<-9
#Filter down to a single segment length###
variability_sequential_fits_singleSegment<-variability_sequential_fits%>%filter(segment_length==choice_segment_length)
#loop through to generate sens slope fits for each year, each segment_length, each fit location####
for(model.index in 1:length(variability_sequential_fits_singleSegment$segment_length)){
  year=seq(min(MohonkIce$Year),max(MohonkIce$Year))
  datalist_sensfits[[model.index]]<-tibble(year=year,
                                           duration_sensSlope_fit_interpolate=variability_sequential_fits_singleSegment$duration_sensSlope_slope[model.index]*year+variability_sequential_fits_singleSegment$duration_sensSlope_intercept[model.index],
                                           iceIn_sensSlope_fit_interpolate=variability_sequential_fits_singleSegment$iceIn_sensSlope_slope[model.index]*year+variability_sequential_fits_singleSegment$iceIn_sensSlope_intercept[model.index],
                                           iceOut_sensSlope_fit_interpolate=variability_sequential_fits_singleSegment$iceOut_sensSlope_slope[model.index]*year+variability_sequential_fits_singleSegment$iceOut_sensSlope_intercept[model.index],
                                           segment_length=variability_sequential_fits_singleSegment$segment_length[model.index],
                                           starting_index=variability_sequential_fits_singleSegment$starting_index[model.index]
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

#Ice in density plot with all the segments####
gg.segmentValidation.iceIn <-
  ggplot() + geom_abline(
    data = variability_sequential_fits,
    aes(slope = iceIn_sensSlope_slope, intercept =
          iceIn_sensSlope_intercept,
        color=segment_length, alpha=segment_length)
    # color = rgb(108, 171, 221, max = 255),
    # alpha = 0.2
  ) + #,color=as.factor(segment_length) add this in aes to get them by color
  geom_ribbon(data=sensSlopeFitsInterpolated_sequential,aes(x=year,ymin=iceIn_q5_sensSlope_fit,ymax=iceIn_q95_sensSlope_fit),linetype=2, fill=NA, color="#000000",alpha=0.6)+
  scale_color_gradient(low = "#FFC20A", high = "#0C7BDC",
                       name="Segment length",
                       guide = guide_colorbar(label = TRUE,
                                              draw.ulim = TRUE, 
                                              draw.llim = TRUE,
                                              frame.colour = "black",
                                              ticks = TRUE, 
                                              nbin = 10,
                                              label.position = "bottom",
                                              barwidth = 13,
                                              barheight = 1.3, 
                                              direction = 'horizontal'))+
  scale_alpha_continuous(range=c(0.8,0.1)) +
  geom_line(data = sensSlopeFitsInterpolated_sequential,
            aes(x = year, y = iceIn_median_sensSlope_fit),
            linewidth = 1) +
  #geom_ribbon(aes(ymin=q25_sensSlope_fit,ymax=q75_sensSlope_fit),alpha=0.1,fill="light grey",color="grey")+
  #coord_cartesian(xlim = c(1932, 2022))
  scale_x_continuous(limits = c(1932, 2022), expand = c(0, 0)) +
  scale_y_continuous(limits =c(5, 15)) +
  theme_pubr(border=TRUE, base_size=8)+
  guides(
    alpha = "none" 
  )

  
#Ice out density plot with all the segments####
gg.segmentValidation.iceOut <-
  ggplot() + geom_abline(
    data = variability_sequential_fits,
    aes(slope = iceOut_sensSlope_slope, intercept = iceOut_sensSlope_intercept,
        color=segment_length, alpha=segment_length)
    # color = rgb(108, 171, 221, max = 255),
    # alpha = 0.4
  ) + #,color=as.factor(segment_length) add this in aes to get them by color
  geom_ribbon(data=sensSlopeFitsInterpolated_sequential,aes(x=year,ymin=iceOut_q5_sensSlope_fit,ymax=iceOut_q95_sensSlope_fit),fill=NA, linetype=2, color="#000000",alpha=0.6)+
  scale_color_gradient(low = "#FFC20A", high = "#0C7BDC",
                       name="Segment length",
                       guide = guide_colorbar(label = TRUE,
                                              draw.ulim = TRUE, 
                                              draw.llim = TRUE,
                                              frame.colour = "black",
                                              ticks = TRUE, 
                                              nbin = 10,
                                              label.position = "bottom",
                                              barwidth = 13,
                                              barheight = 1.3, 
                                              direction = 'horizontal'))+
  scale_alpha_continuous(range=c(0.8,0.1)) +
  geom_line(data = sensSlopeFitsInterpolated_sequential,
            aes(x = year, y = iceOut_median_sensSlope_fit),
            linewidth = 1) +
  #geom_ribbon(aes(ymin=q25_sensSlope_fit,ymax=q75_sensSlope_fit),alpha=0.1,fill="light grey",color="grey")+
  #coord_cartesian(xlim = c(1932, 2022))
  scale_x_continuous(limits = c(1932, 2022), expand = c(0, 0)) +
  scale_y_continuous(limits =c(5, 15)) +
  theme_pubr(border=TRUE, base_size=8)+
  guides(
    alpha = "none" 
  )

#Ice duration density plot with all the segments####
gg.segmentValidation.iceDuration <-
  ggplot() + geom_abline(
    data = variability_sequential_fits,
    aes(slope = duration_sensSlope_slope, intercept = duration_sensSlope_intercept,
        color=segment_length, alpha=segment_length)
    # color = rgb(108, 171, 221, max = 255),
    # alpha = 0.2
  ) + #,color=as.factor(segment_length) add this in aes to get them by color
  geom_ribbon(data=sensSlopeFitsInterpolated_sequential,aes(x=year,ymin=duration_q5_sensSlope_fit,ymax=duration_q95_sensSlope_fit),fill=NA,color="#000000",linetype=2,alpha=0.6)+
  geom_line(
    data = sensSlopeFitsInterpolated_sequential,
    aes(x = year, y = duration_median_sensSlope_fit),
    linewidth = 1
  ) +
  #geom_ribbon(aes(ymin=q25_sensSlope_fit,ymax=q75_sensSlope_fit),alpha=0.1,fill="light grey",color="grey")+
  #coord_cartesian(xlim = c(1932, 2022))
  scale_color_gradient(low = "#FFC20A", high = "#0C7BDC",
                       name="Segment length",
                       guide = guide_colorbar(label = TRUE,
                                              draw.ulim = TRUE, 
                                              draw.llim = TRUE,
                                              frame.colour = "black",
                                              ticks = TRUE, 
                                              nbin = 10,
                                              label.position = "bottom",
                                              barwidth = 13,
                                              barheight = 1.3, 
                                              direction = 'horizontal'))+
  scale_alpha_continuous(range=c(0.8,0.1)) +
  scale_x_continuous(limits = c(1932, 2022), expand = c(0, 0)) +
  scale_y_continuous(limits =c(5, 15)) +
  theme_pubr(border=TRUE, base_size=8)+
  guides(
    alpha = "none" 
  )

panel.size<-10
List<-list(
            gg.segmentValidation.iceIn+
              scale_y_continuous(limits=c(5,28),breaks=c(5,15,25))+
              scale_x_continuous(limits=c(1932,2022),breaks = seq(1940, 2022, by = 20),expand = c(0, 0))+
              xlab("Year")+
              ylab("Ice-on sd (days)")+
              geom_text(aes(x=-Inf,y=Inf,hjust=-0.5,vjust=1.5,label="a"))+
              theme(plot.margin=unit(c(1,1,1,1), "lines"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5)),
            gg.segmentValidation.iceOut+
              scale_y_continuous(limits=c(5,28),breaks=c(5,15,25))+
              scale_x_continuous(limits=c(1932,2022),breaks = seq(1940, 2022, by = 20),expand = c(0, 0))+
              xlab("Year")+
              ylab("Ice-off sd (days)")+
              geom_text(aes(x=-Inf,y=Inf,hjust=-0.5,vjust=1.5,label="b"))+
              theme(plot.margin=unit(c(1,1,1,1), "lines"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5)),
            gg.segmentValidation.iceDuration+
              scale_y_continuous(limits=c(5,28),breaks=c(5,15,25))+
              scale_x_continuous(limits=c(1932,2022),breaks = seq(1940, 2022, by = 20),expand = c(0, 0))+
              xlab("Year")+
              ylab("Duration sd (days)")+
              geom_text(aes(x=-Inf,y=Inf,hjust=-0.5,vjust=1.5,label="c"))+
              theme(plot.margin=unit(c(1,1,1,1), "lines"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))
            )

#Plot them using patchwork####
(gg.3panel.senSlopeValidation<-wrap_plots(List,ncol = 3,nrow = 1)&theme(legend.position="bottom", plot.margin = unit(c(4,6,3,3),"pt"))) +
  plot_layout(guides="collect")


ggsave("figures/FigureSupp.rollingwindow_9year.png", width=7, height=3,units="in", dpi=300)


#MS FIGURE: Figure 2####
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
           gg.iceIn_SD_abline_summary+
             scale_y_continuous(limits=c(5,28),breaks=c(5,15,25))+
             scale_x_continuous(limits=c(1931,2023),breaks = seq(1940, 2020, by = 20))+
             xlab("Year")+
             geom_text(aes(x=-Inf,y=Inf,hjust=-0.5,vjust=1.5,label="d"))+
             theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "lines")),
           gg.iceOut_SD_abline_summary+
             scale_y_continuous(limits=c(5,28),breaks=c(5,15,25))+
             scale_x_continuous(limits=c(1931,2023),breaks = seq(1940, 2020, by = 20))+
             xlab("Year")+
             geom_text(aes(x=-Inf,y=Inf,hjust=-0.5,vjust=1.5,label="e"))+
             theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "lines")),
           gg.duration_SD_abline_summary+
             scale_y_continuous(limits=c(5,28),breaks=c(5,15,25))+
             scale_x_continuous(limits=c(1931,2023),breaks = seq(1940, 2020, by = 20))+
             xlab("Year")+
             geom_text(aes(x=-Inf,y=Inf,hjust=-0.5,vjust=1.5,label="f"))+
             theme(plot.margin=unit(c(0.5,0.5,0.5,0.5), "lines"))
)

#Plot them using patchwork####
(gg.6panel.variability<-wrap_plots(List,ncol = 3,nrow = 2)&theme(plot.margin = unit(c(4,3,3,3),"pt")))
#Could do a 3x3 with width 6, height = 5
ggsave(paste("figures/Fig2.Variability6panels.jpg",sep=""), plot=gg.6panel.variability, width=7, height=4,units="in", dpi=300)



