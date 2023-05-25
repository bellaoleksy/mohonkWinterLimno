
#Run the main script to bring in all data and functions####
source('00_main.R')

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


datalist_sensfits=list()  #initialize empty list for storing a bunch of ice duration variabilities of different lengths
#Set the segment length here####
choice_segment_length<-8
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
        color=segment_length, alpha=segment_length),
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
        color=segment_length, alpha=segment_length),
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


ggsave("figures/FigureSupp.rollingwindow_8year.png", width=7, height=3,units="in", dpi=300)



