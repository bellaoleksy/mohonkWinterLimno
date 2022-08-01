#Script 05-TrendsInWinterWaterTemperature.R####
#Explore the winter water temperature and associated metrics for Mohonk Lake
#Created 15Jul2022, by David Richardson (DCR)

#Packages####
if(!require(scales)){install.packages("scales")}
if(!require(ggridges)){install.packages("ggridges")}
if(!require(lavaan)){install.packages("lavaan")}
if(!require(semPlot)){install.packages("semPlot")}
if(!require(GGally)){install.packages("GGally")}


#Load libraries
library(scales) #for pretty_breaks()
library(ggridges)
library(lavaan) #for SEM fit
library(semPlot) #for SEM visualization
library(GGally) #for corr plot as gg objects

#Graph  each water year with ice in/out as vertical lines, and temperature as spaghetti plot####
ggplot(data=DailyInterpol_winter%>%filter(wateryear==2001),aes(x=Date,y=DailyIceRecord_binomial*5))+geom_line()+
  geom_line(data=DailyInterpol_winter%>%filter(wateryear==2001)%>%dplyr::select(Date:Temp_12m)%>%gather("Depth","Temperature",-Date),aes(x=Date,y=Temperature,color=Depth))+
  scale_y_continuous(limit=c(0,15))+
  theme_bw()

#Plot epi temperatures all years to do some QA/QC####
ggplot(data=DailyInterpol_winter%>%as_tibble(),aes(x=Date,y=DailyIceRecord_binomial*5))+geom_line()+
  geom_point(aes(x=Date,y=Temp_1m),color="red")+
  geom_point(aes(x=Date,y=Temp_2m),color="orange")+
  geom_point(aes(x=Date,y=Temp_3m),color="yellow")+
  scale_y_continuous(limit=c(0,15))+
  theme_bw()+
  facet_wrap(vars(as.factor(wateryear)),scale="free")

#Plot hypo temperatures all years####
ggplot(data=DailyInterpol_winter%>%as_tibble(),aes(x=Date,y=DailyIceRecord_binomial*5))+geom_line()+
  geom_point(aes(x=Date,y=Temp_10m),color="green")+
  geom_point(aes(x=Date,y=Temp_11m),color="blue")+
  geom_point(aes(x=Date,y=Temp_12m),color="purple")+
  scale_y_continuous(limit=c(0,15))+
  theme_bw()+
  facet_wrap(vars(as.factor(wateryear)),scale="free")

#Plot top - bottom temperatures all years####
ggplot(data=DailyInterpol_winter%>%as_tibble(),aes(x=Date,y=DailyIceRecord_binomial*5))+geom_line()+
  geom_point(aes(x=Date,y=Temp_1m-Temp_11m),color="green")+
  scale_y_continuous(limit=c(-5,15))+
  theme_bw()+
  facet_wrap(vars(as.factor(wateryear)),scale="free")

#Plot schmidt stability all years####
ggplot(data=DailyInterpol_winter%>%as_tibble(),aes(x=Date,y=DailyIceRecord_binomial*5))+geom_line()+
  geom_point(aes(x=Date,y=stability_Jperm2),color="pale green")+
  scale_y_continuous(limit=c(-5,30))+
  theme_bw()+
  facet_wrap(vars(as.factor(wateryear)),scale="free")

#Plot first derviative of stability all years####
ggplot(data=DailyInterpol_winter%>%as_tibble(),aes(x=Date,y=DailyIceRecord_binomial*1.5))+geom_line()+
  geom_point(aes(x=Date,y=FirstDerv_stability_Jperm2perday),color="violet")+
  scale_y_continuous(limit=c(-4,4))+
  theme_bw()+
  facet_wrap(vars(as.factor(wateryear)),scale="free")

#Graph  each water year with ice in/out as vertical lines, and other variables as points####  
ggplot(data=DailyInterpol_winter%>%filter(wateryear==2001),aes(x=Date,y=DailyIceRecord_binomial))+geom_line()+
  geom_point(aes(x=Date,y=Temp_1m-Temp_11m))+
  scale_y_continuous(limit=c(-5,15))+
  theme_bw()

#Ridgelines of various variables
#*Ridgelines of Schmidt Stability####
####This could also be one of those cool stacked histogram figures: density ridge plots
####http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/
####Would have to cut DailyInterpol by the start and end of strat (stability.cutoff) then
####https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

#**Overlapping ridgelines - subset out everything without 2014
Ridgelines.DF<-DailyInterpol_winter%>%
              dplyr::select(wateryear,HydroDay,buoyancyfrequency_1_s2,DailyIceRecord_binomial)%>%
              filter(DailyIceRecord_binomial==1)%>%
              filter(!is.na(buoyancyfrequency_1_s2))%>%
              filter(wateryear>1985&wateryear<2022) #%>%filter(wateryear!=2014)%>%filter(wateryear!=2017)

#Gridlines for the plot
segment_data = data.frame(
  x = c(seq(100,200,by=50)),
  xend = c(seq(100,200,by=50)), 
  y = rep(1986,length(seq(100,200,by=50))),
  yend =rep(2022,length(seq(100,200,by=50)))
)

#**Make them a little pretty
ggplot()+
  geom_hline(yintercept=c(seq(1986,2022,by=1)),col="light grey")+
  geom_segment(data=segment_data,aes(x=x,y=y,xend=xend,yend=yend),col="light grey")+
  #geom_vline(xintercept=c(seq(100,300,by=50)),col="light grey")+
  geom_density_ridges_gradient(data=Ridgelines.DF,aes(x=HydroDay,y=wateryear,height=buoyancyfrequency_1_s2,group=wateryear,fill=stat(height)),stat="identity",scale=2)+
  scale_fill_viridis_c(option="C",guide = guide_colorbar(frame.colour = "black",frame.linewidth = 1.25,ticks = TRUE,ticks.colour="white",ticks.linewidth=2))+
  ylab("Year")+
  xlab("Day of Year")+
  theme_bw()+  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),plot.margin=unit(c(2,0.2,2,0.2), "lines"),legend.box.background = element_blank(),plot.background = element_blank(),legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,0,-10,-5),
        panel.border = element_rect(fill=NA, colour = "black", size=1),
        axis.text.x= element_text(color="black"),
        axis.text.y= element_text(color="black"),
        axis.ticks = element_line(color="black"))+
  labs(fill=expression(atop("St",paste("(J m"^-2*")"))))+
  scale_y_continuous(limit=c(1986,2022),breaks=c(seq(1990,2020,by=5)))
#Inverse stratification according to Woolway paper? Temp_1m-Temp_11m seems to go negative
#calculate the density gradient under the curve?
#Figure out other questions
#Create ridgeline plot in a new script


#Analyis for annual under ice data frame####

#Plot the heat contents by year####
#ggplot(data=DailyInterpol_winter%>%filter(DailyIceRecord_binomial==1),aes(x=HydroDay,y=HeatContent_MegaJoules))+geom_line()+facet_wrap(vars(wateryear),scales="free")+theme(axis.text.y = element_blank())+geom_smooth(method="lm")

#*Graph various facets of under ice vs. year after removing bad with not enough data years####
#ggplot(AnnualUnderIceSummary%>%filter(proportionOfDaysWithData==1),aes(x=wateryear,y=StartOfStratification_HydroDay))+geom_point()

#*Graph various facets of under ice vs. ice length after removing bad with not enough data years####
#ggplot(AnnualUnderIceSummary%>%filter(proportionOfDaysWithData==1),aes(x=numberOfIceDays,y=MeanUnderIce_HypoTemp_degC))+geom_point()+geom_smooth(method='lm')

#*Path Analysis####
#**Model: Start out with a simple model####
model<-'
      MeanUnderIce_HypoTemp_degC_scale~ LengthOfIceCover_days_scale + IceOutDayofYear_fed_scale
      MeanUnderIce_EpiTemp_degC_scale~LengthOfIceCover_days_scale + IceOutDayofYear_fed_scale
      MeanDelta1_11mWaterDensity_kgperm3_scale ~ LengthOfIceCover_days_scale + MeanUnderIce_HypoTemp_degC_scale + MeanUnderIce_EpiTemp_degC_scale
      SlopeHeatContent_MegaJoulesperDay_scale~MeanDelta1_11mWaterDensity_kgperm3_scale + LengthOfIceCover_days_scale + IceOutDayofYear_fed_scale
      FinalHeatContent_MegaJoules_scale ~ MeanDelta1_11mWaterDensity_kgperm3_scale + LengthOfIceCover_days_scale + IceOutDayofYear_fed_scale+SlopeHeatContent_MegaJoulesperDay_scale+SlopeHeatContent_MegaJoulesperDay_scale
      LengthSpringMixedPeriod_days_scale ~ LengthOfIceCover_days_scale + IceOutDayofYear_fed_scale+FinalHeatContent_MegaJoules_scale
      
      
      LengthOfIceCover_days_scale ~~ IceOutDayofYear_fed_scale
      MeanUnderIce_EpiTemp_degC_scale~~MeanUnderIce_HypoTemp_degC_scale
      
      
      '
#Model 2 has ice as a latent variable composed of length and ice out####
model2<-'MeanUnderIce_HypoTemp_degC_scale~LengthOfIceCover_days_scale+ IceOutDayofYear_fed_scale
        MeanUnderIce_EpiTemp_degC_scale~LengthOfIceCover_days_scale+ IceOutDayofYear_fed_scale
        MeanDelta1_11mWaterDensity_kgperm3_scale ~ MeanUnderIce_HypoTemp_degC_scale + MeanUnderIce_EpiTemp_degC_scale
        MeanHeatContent_MegaJoules_scale~MeanDelta1_11mWaterDensity_kgperm3_scale+LengthOfIceCover_days_scale+ IceOutDayofYear_fed_scale
        
        LengthSpringMixedPeriod_days_scale ~ LengthOfIceCover_days_scale+ IceOutDayofYear_fed_scale + MeanHeatContent_MegaJoules_scale
        
        
         LengthOfIceCover_days_scale ~~ IceOutDayofYear_fed_scale
         MeanUnderIce_EpiTemp_degC_scale~~MeanUnderIce_HypoTemp_degC_scale
         
        '

      'MeanUnderIce_HypoTemp_degC_scale~ Ice
      MeanUnderIce_EpiTemp_degC_scale~Ice
      MeanDelta1_11mWaterDensity_kgperm3_scale ~ Ice + MeanUnderIce_HypoTemp_degC_scale + MeanUnderIce_EpiTemp_degC_scale
      SlopeHeatContent_MegaJoulesperDay_scale~MeanDelta1_11mWaterDensity_kgperm3_scale + Ice
      FinalHeatContent_MegaJoules_scale ~ MeanDelta1_11mWaterDensity_kgperm3_scale + Ice +SlopeHeatContent_MegaJoulesperDay_scale+SlopeHeatContent_MegaJoulesperDay_scale
      LengthSpringMixedPeriod_days_scale ~ Ice + FinalHeatContent_MegaJoules_scale
      #FinalHeatContent_MegaJoules_scale ~ MeanDelta1_11mWaterDensity_kgperm3_scale + SlopeHeatContent_MegaJoulesperDay_scale + LengthOfIceCover_days_scale
      Ice=~LengthOfIceCover_days_scale+IceInDayofYear_fed_scale + IceOutDayofYear_fed_scale 
      
      LengthOfIceCover_days_scale ~~ IceOutDayofYear_fed_scale
      MeanUnderIce_EpiTemp_degC_scale~~MeanUnderIce_HypoTemp_degC_scale
      
      
      '
      #Ice=~LengthOfIceCover_days_scale+IceInDayofYear_fed_scale + IceOutDayofYear_fed_scale
      #MeanUnderIce_HypoTemp_degC_scale ~ Ice
      #MeanUnderIce_EpiTemp_degC_scale ~ Ice
      #Other possible components
      #MeanDelta1_11mTemp_degC_scale ~ LengthOfIceCover_days_scale + MeanUnderIce_HypoTemp_degC_scale + MeanUnderIce_EpiTemp_degC_scale
      #MeanDelta1_11mWaterDensity_kgperm3_scale ~ LengthOfIceCover_days_scale + MeanUnderIce_HypoTemp_degC_scale + MeanUnderIce_EpiTemp_degC_scale
      #LengthSpringMixedPeriod_days_scale ~ LengthOfIceCover_days_scale + IceOutDayofYear_fed_scale
      #FinalHeatContent_MegaJoules_scale ~ MeanDelta1_11mWaterDensity_kgperm3_scale + LengthOfIceCover_days_scale + IceOutDayofYear_fed_scale

#**Create scaled variables for each of interest####
AnnualUnderIceSummary_SEM<-AnnualUnderIceSummary%>%
  filter(proportionOfDaysWithData==1)%>%
  dplyr::select(-wateryear,-numberOfIceDays,-numberOfDaysWithData,-proportionOfDaysWithData,-DateOfStratification)%>%
  mutate_all(.funs=list(scale=~scale(.)[,1]))
  

#**Fit the model####
#From lavaan package
fit<-sem(model2,data=AnnualUnderIceSummary_SEM)
  varTable(fit)
  
#***view the results####
summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)

#**Building a Structural Equation Model (SEM)####
semPaths(fit,'std',layout='tree',edge.label.cex = 1.1,label.cex=1.1)

#**correlation plot####
ggcorr(AnnualUnderIceSummary_SEM%>%
         dplyr::select(LengthOfIceCover_days_scale,IceInDayofYear_fed_scale,IceOutDayofYear_fed_scale,MeanUnderIce_HypoTemp_degC_scale, MeanUnderIce_EpiTemp_degC_scale,MeanDelta1_11mWaterDensity_kgperm3_scale,FinalHeatContent_MegaJoules_scale,SlopeHeatContent_MegaJoulesperDay_scale),
       nbreaks = 6, label = T, low = "red3", high = "green3", 
label_round = 2, name = "Correlation Scale", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlation Plot") +
  theme(plot.title = element_text(hjust = 0.6))
  
#Pull out the strongest relationships from the SEM####
  ggplot(data=AnnualUnderIceSummary_SEM,aes(x=LengthOfIceCover_days_scale,y=MeanUnderIce_HypoTemp_degC_scale))+geom_point()
      summary(lm(AnnualUnderIceSummary_SEM$MeanUnderIce_HypoTemp_degC_scale~AnnualUnderIceSummary_SEM$LengthOfIceCover_days_scale))  
      #Not sig
  ggplot(data=AnnualUnderIceSummary_SEM,aes(x=LengthOfIceCover_days,y=FinalHeatContent_MegaJoules))+geom_point()+geom_smooth(method="lm")
      summary(lm(AnnualUnderIceSummary_SEM$FinalHeatContent_MegaJoules~AnnualUnderIceSummary_SEM$LengthOfIceCover_days))  
      #sig. p=0.0144,R2=0.1923
  ggplot(data=AnnualUnderIceSummary_SEM,aes(x=LengthOfIceCover_days,y=SlopeHeatContent_MegaJoulesperDay))+geom_point()+geom_smooth(method="lm")
      summary(lm(AnnualUnderIceSummary_SEM$SlopeHeatContent_MegaJoulesperDay~AnnualUnderIceSummary_SEM$LengthOfIceCover_days))  
      #sig. p=0.0001,R2=0.44
  ggplot(data=AnnualUnderIceSummary_SEM,aes(x=IceOutDayofYear_fed,y=LengthSpringMixedPeriod_days))+geom_point()+geom_smooth(method="lm")
      summary(lm(AnnualUnderIceSummary_SEM$LengthSpringMixedPeriod_days~AnnualUnderIceSummary_SEM$IceOutDayofYear_fed))  
      #sig. p=0.0013,R2=0.34
  ggplot(data=AnnualUnderIceSummary_SEM,aes(x=IceOutDayofYear_fed,y=StartOfStratification_HydroDay))+geom_point()
      summary(lm(AnnualUnderIceSummary_SEM$StartOfStratification_HydroDay~AnnualUnderIceSummary_SEM$IceOutDayofYear_fed))  
      #NS
  ggplot(data=AnnualUnderIceSummary_SEM,aes(x=IceOutDayofYear_fed,y=MeanUnderIce_EpiTemp_degC))+geom_point()
      summary(lm(AnnualUnderIceSummary_SEM$MeanUnderIce_EpiTemp_degC~AnnualUnderIceSummary_SEM$IceOutDayofYear_fed))  
      #NS
  ggplot(data=AnnualUnderIceSummary_SEM,aes(x=LengthOfIceCover_days,y=MeanDelta1_11mWaterDensity_kgperm3_scale))+geom_point()
      summary(lm(AnnualUnderIceSummary_SEM$MeanDelta1_11mWaterDensity_kgperm3_scale~AnnualUnderIceSummary_SEM$LengthOfIceCover_days))  
      #NS

      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=LengthOfIceCover_days,y=VarSchmidtStabilityUnderIce_Jdayspm2))+geom_point()  
      
      
 