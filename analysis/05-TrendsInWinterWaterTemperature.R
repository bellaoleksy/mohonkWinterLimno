#Script 05-TrendsInWinterWaterTemperature.R####
#Explore the winter water temperature and associated metrics for Mohonk Lake
#Created 15Jul2022, by David Richardson (DCR)

#Run previous code to get in data####
source('00_main.R')

#Packages####
if(!require(scales)){install.packages("scales")}
if(!require(ggridges)){install.packages("ggridges")}
if(!require(lavaan)){install.packages("lavaan")}
if(!require(semPlot)){install.packages("semPlot")}
if(!require(GGally)){install.packages("GGally")}
if(!require(patchwork)){install.packages("patchwork")}
if(!require(ggplotify)){install.packages("ggplotify")}
if(!require(ggnetwork)){install.packages("ggnetwork")}
if(!require(cowplot)){install.packages("cowplot")}


#Load libraries
library(scales) #for pretty_breaks()
library(ggridges)
library(lavaan) #for SEM fit
library(semPlot) #for SEM visualization
library(GGally) #for corr plot as gg objects
library(patchwork) #laying out multipanel plots with the same size
library(ggplotify) #create gg objects or grobs from other graphical systems
library(ggnetwork) #Makes nice network plots in ggplot from lavaan object
library(cowplot) #laying out multipanel plots with different sizes

summarize <- dplyr::summarize 

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
model1<-'
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
model2<-'Ice=~IceInDayofYear_fed_scale + IceOutDayofYear_fed_scale 
        MeanUnderIce_HypoTemp_degC_scale~Ice
        MeanUnderIce_EpiTemp_degC_scale~Ice
        MeanDelta1_11mWaterDensity_kgperm3_scale ~ MeanUnderIce_HypoTemp_degC_scale + MeanUnderIce_EpiTemp_degC_scale
        
        FinalHeatContent_MegaJoules_scale~Ice
        LengthSpringMixedPeriod_days_scale ~ Ice + MeanDelta1_11mWaterDensity_kgperm3_scale+FinalHeatContent_MegaJoules_scale
        
        
         IceInDayofYear_fed_scale ~~ IceOutDayofYear_fed_scale
         
         MeanUnderIce_EpiTemp_degC_scale~~MeanUnderIce_HypoTemp_degC_scale
         
        '


#Model 3 ####
model3<-'MeanUnderIce_HypoTemp_degC_scale~LengthOfIceCover_days_scale+ IceOutDayofYear_fed_scale
        MeanUnderIce_EpiTemp_degC_scale~LengthOfIceCover_days_scale+ IceOutDayofYear_fed_scale
        MeanDelta1_11mWaterDensity_kgperm3_scale ~ MeanUnderIce_HypoTemp_degC_scale + MeanUnderIce_EpiTemp_degC_scale
        
        LengthSpringMixedPeriod_days_scale ~  IceOutDayofYear_fed_scale+FinalHeatContent_MegaJoules_scale+LengthOfIceCover_days_scale
        FinalHeatContent_MegaJoules_scale~LengthOfIceCover_days_scale+MeanDelta1_11mWaterDensity_kgperm3_scale
        SlopeHeatContent_MegaJoulesperDay_scale~MeanDelta1_11mWaterDensity_kgperm3_scale+LengthOfIceCover_days_scale
          
         SlopeHeatContent_MegaJoulesperDay_scale~~FinalHeatContent_MegaJoules_scale 
         LengthOfIceCover_days_scale ~~ IceOutDayofYear_fed_scale
         MeanUnderIce_EpiTemp_degC_scale~~MeanUnderIce_HypoTemp_degC_scale
         
        '

#Model 4 ####
model4<-'MeanUnderIce_HypoTemp_degC_scale~LengthOfIceCover_days_scale+ IceOutDayofYear_fed_scale
        MeanUnderIce_EpiTemp_degC_scale~LengthOfIceCover_days_scale+ IceOutDayofYear_fed_scale
        MeanDelta1_11mWaterDensity_kgperm3_scale ~ MeanUnderIce_HypoTemp_degC_scale + MeanUnderIce_EpiTemp_degC_scale
        
        LengthSpringMixedPeriod_days_scale ~  IceOutDayofYear_fed_scale+LengthOfIceCover_days_scale
       
         
         LengthOfIceCover_days_scale ~~ IceOutDayofYear_fed_scale
         MeanUnderIce_EpiTemp_degC_scale~~MeanUnderIce_HypoTemp_degC_scale
         
        '
      #MeanSchmidtStabilityUnderIce_Jdayspm2_scale~MeanDelta1_11mWaterDensity_kgperm3_scale
      #FinalVolumeWeightedMeanTemp_degC_scale~MeanUnderIce_HypoTemp_degC_scale + MeanUnderIce_EpiTemp_degC_scale+MeanDelta1_11mWaterDensity_kgperm3_scale
      #FinalHeatContent_MegaJoules_scale~MeanDelta1_11mWaterDensity_kgperm3_scale+LengthOfIceCover_days_scale+ IceOutDayofYear_fed_scale
      #MeanUnderIce_HypoTemp_degC_scale~ Ice
      #MeanUnderIce_EpiTemp_degC_scale~Ice
      #MeanDelta1_11mWaterDensity_kgperm3_scale ~ Ice + MeanUnderIce_HypoTemp_degC_scale + MeanUnderIce_EpiTemp_degC_scale
      #SlopeHeatContent_MegaJoulesperDay_scale~MeanDelta1_11mWaterDensity_kgperm3_scale + Ice
      #FinalHeatContent_MegaJoules_scale ~ MeanDelta1_11mWaterDensity_kgperm3_scale + Ice +SlopeHeatContent_MegaJoulesperDay_scale+SlopeHeatContent_MegaJoulesperDay_scale
      #LengthSpringMixedPeriod_days_scale ~ Ice + FinalHeatContent_MegaJoules_scale
      #FinalHeatContent_MegaJoules_scale ~ MeanDelta1_11mWaterDensity_kgperm3_scale + SlopeHeatContent_MegaJoulesperDay_scale + LengthOfIceCover_days_scale
      #Ice=~LengthOfIceCover_days_scale+IceInDayofYear_fed_scale + IceOutDayofYear_fed_scale 
      
      #LengthOfIceCover_days_scale ~~ IceOutDayofYear_fed_scale
      #MeanUnderIce_EpiTemp_degC_scale~~MeanUnderIce_HypoTemp_degC_scale
      
      
      
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
fit<-sem(model4,data=AnnualUnderIceSummary_SEM,meanstructure=TRUE)
  varTable(fit)
  
#***view the results####
summary(fit, fit.measures = TRUE, standardized=T,rsquare=T)
  
  parameterEstimates(fit)
  
#Visualize the SEM
  semPaths(fit,'std',layout='tree2',edge.label.cex = 1.3,label.cex=1.1,intercepts=FALSE,curve=TRUE)

#**Building a Structural Equation Model (SEM)####
SEM.plot<-semPaths(fit,'std',layout='tree2',edge.label.cex = 1.3,label.cex=1.1,intercepts=FALSE,curve=TRUE,nCharNodes = 8,title=FALSE,residuals=FALSE,
         nodeLabels=c("Hyp Tmp","Epi Tmp","Den Del","Spr Mix","Ice Dur","Ice Out"),node.width=c(2,2,2,2,2,2),node.height=c(1.1,1.1,1.1,1.1,1.1,1.1),shapeMan="rectangle",
         edge.label.position=c(0.5,0.65,0.35,0.65,0.5,0.5,0.5,0.65,0.5,0.5,0.5,0.5,0.5,0.5))

#Export as a jpg####
jpeg("figures/SEMplot.jpg",width=3,height=2.5,units="in",res=300)
semPaths(fit,'std',layout='tree2',edge.label.cex = 1.7,label.cex=1.3,intercepts=FALSE,curve=TRUE,nCharNodes = 8,title=FALSE,residuals=FALSE,
         nodeLabels=c("Hyp Temp","Epi Temp",bquote(" "~Dens.~Delta~" ")," Spr Mix "," Ice Dur. "," Ice Out  "),node.width=c(4.4,4.4,4.4,4.4,4.4,4.4),node.height=c(1.5,1.5,1.5,1.5,1.5,1.5),shapeMan="rectangle",
         edge.label.position=c(0.5,0.65,0.35,0.65,0.5,0.5,0.5,0.7,0.5,0.5,0.5,0.5,0.5,0.5))
dev.off()
#**Partial residual plots for variables of interest####
#*https://en.wikipedia.org/wiki/Partial_residual_plot
#*Calculate the resiudals for the variable using the equation
#*Calculate the partial residuals by doing the model residuals + Beta1*x and plot vs. x for a variable of interest
#*use Beta*x for the CCPR - component and compenent plus residual showing where the fitted line would lie
  #***Plot the partial residuals for the top variable for Length of mixing####
  #****Create labels and breaks in the right spots to back transform####
  labels_lengthspringmixed<-c(10,20,30,40,50)
  breaks_lengthspringmixed<-(labels_lengthspringmixed-mean(AnnualUnderIceSummary_SEM$LengthSpringMixedPeriod_days,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$LengthSpringMixedPeriod_days,na.rm=TRUE)
  
  labels_IceOutDayofYear_fed<-c(166,183,197)
  breaks_IceOutDayofYear_fed<-(labels_IceOutDayofYear_fed-mean(AnnualUnderIceSummary_SEM$IceOutDayofYear_fed,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$IceOutDayofYear_fed,na.rm=TRUE)
  limits_IceOutDayofYear_fed<-(c(160,200)-mean(AnnualUnderIceSummary_SEM$IceOutDayofYear_fed,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$IceOutDayofYear_fed,na.rm=TRUE)
  
  (gg.partialResid.iceOutVsSpringMixed<-AnnualUnderIceSummary_SEM%>%mutate(LengthSpringMixedPeriod_days_scale_Resids=LengthSpringMixedPeriod_days_scale-(-0.793*IceOutDayofYear_fed_scale+0.287*LengthOfIceCover_days_scale-0.012))%>%
    ggplot(.,aes(y=(LengthSpringMixedPeriod_days_scale_Resids-0.793*IceOutDayofYear_fed_scale),x=IceOutDayofYear_fed_scale))+
    geom_point()+
    geom_line(aes(y=IceOutDayofYear_fed_scale*-0.793-0.012))+
    xlab("Ice off date")+
    ylab("Spr. mixed period (d)")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))+
    #Can modify to actual day here... but is better to get more evenly spaced numbers and then figure out the z-score of those numbers for the breaks
    scale_y_continuous(breaks=breaks_lengthspringmixed,labels=labels_lengthspringmixed)+
    scale_x_continuous(breaks=breaks_IceOutDayofYear_fed,labels=c("15-Mar","01-Apr","15-Apr"),limits=limits_IceOutDayofYear_fed)
  )
  
  
  
  #***Plot the partial residuals for the top variable for hypo temp####
  #****Create labels and breaks in the right spots to back transform####
  labels_LengthOfIceCover_days<-c(40,60,80,100,120)
  breaks_LengthOfIceCover_days<-(labels_LengthOfIceCover_days-mean(AnnualUnderIceSummary_SEM$LengthOfIceCover_days,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$LengthOfIceCover_days,na.rm=TRUE)
  labels_MeanUnderIce_HypoTemp_degC<-c(2.5,3,3.5,4)
  breaks_MeanUnderIce_HypoTemp_degC<-(labels_MeanUnderIce_HypoTemp_degC-mean(AnnualUnderIceSummary_SEM$MeanUnderIce_HypoTemp_degC,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$MeanUnderIce_HypoTemp_degC,na.rm=TRUE)
  labels2_MeanUnderIce_HypoTemp_degC<-c("2.5","3.0","3.5","4.0")
  
  #Check the max and min ranges
  AnnualUnderIceSummary_SEM%>%summarize(min=min(LengthOfIceCover_days,na.rm=TRUE),max=max(LengthOfIceCover_days,na.rm=TRUE))
  AnnualUnderIceSummary_SEM%>%summarize(min=min(MeanUnderIce_HypoTemp_degC,na.rm=TRUE),max=max(MeanUnderIce_HypoTemp_degC,na.rm=TRUE))
  
  (gg.partialResid.lengthIceVsHypoTemp<-AnnualUnderIceSummary_SEM%>%mutate(MeanUnderIce_HypoTemp_degC_scale_Resids=MeanUnderIce_HypoTemp_degC_scale-(0.878*LengthOfIceCover_days_scale-0.754*IceOutDayofYear_fed_scale+0.046))%>%
    ggplot(.,aes(y=(MeanUnderIce_HypoTemp_degC_scale_Resids+0.878*LengthOfIceCover_days_scale),x=LengthOfIceCover_days_scale))+
    geom_point()+
    geom_line(aes(y=LengthOfIceCover_days_scale*0.878+0.046))+
    xlab("Ice duration (days)")+
    ylab(bquote(Under~ice~hypo.~(degree*C)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))+
    scale_x_continuous(breaks=breaks_LengthOfIceCover_days,labels=labels_LengthOfIceCover_days)+
    scale_y_continuous(breaks=breaks_MeanUnderIce_HypoTemp_degC,labels=labels2_MeanUnderIce_HypoTemp_degC,limits=c(-2.74,1.9))
    )
  
  
  #***Plot the partial residuals for the top variable for  water delta vs. hypo temp####
  AnnualUnderIceSummary_SEM%>%mutate(MeanDelta1_11mWaterDensity_kgperm3_scale_Resids=MeanDelta1_11mWaterDensity_kgperm3_scale-(-0.852*MeanUnderIce_HypoTemp_degC_scale+1.239*MeanUnderIce_EpiTemp_degC_scale-0.002))%>%
    ggplot(.,aes(y=(MeanDelta1_11mWaterDensity_kgperm3_scale_Resids-0.852*MeanUnderIce_HypoTemp_degC_scale),x=MeanUnderIce_HypoTemp_degC_scale))+
    geom_point()+
    geom_line(aes(y=-0.852*MeanUnderIce_HypoTemp_degC_scale-0.002))+
    xlab("Under ice hypo temp (z-score)")+
    ylab("Water density delta (z-score)")+
    theme_bw()
  
  #***Plot the partial residuals for the top variable for  water delta vs. epi temp####
  #****Create labels and breaks in the right spots to back transform####
  labels_MeanDelta1_11mWaterDensity_kgperm3<-c(-0.02,-0.01,0,0.01)
  breaks_MeanDelta1_11mWaterDensity_kgperm3<-(labels_MeanDelta1_11mWaterDensity_kgperm3-mean(AnnualUnderIceSummary_SEM$MeanDelta1_11mWaterDensity_kgperm3,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$MeanDelta1_11mWaterDensity_kgperm3,na.rm=TRUE)
  labels2_MeanDelta1_11mWaterDensity_kgperm3<-c("-0.02","-0.01","0.00","0.01")
  labels_MeanUnderIce_EpiTemp_degC<-c(2.5,3.0,3.5,4.0,4.5)
  breaks_MeanUnderIce_EpiTemp_degC<-(labels_MeanUnderIce_EpiTemp_degC-mean(AnnualUnderIceSummary_SEM$MeanUnderIce_EpiTemp_degC,na.rm=TRUE))/sd(AnnualUnderIceSummary_SEM$MeanUnderIce_EpiTemp_degC,na.rm=TRUE)
  labels2_MeanUnderIce_EpiTemp_degC<-c("2.5","3.0","3.5","4.0","4.5")
  
  #Check the max and min ranges
  AnnualUnderIceSummary_SEM%>%summarize(min=min(MeanDelta1_11mWaterDensity_kgperm3,na.rm=TRUE),max=max(MeanDelta1_11mWaterDensity_kgperm3,na.rm=TRUE))
  AnnualUnderIceSummary_SEM%>%summarize(min=min(MeanUnderIce_EpiTemp_degC,na.rm=TRUE),max=max(MeanUnderIce_EpiTemp_degC,na.rm=TRUE))
  
  (gg.partialResid.epiTempVsWaterDensity<-AnnualUnderIceSummary_SEM%>%mutate(MeanDelta1_11mWaterDensity_kgperm3_scale_Resids=MeanDelta1_11mWaterDensity_kgperm3_scale-(-0.852*MeanUnderIce_HypoTemp_degC_scale+1.239*MeanUnderIce_EpiTemp_degC_scale-0.002))%>%
    ggplot(.,aes(y=(MeanDelta1_11mWaterDensity_kgperm3_scale_Resids+1.239*MeanUnderIce_EpiTemp_degC_scale),x=MeanUnderIce_EpiTemp_degC_scale))+
    geom_point()+
    geom_line(aes(y=1.239*MeanUnderIce_EpiTemp_degC_scale-0.002))+
    xlab(bquote(Under~ice~epi.~temp.~(degree*C)))+
    ylab(bquote(Density~Delta~(kg~m^-3)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.y = element_text(angle = 90, hjust=0.5))+
    scale_x_continuous(breaks=breaks_MeanUnderIce_EpiTemp_degC,labels=labels2_MeanUnderIce_EpiTemp_degC)+
    scale_y_continuous(breaks=breaks_MeanDelta1_11mWaterDensity_kgperm3,labels=labels2_MeanDelta1_11mWaterDensity_kgperm3,limits=c(-2.9,2.3))
  )
  
 
  
  #***Plot the partial residuals for the top variable for  final heat content####
  AnnualUnderIceSummary_SEM%>%mutate(FinalHeatContent_MegaJoules_scale_Resids=FinalHeatContent_MegaJoules_scale-(0.481*LengthOfIceCover_days_scale-0.006*MeanDelta1_11mWaterDensity_kgperm3_scale+0.019))%>%
    ggplot(.,aes(y=(FinalHeatContent_MegaJoules_scale_Resids+0.481*LengthOfIceCover_days_scale),x=LengthOfIceCover_days_scale))+
    geom_point()+
    geom_line(aes(y=0.481*LengthOfIceCover_days_scale+0.019))+
    xlab("Length of ice cover (z-score)")+
    ylab("Final heat content (z-score)")+
    theme_bw()
  
  #***Plot the partial residuals for the top variable for slope heat content####
  AnnualUnderIceSummary_SEM%>%mutate(SlopeHeatContent_MegaJoulesperDay_scale_Resids=SlopeHeatContent_MegaJoulesperDay_scale-(-0.768*LengthOfIceCover_days_scale-0.215*MeanDelta1_11mWaterDensity_kgperm3_scale-0.036))%>%
    ggplot(.,aes(y=(SlopeHeatContent_MegaJoulesperDay_scale_Resids-0.768*LengthOfIceCover_days_scale),x=LengthOfIceCover_days_scale))+
    geom_point()+
    geom_line(aes(y=-0.768*LengthOfIceCover_days_scale-0.036))+
    xlab("Length of ice cover (z-score)")+
    ylab("Slope heat content (z-score)")+
    theme_bw()

#**correlation plot####
ggcorr(AnnualUnderIceSummary_SEM%>%
         dplyr::select(LengthOfIceCover_days_scale,IceInDayofYear_fed_scale,IceOutDayofYear_fed_scale,MeanUnderIce_HypoTemp_degC_scale, MeanUnderIce_EpiTemp_degC_scale,MeanDelta1_11mWaterDensity_kgperm3_scale,FinalHeatContent_MegaJoules_scale,SlopeHeatContent_MegaJoulesperDay_scale),
       nbreaks = 6, label = T, low = "red3", high = "green3", 
label_round = 2, name = "Correlation Scale", label_alpha = T, hjust = 0.75) +
  ggtitle(label = "Correlation Plot") +
  theme(plot.title = element_text(hjust = 0.6))

  #Need to create partial dependency plot of some sort--> or just use SEM to get out some of the more important relationships
  #https://www.researchgate.net/post/Is_it_possible_to_extract_a_score_for_all_observations_of_a_latent_variable_after_confirmatory_factor_analysis_If_yes_how
  
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

      ggplot(data=AnnualUnderIceSummary_SEM,aes(x=MeanUnderIce_HypoTemp_degC,y=MeanDelta1_11mWaterDensity_kgperm3))+geom_point()  
  
      

#Create customized ggplot of network from lavaan object####
      #https://www.ethan-young.com/code/sem-diagrams-for-lavaan-models/
      #*Extract all the parameters####
      lavaan_parameters <- parameterestimates(fit)
      #*Create graphical locations for each of the nodes####
      nodes <- lavaan_parameters %>% 
        dplyr::select(lhs) %>% 
        rename(name = lhs) %>% 
        distinct(name) %>% 
        mutate(
          x = case_when(name == "LengthOfIceCover_days_scale"      ~ 0.3,
                        name == "IceOutDayofYear_fed_scale"        ~ 0.7,
                        name == "MeanUnderIce_HypoTemp_degC_scale" ~ 0.05,
                        name == "MeanUnderIce_EpiTemp_degC_scale" ~ 0.5,
                        name == "LengthSpringMixedPeriod_days_scale" ~ 0.9,
                        name == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ 0.5),
          y = case_when(name == "LengthOfIceCover_days_scale"      ~ 0.8,
                        name == "IceOutDayofYear_fed_scale"        ~ 0.8,
                        name == "MeanUnderIce_HypoTemp_degC_scale" ~ 0.5,
                        name == "MeanUnderIce_EpiTemp_degC_scale" ~ 0.5,
                        name == "LengthSpringMixedPeriod_days_scale" ~ 0.5,
                        name == "MeanDelta1_11mWaterDensity_kgperm3_scale" ~ 0.2),
          xend = x,
          yend = y
        )
      #*Create graphical locations for each of the edges####
      edges <- lavaan_parameters %>%
        filter(op %in% c("~","~~"))
      #Combine edges and nodes
      combined <- nodes %>% 
        bind_rows(
          left_join(edges,nodes %>% select(name,x,y),by=c("lhs"="name")) %>%
            left_join(nodes %>% select(name,xend,yend),by = c("rhs"="name"))
        )
      #Create edge labels
      combined_edge_labels <- combined %>% 
        mutate(
          est = round(est,2),
          p.code     = ifelse(pvalue<.05,"p < .05","p > .05"),
          shape      = "observed",
          midpoint.x = (x + xend)/2,
          midpoint.y = (y + yend)/2,
          x2    = case_when(op=="~"~xend,
                            lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ 0.1,
                            lhs == "LengthOfIceCover_days_scale" & rhs == "IceOutDayofYear_fed_scale" ~ 0.35,
                            lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "LengthSpringMixedPeriod_days_scale"~0.55 
                            ),
          xend2 = ifelse(op=="~",x,xend),
          y2    = case_when(op=="~"~yend,
                            lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ y,
                            lhs == "LengthOfIceCover_days_scale" & rhs == "IceOutDayofYear_fed_scale" ~ y,
                            lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "LengthSpringMixedPeriod_days_scale"~0.245),
          yend2 = ifelse(op=="~",y,yend),
          rise = yend2-y2,
          run  = x2-xend2,
          dist = sqrt(run^2 + rise^2) %>% round(2),
          newx = case_when(lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "LengthOfIceCover_days_scale" ~ (x2 + (xend2 - x2) * 0.85),
                           lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "IceOutDayofYear_fed_scale" ~ (x2 + (xend2 - x2) * 0.92), 
                           lhs == "MeanUnderIce_EpiTemp_degC_scale" & rhs == "LengthOfIceCover_days_scale" ~ (x2 + (xend2 - x2) * .85),
                           lhs == "MeanUnderIce_EpiTemp_degC_scale" & rhs == "IceOutDayofYear_fed_scale" ~ (x2 + (xend2 - x2) * .85),
                           lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ (x2 + (xend2 - x2) * 0.89), 
                           lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ (x2 + (xend2 - x2) * .85),
                           lhs == "LengthSpringMixedPeriod_days_scale" & rhs == "IceOutDayofYear_fed_scale" ~ (x2 + (xend2 - x2) * .85),
                           lhs == "LengthSpringMixedPeriod_days_scale" & rhs == "LengthOfIceCover_days_scale" ~ (x2 + (xend2 - x2) * 0.92),
                           lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ 0.45,
                           lhs == "LengthOfIceCover_days_scale" & rhs == "IceOutDayofYear_fed_scale" ~ 0.65,
                           lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "LengthSpringMixedPeriod_days_scale" ~ (x2 + (xend2 - x2) * 0.87)  
                           ),
          newy = case_when(lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "LengthOfIceCover_days_scale" ~ (y2 + (yend2 - y2) * 0.68), 
                           lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "IceOutDayofYear_fed_scale" ~ (y2 + (yend2 - y2) * .85),
                           lhs == "MeanUnderIce_EpiTemp_degC_scale" & rhs == "LengthOfIceCover_days_scale" ~ (y2 + (yend2 - y2) * 0.7), 
                           lhs == "MeanUnderIce_EpiTemp_degC_scale" & rhs == "IceOutDayofYear_fed_scale" ~ (y2 + (yend2 - y2) * 0.7), 
                           lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ (y2 + (yend2 - y2) * .85),
                           lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ (y2 + (yend2 - y2) * 0.66), 
                           lhs == "LengthSpringMixedPeriod_days_scale" & rhs == "IceOutDayofYear_fed_scale" ~ (y2 + (yend2 - y2) * 0.69), 
                           lhs == "LengthSpringMixedPeriod_days_scale" & rhs == "LengthOfIceCover_days_scale" ~ (y2 + (yend2 - y2) * .85),
                           lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ (y2 + (yend2 - y2) * .85),
                           lhs == "LengthOfIceCover_days_scale" & rhs == "IceOutDayofYear_fed_scale" ~ (y2 + (yend2 - y2) * .85),
                           lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "LengthSpringMixedPeriod_days_scale" ~ (y2 + (yend2 - y2) * .85)
                           ),
          node.labels=case_when(name=="MeanUnderIce_HypoTemp_degC_scale" ~ as.character(expression(Hypo~degree*C)),
                                name=="MeanUnderIce_EpiTemp_degC_scale" ~ as.character(expression(Epi~degree*C)),
                                name=="MeanDelta1_11mWaterDensity_kgperm3_scale" ~ as.character(expression(paste("Dens ",Delta))),
                                name=="LengthSpringMixedPeriod_days_scale" ~ as.character(expression(paste("Spr Mix"))),
                                name=="LengthOfIceCover_days_scale" ~ as.character(expression(paste("Ice Dur"))),
                                name=="IceOutDayofYear_fed_scale" ~ as.character(expression(paste("Ice Off")))
                                ),
          arrow.ends=case_when(op=="~"~"last",
                               op=="~~"~"both"
                                 ),
          y2=ifelse(lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "LengthSpringMixedPeriod_days_scale",0.245,y2), #Adjust the start of this arrow
          edge_color=ifelse(est<0,"red","green"),
          #change the x and y value of the edge labels
          midpoint.x = case_when(lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "LengthOfIceCover_days_scale" ~ midpoint.x - 0.0,
                           lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "IceOutDayofYear_fed_scale" ~ midpoint.x-0.10, 
                           lhs == "MeanUnderIce_EpiTemp_degC_scale" & rhs == "LengthOfIceCover_days_scale" ~ midpoint.x-0.03, 
                           lhs == "MeanUnderIce_EpiTemp_degC_scale" & rhs == "IceOutDayofYear_fed_scale" ~ midpoint.x+0.03,  
                           lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ midpoint.x,
                           lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ midpoint.x, 
                           lhs == "LengthSpringMixedPeriod_days_scale" & rhs == "IceOutDayofYear_fed_scale" ~ midpoint.x+0.01, 
                           lhs == "LengthSpringMixedPeriod_days_scale" & rhs == "LengthOfIceCover_days_scale" ~ midpoint.x+0.09,
                           lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ midpoint.x,
                           lhs == "LengthOfIceCover_days_scale" & rhs == "IceOutDayofYear_fed_scale" ~ midpoint.x,
                           lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "LengthSpringMixedPeriod_days_scale" ~ midpoint.x
                            ),
          midpoint.y = case_when(lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "LengthOfIceCover_days_scale" ~ midpoint.y+0.02,
                                 lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "IceOutDayofYear_fed_scale" ~ midpoint.y-0.03, 
                                 lhs == "MeanUnderIce_EpiTemp_degC_scale" & rhs == "LengthOfIceCover_days_scale" ~ midpoint.y+0.055,  
                                 lhs == "MeanUnderIce_EpiTemp_degC_scale" & rhs == "IceOutDayofYear_fed_scale" ~ midpoint.y+0.055, 
                                 lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_HypoTemp_degC_scale" ~ midpoint.y+0.005, 
                                 lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ midpoint.y+0.02, 
                                 lhs == "LengthSpringMixedPeriod_days_scale" & rhs == "IceOutDayofYear_fed_scale" ~ midpoint.y+0.02, 
                                 lhs == "LengthSpringMixedPeriod_days_scale" & rhs == "LengthOfIceCover_days_scale" ~ midpoint.y-0.03, 
                                 lhs == "MeanUnderIce_HypoTemp_degC_scale" & rhs == "MeanUnderIce_EpiTemp_degC_scale" ~ midpoint.y,
                                 lhs == "LengthOfIceCover_days_scale" & rhs == "IceOutDayofYear_fed_scale" ~ midpoint.y,
                                 lhs == "MeanDelta1_11mWaterDensity_kgperm3_scale" & rhs == "LengthSpringMixedPeriod_days_scale" ~ midpoint.y
          ),
          #create a variable that is a factor for the weight of the edges
          edge.factor=factor(abs(est))
        )
      
      
      #Plot using ggplot and some of ggnetwork calls####
      (gg.networkplot<-ggplot(data=combined_edge_labels,aes(x = x, y = y, xend = xend, yend = yend)) +
        geom_edges(data=combined_edge_labels%>%filter(op=="~"),aes(x = x2, y = y2, xend = newx, yend = newy,color=edge_color,size=edge.factor),
                   arrow = arrow(length = unit(6, "pt"), type = "open",ends = "last"),lineend="round",linejoin="mitre") + #edges for the regressions
        geom_edges(data=combined_edge_labels%>%filter(op=="~~"),aes(x = x2, y = y2, xend = newx, yend = newy,color=edge_color,size=edge.factor),
                   arrow = arrow(length = unit(6, "pt"), type = "open",ends = "both"),lineend="round",linejoin="mitre",curvature=0) + #edges for the covariances
        geom_nodes(data=combined_edge_labels,aes(shape="observed"), color = "black",fill="white",size = 22,shape=22) +
        geom_nodetext(data=combined_edge_labels,aes(label = node.labels),parse=TRUE,fontface = "bold",size=4) + 
        geom_label(data=combined_edge_labels%>%filter(op%in%c("~","~~")&lhs!=rhs),aes(x = midpoint.x, y = midpoint.y, label = est,color=edge_color), label.size = NA,hjust = .5,vjust=.5,size=3.7,label.padding=unit(0.1,"lines")) +
        scale_y_continuous(expand = c(.07,0.07)) +
        scale_x_continuous(expand = c(.04,0.04)) +
        scale_shape_manual(values = c(15,19),guide=F) +
        scale_color_manual(values=c(rgb(75,174,76,maxColorValue = 255),rgb(203,84,80,maxColorValue = 255)))+
        scale_size_discrete(range=c(0.15,1.5))+
        theme_blank()+ 
        theme(legend.position = "none",plot.margin = unit(c(0.1,0.1,0.1,0.1),"pt"),
               axis.text.x=element_blank(),
               axis.ticks.x=element_blank(),
               axis.title.x=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank(),
               axis.title.y=element_blank(),
               )
       )
      
      #ggsave(paste("figures/MohonkWinterLimno-FigureX-SEMplot.jpg",sep=""),plot=gg.networkplot,width=3,height=2.5,units="in", dpi=300)

      #Alternative arrangement
      #****If we go with this one, modify arrow locations, increase size of the nodes****####
      (Fig5alt2<-plot_grid(gg.networkplot,
                           plot_grid(gg.partialResid.lengthIceVsHypoTemp,gg.partialResid.epiTempVsWaterDensity,gg.partialResid.iceOutVsSpringMixed,
                                     ncol=3,nrow=1,labels=c("b","c","d"),label_size=11,label_x=c(0.23,0.25,0.2),hjust=0,label_y=0.98)
                           ,ncol=1,nrow=2,labels=c("a",""),label_size=11,label_x=c(0.05),hjust=0,label_y=0.98,rel_heights = c(0.65,0.5)))
      
      ggsave("figures/Figure5.SEMplot4panelsPartialResids_alt.jpg", plot=Fig5alt2, width=7, height=5.3,units="in", dpi=300)
      
            
#Multiple panel plot for SEM####
      #Includes SEM graph and partial residuals for 3 of the relationships
        (gg.4panel.SEM<-plot_grid( 
          #left hand side - use cowplot plot_grid so they are separate margins
          plot_grid(gg.networkplot, 
                    gg.partialResid.lengthIceVsHypoTemp,
                    ncol=1,
                    nrow=2,
                    labels=c("a","c"),label_size=11),
          #Right hand side, use patchwork so they align
          plot_grid(gg.partialResid.iceOutVsSpringMixed, 
                    gg.partialResid.epiTempVsWaterDensity,
                    ncol=1,
                    nrow=2,
                    align="v",
                    labels=c("b","d"),label_size=11),
          ncol=2,nrow=1,align="hv")
        )
#*Export the four panel figure####        
ggsave(paste("figures/Figure5.SEMplot4panelsPartialResids.jpg",sep=""),plot=gg.4panel.SEM,width=6,height=5,units="in", dpi=600)
        
     

