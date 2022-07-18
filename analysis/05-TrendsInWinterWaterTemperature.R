#Script 05-TrendsInWinterWaterTemperature.R####
#Explore the winter water temperature and associated metrics for Mohonk Lake
#Created 15Jul2022, by David Richardson (DCR)

#Packages####
if(!require(scales)){install.packages("scales")}
if(!require(ggridges)){install.packages("ggridges")}

#Load libraries
library(scales) #for pretty_breaks()
library(ggridges)

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
