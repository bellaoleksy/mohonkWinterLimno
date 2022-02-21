#R script created 20Dec2019 by DCR####

#Packages####
if(!require(reshape2)){install.packages("reshape2")}
if(!require(ggridges)){install.packages("ggridges")}
if(!require(egg)){install.packages("egg")}
if(!require(gridExtra)){install.packages("gridExtra")}

#Load libraries
library(reshape2)
library(ggridges)
library(egg)
library(gridExtra)

##EXPLORATORY FIGURE: Print out the heat maps for each year####
#Export a bunch of heat maps in one pdf
pdf(file=paste("figures/exploratory/MohonkPreserve-AnnualHeatMaps.pdf",sep=""), width=5, height=4,family="Times")

par(mfcol=c(1,1))
par(oma=c(4,4,1,1));  
par(mar=c(0,0,0,0))  

#Vector of all the years
years.vector<-as.numeric(levels(factor(MohonkWeeklyProfilesMetric.derivedData$year)))
for(kk in 1:length(years.vector)){
  heatmap.temp<-MohonkWeeklyProfilesMetric.derivedData[MohonkWeeklyProfilesMetric.derivedData$year==years.vector[kk],]
  #Format for heatmap
  heatmap.temp2<-data.frame(dateTime=as.POSIXct(heatmap.temp$Date,"yyyy-mm-dd hh:mm"),heatmap.temp[,5:18])
  names(heatmap.temp2)[15]<-"Temp_13m"
  #5:18 are columns for temperature
  NENA.HeatMapOne(heatmap.temp2,depths.v,years.vector[kk])
  
}

dev.off()

##EXPLORATORY FIGURE: Print out the thermistor profiles for each year####
#Export a bunch of heat maps in one pdf
pdf(file=paste("figures/exploratory/MohonkPreserve-AnnualThermistorProfiles.pdf",sep=""), width=5, height=4,family="Times")

par(mfcol=c(1,1))
par(oma=c(4,4,1,1));  
par(mar=c(0,0,0,0))  

#Vector of all the years
years.vector<-as.numeric(levels(factor(MohonkWeeklyProfilesMetric.derivedData$year)))
for(kk in 1:length(years.vector)){
  profiles.temp<-MohonkWeeklyProfilesMetric.derivedData[MohonkWeeklyProfilesMetric.derivedData$year==years.vector[kk],]
  colors<-rainbow(14)
  plot(profiles.temp$Temp_0m~profiles.temp$Date,col="white",ylim=c(0,30),main=years.vector[kk])
  points(profiles.temp$Temp_0m~profiles.temp$Date,col=colors[1])
  points(profiles.temp$Temp_1m~profiles.temp$Date,col=colors[2])
  points(profiles.temp$Temp_2m~profiles.temp$Date,col=colors[3])
  points(profiles.temp$Temp_3m~profiles.temp$Date,col=colors[4])
  points(profiles.temp$Temp_4m~profiles.temp$Date,col=colors[5])
  points(profiles.temp$Temp_5m~profiles.temp$Date,col=colors[6])
  points(profiles.temp$Temp_6m~profiles.temp$Date,col=colors[7])
  points(profiles.temp$Temp_7m~profiles.temp$Date,col=colors[8])
  points(profiles.temp$Temp_8m~profiles.temp$Date,col=colors[9])
  points(profiles.temp$Temp_9m~profiles.temp$Date,col=colors[10])
  points(profiles.temp$Temp_10m~profiles.temp$Date,col=colors[11])
  points(profiles.temp$Temp_11m~profiles.temp$Date,col=colors[12])
  points(profiles.temp$Temp_12m~profiles.temp$Date,col=colors[13])
  points(profiles.temp$Temp_Bottom~profiles.temp$Date,col=colors[14])
}
dev.off()

##EXPLORATORY FIGURE: Print out the thermistor profiles for the daily interpolation for each year####
#Export a bunch of heat maps in one pdf
pdf(file=paste("figures/exploratory/MohonkPreserve-AnnualThermistorProfilesDailyInterpol.pdf",sep=""), width=5, height=4,family="Times")

par(mfcol=c(1,1))
par(oma=c(4,4,1,1));  
par(mar=c(0,0,0,0))  

#Vector of all the years
years.vector<-as.numeric(levels(factor(DailyInterpol$year)))
for(kk in 1:length(years.vector)){
  profiles.temp<-DailyInterpol[DailyInterpol$year==years.vector[kk],]
  colors<-rainbow(14)
  plot(profiles.temp$Temp_0m~profiles.temp$Date,col="white",ylim=c(0,30),main=years.vector[kk])
  points(profiles.temp$Temp_0m~profiles.temp$Date,col=colors[1])
  points(profiles.temp$Temp_1m~profiles.temp$Date,col=colors[2])
  points(profiles.temp$Temp_2m~profiles.temp$Date,col=colors[3])
  points(profiles.temp$Temp_3m~profiles.temp$Date,col=colors[4])
  points(profiles.temp$Temp_4m~profiles.temp$Date,col=colors[5])
  points(profiles.temp$Temp_5m~profiles.temp$Date,col=colors[6])
  points(profiles.temp$Temp_6m~profiles.temp$Date,col=colors[7])
  points(profiles.temp$Temp_7m~profiles.temp$Date,col=colors[8])
  points(profiles.temp$Temp_8m~profiles.temp$Date,col=colors[9])
  points(profiles.temp$Temp_9m~profiles.temp$Date,col=colors[10])
  points(profiles.temp$Temp_10m~profiles.temp$Date,col=colors[11])
  points(profiles.temp$Temp_11m~profiles.temp$Date,col=colors[12])
  points(profiles.temp$Temp_12m~profiles.temp$Date,col=colors[13])
  points(profiles.temp$Temp_Bottom~profiles.temp$Date,col=colors[14])
}
dev.off()


####Weekly trends over the years####
####COULD MODIFY THIS FOR DIFFERENT VARIABLES; MAKE SURE TO INCLUDE CORRECTED P.VALUE
####Did it for Epi temp and BF 
####Look for weekly trends (weeks 1 to 52) over the years for the following variables:
#"thermoclineDepth_m_thresh0.1","stability_Jperm2","buoyancyfrequency_1_s2","EpiTemp_degC","HypoTemp_degC","VolumeWeightedMeanTemp_degC" 
#week.index<-1
weeklyAnalysis.DF<-data.frame(weekofyear=seq(1,52,by=1))

for(week.index in 1:52){
  y.var<-"stability_Jperm2"
  tmp.weekData<-DailyInterpol[DailyInterpol$weekofyear==week.index,c("year",y.var)]  
  #If 1985 is NA, remove it
  if(sum(!is.na(tmp.weekData[tmp.weekData$year==1985,y.var]))==0){tmp.weekData<-tmp.weekData[tmp.weekData$year>1985,]}
  tmp.weekData.agg<-setNames(aggregate(tmp.weekData[,y.var],by=list(tmp.weekData$year),FUN=mean,na.rm=T),c("year",y.var))
  #plot(tmp.weekData.agg[,y.var]~tmp.weekData.agg$year)
  #Extend data out to include missing years
  tmp.weekData.impute<-merge(tmp.weekData.agg,data.frame(year=seq(min(tmp.weekData.agg$year),max(tmp.weekData.agg$year),by=1)),by="year",all.y=T)
  
  #Impute data for 1997
  tmp.weekData.impute[tmp.weekData.impute$year==1997,y.var]<-mean(tmp.weekData.impute[tmp.weekData.impute$year==1995|tmp.weekData.impute$year==1996|tmp.weekData.impute$year==1998|tmp.weekData.impute$year==1999,y.var])
  #Impute data for 2014
  tmp.weekData.impute[tmp.weekData.impute$year==2014,y.var]<-mean(tmp.weekData.impute[tmp.weekData.impute$year==2012|tmp.weekData.impute$year==2013|tmp.weekData.impute$year==2015|tmp.weekData.impute$year==2016,y.var])
  
  
  ####*SENS SLOPE for the weekly variables of interest
  s.slope<-sens.slope(tmp.weekData.impute[,y.var])
  #Store the sen slope and the p value
  weeklyAnalysis.DF[week.index,paste("SenSlope_",y.var,sep="")]<-s.slope$estimates
  weeklyAnalysis.DF[week.index,paste("SenSlopePvalue_",y.var,sep="")]<-s.slope$p.value
}

#Need to adjust to alpha for 52 analyses
#Sidak adjusts to 0.0009859 or rounds to 0.001 for 52 comparisons
plot(weeklyAnalysis.DF[,paste("SenSlope_",y.var,sep="")]~weeklyAnalysis.DF$weekofyear)
points(weeklyAnalysis.DF[weeklyAnalysis.DF[,paste("SenSlopePvalue_",y.var,sep="")]<0.001,paste("SenSlope_",y.var,sep="")]~weeklyAnalysis.DF$weekofyear[weeklyAnalysis.DF[,paste("SenSlopePvalue_",y.var,sep="")]<0.001],col="blue",pch=19)
abline(h=0)

#EXPLORATORY FIGURES: Analysis of thermal structure ----------------------------
#Export a bunch of graphs in one pdf
pdf(file=paste("figures/exploratory/MohonkPreserve-AnalysisOfThermalStructure.pdf",sep=""), width=5, height=4,family="Times")

par(mfcol=c(1,1))
par(oma=c(4,4,1,1));  
par(mar=c(0,0,0,0))  

#Series of y labels
ylabels<-c(expression(Max~Stability~(J~m^-2)),
           expression(Day~of~Max~Stability),
           expression(Total~Stability~(J~m^-2~day)),
           expression(Start~of~Stratification~(Day)),
           expression(End~of~Stratification~(Day)),
           expression(Length~Of~Strat~(days)),
           expression(Summer~Epi~Temp~(degree*C)),
           expression(Summer~Hypo~Temp~(degree*C)),
           expression(Thermocline~Slope~(m~d^-1)),
           expression(Strat~Epi~Temp~(degree*C)),
           expression(Strat~Hypo~Temp~(degree*C)),
           expression(Date~Max~Stability),
           expression(Peak~Epi~Temp~(degree*C)),
           expression(Peak~Hypo~Temp~(degree*C)),
           expression(Mixing~Action~(gJ~d)))


for(annual.index in 2:length(names(AnnualData))){
  print(names(AnnualData)[annual.index])
  if(annual.index==13){}else{
    #Plot the annual maximum stability and do the linear regression
    plot(AnnualData[,annual.index]~AnnualData$Year,xaxt='n')
    mtext(ylabels[annual.index-1],side=2,line=2.2,cex=1)
    mtext("Year",side=1,line=2.2,cex=1)
    axis(1,at=seq(1985,2015,10),labels=seq(1985,2015,10))
    mtext(bquote(R^2*"="*.(round(summary(lm(AnnualData[,annual.index]~AnnualData$Year))$r.squared,3))~p*"="*.(round(summary(lm(AnnualData[,annual.index]~AnnualData$Year))$coefficients[2,4],3))),side=3,line=-1.35,adj=0.02)
    if(round(summary(lm(AnnualData[,annual.index]~AnnualData$Year))$coefficients[2,4],3)<0.05){
      abline(lm(AnnualData[,annual.index]~AnnualData$Year))
    }    
  }
}

dev.off()  

####EXPLORATORY FIGURE: Annual trends: Mixing action alone vs. year####
op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MohonkLake-YearInLife-ExploratoryFigure-MixingAction.jpg", width = 6, height = 4, units = "in",res = 300)
par(mfrow=c(1,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

#Series of y labels
ylabels<-c(expression(Surface~Temp~(degree*C)),expression(Deep~Temp~(degree*C)),expression(Mixing~Action~(GJ~day)))

#Plot the stratified season average temperature
plot(AnnualData$MixingAction_gigaJday~AnnualData$Year,xaxt='n',yaxt='n')
mtext(ylabels[3],side=2,line=2.2,cex=0.8)
axis(side=2,at=c(seq(3.3,4.5,0.3)))
#X axis labels    
mtext("Year",side=1,line=2.2,cex=0.8)
axis(1,at=seq(1985,2015,10),labels=seq(1985,2015,10))

dev.off()  
par(op)      

####EXPLORATORY FIGURE: Annual trends: Start, end, and length of stratification vs. year####
####Based on stability metrics for stratification
op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-StartEndLengthOfStratificationFigure.jpg", width = 3.3, height = 6.3, units = "in",res = 300)
par(mfrow=c(3,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

#Series of y labels
ylabels<-c(expression(Start~of~Strat~(day~of~year)),expression(End~of~Strat~(day~of~year)),expression(Length~of~strat~(days)))

#Plot the stratified season average temperature
plot(AnnualData$StartOfStratification_Day~AnnualData$Year,xaxt='n',yaxt='n',ylim=c(90,134))
mtext(ylabels[1],side=2,line=2.2,cex=0.8)
#Y axis label
axis(side=2,at=seq(90,130,by=10))

#Plot the stratified season average temperature
plot(AnnualData$EndOfStratification_Day~AnnualData$Year,xaxt='n',yaxt='n',ylim=c(280,314))
mtext(ylabels[2],side=2,line=2.2,cex=0.8)
#Y axis label
axis(side=2,at=seq(280,310,by=10))

#Plot the stratified season average temperature
plot(AnnualData$LengthOfStrat_days~AnnualData$Year,xaxt='n',ylim=c(160,217))
mtext(ylabels[3],side=2,line=2.2,cex=0.8)
#X axis labels    
mtext("Year",side=1,line=2.2,cex=0.8)
axis(1,at=seq(160,210,10))
axis(1,at=seq(1985,2015,10),labels=seq(1985,2015,10))

dev.off()  
par(op)

####STATS: Annual trends and breakpoint####

#*Sen slopes using DCR defined function that is drawn from zyp and trend functions####
#No longer need to impute values or use zyp.sen
#Deals with NA values
#Use MTCC.sensSlope(x=AnnualData$Year,y=AnnualData$VARIABLE) 
#create new df with variable name, sen slope, intercept, z, n, pvalue

#Melts Annual Data into long form using reshape2
AnnualData.long<-melt(AnnualData,id="Year")

#**Runs Theil Sen's slopes on each variable####
#Includes summary stats of significant slopes
AnnualData.SensSlopeSummary<-
  AnnualData.long%>%
  group_by(variable)%>%
  summarize(Sens_Slope=MTCC.sensSlope(x=Year,y=value)$coefficients["Year"],
            Sens_Intercept=MTCC.sensSlope(x=Year,y=value)$coefficients["Intercept"],
            Sens_pval=MTCC.sensSlope(x=Year,y=value)$pval,
            Sens_z_stat=MTCC.sensSlope(x=Year,y=value)$z_stat,
            Sens_n=MTCC.sensSlope(x=Year,y=value)$n)%>%
  mutate(Significance=ifelse(Sens_pval<0.05,"*","NS"))

#**write csv of the sens slopes summary####
write_csv(AnnualData.SensSlopeSummary,"figures/MTCC-TableX-SensSlopesAllVariables.csv")

#**Plot interannual trends of all variables####
ggplot(data=AnnualData.long,aes(x=Year,y=value))+
  geom_point()+
  facet_wrap(~variable,scale="free")

#**Plot only significant trends####
ggplot(data=left_join(AnnualData.long,AnnualData.SensSlopeSummary%>%select(variable,Significance),by="variable")%>%filter(Significance=="*"),aes(x=Year,y=value))+
  geom_point(col="red")+
  facet_wrap(~variable,scale="free")

#**Plot only non-significant trends####
ggplot(data=left_join(AnnualData.long,AnnualData.SensSlopeSummary%>%select(variable,Significance),by="variable")%>%filter(Significance=="NS"),aes(x=Year,y=value))+
  geom_point(col="black")+
  facet_wrap(~variable,scale="free")

#!!Could include breakpoint analysis using pettitt.test below####
#Should do it for all variables using smilar code to the sens slope implementation above
#pettitt.test(AnnualData[,"Temp.Hypo.peak"])



#STATS: Winter temperature analysis --------------------------------------------

DailyInterpol.winter<-DailyInterpol[DailyInterpol$dayofyear>=355|DailyInterpol$dayofyear<=79,]
#find the winter's first year
DailyInterpol.winter$winter.startyear<-NA
DailyInterpol.winter$winter.startyear[DailyInterpol.winter$dayofyear>=355]<-DailyInterpol.winter$year[DailyInterpol.winter$dayofyear>=355]
DailyInterpol.winter$winter.startyear[DailyInterpol.winter$dayofyear<=79]<-DailyInterpol.winter$year[DailyInterpol.winter$dayofyear<=79]-1

#find the winter's first year
DailyInterpol.winter$winter.endyear<-NA
DailyInterpol.winter$winter.endyear[DailyInterpol.winter$dayofyear>=355]<-DailyInterpol.winter$year[DailyInterpol.winter$dayofyear>=355]+1
DailyInterpol.winter$winter.endyear[DailyInterpol.winter$dayofyear<=79]<-DailyInterpol.winter$year[DailyInterpol.winter$dayofyear<=79]

#Create teh winter range
DailyInterpol.winter$winter.range<-paste(DailyInterpol.winter$winter.startyear," to ",DailyInterpol.winter$winter.endyear,sep="")

#Find the winter averages
AnnualData.winter<-aggregate(DailyInterpol.winter$Temp_1m,by=list(DailyInterpol.winter$winter.range),FUN=mean)
names(AnnualData.winter)<-c("winter.range","Temp_1m")
plot(AnnualData.winter$Temp_1m)

#Use 3 methods to determine ice on/off and length
#Bruesewitz, Pierson, visual
DailyInterpol$IceCover.Pierson0.1<-0  
DailyInterpol$IceCover.Pierson0.4<-0 
DailyInterpol$IceCover.Pierson0.1[DailyInterpol$Temp_10m-DailyInterpol$Temp_0m>0.1]<-1
DailyInterpol$IceCover.Pierson0.4[DailyInterpol$Temp_10m-DailyInterpol$Temp_0m>0.4]<-1

#Create ice cover variable from the visual observations from Mohonk DSRC
DailyInterpol$IceCover.Visual<-0  
for(iii in 1:length(MohonkIcePost1985$year)){
  DailyInterpol$IceCover.Visual[DailyInterpol$Date>=MohonkIcePost1985$IceInDate[iii]&DailyInterpol$Date<=MohonkIcePost1985$IceOutDate[iii]]<-1  
}

plot(DailyInterpol$IceCover.Pierson0.1~DailyInterpol$Date,col="white")
lines(DailyInterpol$IceCover.Pierson0.1~DailyInterpol$Date,col="green")
lines(DailyInterpol$IceCover.Pierson0.4-0.2~DailyInterpol$Date,col="blue")
lines(DailyInterpol$IceCover.Visual-0.4~DailyInterpol$Date,col="red")

#Check here the change in Ice length, ice in, ice out days
plot(MohonkIcePost1985$LengthOfIceCover_days~MohonkIcePost1985$year)
temp<-MohonkIcePost1985$IceInDayofYear
temp[MohonkIcePost1985$IceInDayofYear<200]<-temp[MohonkIcePost1985$IceInDayofYear<200]+365




#MANUSCRIPT FIGURES####


#*MS FIGURE: Composite Thermocline and Schmidt stability figure####
#Get the quantiles including min, 25th, median, 75th, max
tmp.composite<-aggregate(DailyInterpol$stability_Jperm2,
                         by=list(DailyInterpol$dayofyear),
                         FUN=quantile,na.rm=T)

tmp2.composite<-aggregate(DailyInterpol$stability_Jperm2,
                          by = list(DailyInterpol$dayofyear),
                          FUN = function(x) quantile(x, probs = c(0.05,0.95),na.rm=T))

Stability.composite<-data.frame(dayofyear=tmp.composite$Group.1,
                                Min.stability_Jperm2=tmp.composite[["x"]][,1],
                                Fifth.stability_Jperm2=tmp2.composite[["x"]][,1],
                                TwentyFifth.stability_Jperm2=tmp.composite[["x"]][,2],
                                Median.stability_Jperm2=tmp.composite[["x"]][,3],
                                SeventyFifth.stability_Jperm2=tmp.composite[["x"]][,4],
                                NinetyFifth.stability_Jperm2=tmp2.composite[["x"]][,2],
                                Max.stability_Jperm2=tmp.composite[["x"]][,5])  

#Do the same for thermocline depth
tmp.composite<-aggregate(DailyInterpol$thermoclineDepth_m_thresh0.1,
                         by=list(DailyInterpol$dayofyear),
                         FUN=quantile,
                         na.rm=T)

tmp2.composite<-aggregate(DailyInterpol$thermoclineDepth_m_thresh0.1,
                          by = list(DailyInterpol$dayofyear),
                          FUN = function(x) quantile(x, probs = c(0.05,0.95),
                                                     na.rm=T))

ThermoclineDepth.composite<-data.frame(dayofyear=tmp.composite$Group.1,
                                       Min.thermoclineDepth_m_thresh0.1=tmp.composite[["x"]][,1],
                                       Fifth.thermoclineDepth_m_thresh0.1=tmp2.composite[["x"]][,1],
                                       TwentyFifth.thermoclineDepth_m_thresh0.1=tmp.composite[["x"]][,2],
                                       Median.thermoclineDepth_m_thresh0.1=tmp.composite[["x"]][,3],
                                       SeventyFifth.thermoclineDepth_m_thresh0.1=tmp.composite[["x"]][,4],
                                       NinetyFifth.thermoclineDepth_m_thresh0.1=tmp2.composite[["x"]][,2],
                                       Max.thermoclineDepth_m_thresh0.1=tmp.composite[["x"]][,5])  

#Cutoff at these days, before 75, the median is all 0.5; after 321, there is a linear increase to 1 by about 0.4 per day
#Day 321 is about the maximum median thermocline depth - probably representing mixing of the top 
lo<-75; hi<-321

#FIGURE: Composite thermocline depth and stability figure vs. day of year summarizing over all years#
op <- par(no.readonly = TRUE)
jpeg("figures/MTCC-FigX-StabilityCompositeFigure.jpg", width = 3.3, height = 4.2, units = "in",res = 300)
par(mfrow=c(2,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

#Plot the composite thermocline depth figure with 25th, median, and 75th percentiles
plot(ThermoclineDepth.composite$Median.thermoclineDepth_m_thresh0.1~ThermoclineDepth.composite$dayofyear,
     col="white",
     ylim=c(max(ThermoclineDepth.composite$NinetyFifth.thermoclineDepth_m_thresh0.1,na.rm=T),0),
     ylab="",
     xlab="",
     yaxt='n',
     xaxt='n')

#Plot vertical dashed lines for spring/summer/fall cutoffs
abline(v=c(80,172,265),lty=2,col="dark grey")

#Cutoff at these days, before 75, the median is all 0.5; after 321, there is a linear increase to 1 by about 0.4 per day
#Day 321 is about the maximum median thermocline depth - probably representing mixing of the top 
lo<-75; hi<-321

#Draw in sky blue from 25th to 75th percentile
polygon(c(ThermoclineDepth.composite$dayofyear[lo:hi],
          rev(ThermoclineDepth.composite$dayofyear[lo:hi]),
          ThermoclineDepth.composite$dayofyear[lo]),
        c(ThermoclineDepth.composite$Fifth.thermoclineDepth_m_thresh0.1[lo:hi],
          rev(ThermoclineDepth.composite$NinetyFifth.thermoclineDepth_m_thresh0.1[lo:hi]),
          ThermoclineDepth.composite$Fifth.thermoclineDepth_m_thresh0.1[lo]),
        col = "lightskyblue1", border = NA)

#Draw in sky blue from 25th to 75th percentile
polygon(c(ThermoclineDepth.composite$dayofyear[lo:hi],
          rev(ThermoclineDepth.composite$dayofyear[lo:hi]),
          ThermoclineDepth.composite$dayofyear[lo]),
        c(ThermoclineDepth.composite$TwentyFifth.thermoclineDepth_m_thresh0.1[lo:hi],
          rev(ThermoclineDepth.composite$SeventyFifth.thermoclineDepth_m_thresh0.1[lo:hi]),
          ThermoclineDepth.composite$TwentyFifth.thermoclineDepth_m_thresh0.1[lo]),
        col = "deepskyblue3", border = NA)

lines(ThermoclineDepth.composite$Median.thermoclineDepth_m_thresh0.1[lo:hi]~ThermoclineDepth.composite$dayofyear[lo:hi],lwd=2)
lines(ThermoclineDepth.composite$SeventyFifth.thermoclineDepth_m_thresh0.1[lo:hi]~ThermoclineDepth.composite$dayofyear[lo:hi],col="blue")
lines(ThermoclineDepth.composite$TwentyFifth.thermoclineDepth_m_thresh0.1[lo:hi]~ThermoclineDepth.composite$dayofyear[lo:hi],col="blue")
lines(ThermoclineDepth.composite$Fifth.thermoclineDepth_m_thresh0.1[lo:hi]~ThermoclineDepth.composite$dayofyear[lo:hi],col="blue")
lines(ThermoclineDepth.composite$NinetyFifth.thermoclineDepth_m_thresh0.1[lo:hi]~ThermoclineDepth.composite$dayofyear[lo:hi],col="blue")

#Y axis label
mtext(side=2,
      line=2,
      expression(Thermocline~Depth~(m)),
      cex=0.8)



#Y axis
axis(side=2,
     at=c(0,6,12),
     cex.axis=0.8,
     mgp=c(3, .5, 0))
#X axis label
#mtext(side=1,line=2,expression(Day~of~the~year),cex=0.8)
#X axis
#axis(side=1,at=c(0,100,200,300),labels=c(0,100,200,300),cex.axis=0.8,mgp=c(3, .5, 0))

#Plot the composite Schmidt stability with 25th, median, and 75th percentiles
plot(Stability.composite$Median.stability_Jperm2~Stability.composite$dayofyear,
     col="white",
     ylim=c(0,max(Stability.composite$NinetyFifth.stability_Jperm2,na.rm=T)),
     ylab="",
     xlab="",
     yaxt='n'
     ,xaxt='n')
lo<-5; hi<-360

#Plot vertical dashed lines for spring/summer/fall cutoffs
abline(v=c(80,172,265),lty=2,col="dark grey")

#Draw in sky blue from 25th to 75th percentile
polygon(c(Stability.composite$dayofyear[lo:hi],
          rev(Stability.composite$dayofyear[lo:hi]),
          Stability.composite$dayofyear[lo]),
        c(Stability.composite$Fifth.stability_Jperm2[lo:hi],
          rev(Stability.composite$NinetyFifth.stability_Jperm2[lo:hi]),
          Stability.composite$Fifth.stability_Jperm2[lo]),
        col = "lightskyblue1", border = NA)

#Draw in sky blue from 25th to 75th percentile
polygon(c(Stability.composite$dayofyear[lo:hi],
          rev(Stability.composite$dayofyear[lo:hi]),
          Stability.composite$dayofyear[lo]),
        c(Stability.composite$TwentyFifth.stability_Jperm2[lo:hi],
          rev(Stability.composite$SeventyFifth.stability_Jperm2[lo:hi]),
          Stability.composite$TwentyFifth.stability_Jperm2[lo]),
        col = "deepskyblue3", border = NA)

lines(Stability.composite$Median.stability_Jperm2~Stability.composite$dayofyear,lwd=2)
lines(Stability.composite$SeventyFifth.stability_Jperm2~Stability.composite$dayofyear,col="blue")
lines(Stability.composite$TwentyFifth.stability_Jperm2~Stability.composite$dayofyear,col="blue")
lines(Stability.composite$Fifth.stability_Jperm2~Stability.composite$dayofyear,col="blue")
lines(Stability.composite$NinetyFifth.stability_Jperm2~Stability.composite$dayofyear,col="blue")
#Y axis label
mtext(side=2,line=2,expression(Schmidt~Stability~(J~m^-2)),cex=0.8)
#Y axis
axis(side=2,at=c(0,250,500),cex.axis=0.8,mgp=c(3, .5, 0))
#X axis label
mtext(side=1,line=2,expression(Day~of~the~year),cex=0.8)
#X axis
axis(side=1,at=c(0,100,200,300),labels=c(0,100,200,300),cex.axis=0.8,mgp=c(3, .5, 0))
#horizontal abline of 11 for stratification cutoff
abline(h=Stability.cutoff,lty=2,col="red")


dev.off()
par(op)

####*MS FIGURE: Annual trends: Summer Epi, Summer Hypo, Total Stratification vs. year####
####This is the average June 21 to Sep 21 for 1,2,3m for surface and 10,11,12 meters for deep
####Perhaps this should be average temperature during stratified period
op <- par(no.readonly = TRUE)
jpeg("figures/MTCC-FigX-SummerEpiHypoTotalStratificationFigure.jpg",
     width = 3.3, height = 6.3,
     units = "in",res = 300)
par(mfrow=c(3,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

#Series of y labels
ylabels<-c(expression(Surface~Temp~(degree*C)),
           expression(Deep~Temp~(degree*C)),
           expression(Mixing~Action~(GJ~day)))

#Plot the summer average temperature
plot(AnnualData$SurfaceWaterTemp_Summer_degC~AnnualData$Year,
     xaxt='n',yaxt='n',ylim=c(21,24.5))

mtext(ylabels[1],side=2,line=2.2,cex=0.8)

#Y axis label
axis(side=2,at=c(21,22,23,24))

#Plot the stratified season average temperature
plot(AnnualData$DeepWaterTemp_Summer_degC~AnnualData$Year,
     xaxt='n',yaxt='n')

mtext(ylabels[2],side=2,line=2.2,cex=0.8)

#Y axis label
axis(side=2,at=c(6,7,8))

#Plot the stratified season average temperature
plot(AnnualData$MixingAction_gigaJday~AnnualData$Year,
     xaxt='n',yaxt='n')

mtext(ylabels[3],side=2,line=2.2,cex=0.8)

axis(side=2,at=c(seq(3.3,4.5,0.3)))

#X axis labels    
mtext("Year",side=1,line=2.2,cex=0.8)

axis(1,at=seq(1985,2015,10),labels=seq(1985,2015,10))

dev.off()  
par(op)

####*EXPLORATORY FIGURE: Annual trends: Stratification phenology vs. year####
####This is a composite figure that shows the start of stratification, end of stratification, peak of stratifcation, and length (grey bars)

#Create yhat for date of stratification onset
AnnualData.Predicted<-AnnualData%>%select(Year)%>%
                      mutate(StartOfStratification_day_yhat=Year*as.numeric(AnnualData.SensSlopeSummary%>%filter(variable=="StartOfStratification_Day")%>%select(Sens_Slope))+as.numeric(AnnualData.SensSlopeSummary%>%filter(variable=="StartOfStratification_Day")%>%select(Sens_Intercept)))%>%
                      mutate(LengthOfStratification_day_yhat=Year*as.numeric(AnnualData.SensSlopeSummary%>%filter(variable=="LengthOfStrat_days")%>%select(Sens_Slope))+as.numeric(AnnualData.SensSlopeSummary%>%filter(variable=="LengthOfStrat_days")%>%select(Sens_Intercept)))%>%
                      mutate(MixingAction_gigaJday_yhat=Year*as.numeric(AnnualData.SensSlopeSummary%>%filter(variable=="MixingAction_gigaJday")%>%select(Sens_Slope))+as.numeric(AnnualData.SensSlopeSummary%>%filter(variable=="MixingAction_gigaJday")%>%select(Sens_Intercept)))

#**Phenology of stratification plot####
gg.stratificationPhenology<-ggplot()+
  geom_segment(data=AnnualData,aes(x=Year,xend=Year,y=StartOfStratification_Day,yend=EndOfStratification_Day),col="grey")+
  geom_line(data=AnnualData.Predicted,aes(x=Year,y=StartOfStratification_day_yhat),color="skyblue",lty=1)+
  geom_point(data=AnnualData,aes(x=Year,y=StartOfStratification_Day))+
  geom_point(data=AnnualData,aes(x=Year,y=EndOfStratification_Day))+
  geom_point(data=AnnualData,aes(x=Year,y=DayMaxStability),col="red",shape=1)+
  #ylim(320,90)+
  scale_y_reverse(lim=c(320,90),breaks=seq(300,100,by=-50))+
  scale_x_continuous(limit=c(1984,2018),breaks=seq(1985,2015,by=5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Year")+
  ylab(bquote(Day~Of~Year))

#**Length of stratification plot####
gg.lengthOfStratification<-ggplot()+
  geom_line(data=AnnualData.Predicted,aes(x=Year,y=LengthOfStratification_day_yhat),color="skyblue",lty=1)+
  geom_point(data=AnnualData,aes(x=Year,y=LengthOfStrat_days))+
  scale_y_continuous(limit=c(160,220),breaks=seq(160,220,by=20))+
  scale_x_continuous(limit=c(1984,2018),breaks=seq(1985,2015,by=5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Year")+
  ylab(bquote(Length~of~strat.~(days)))

#**Mixing action solo plot####
gg.mixingAction<-ggplot()+
  geom_line(data=AnnualData.Predicted,aes(x=Year,y=MixingAction_gigaJday_yhat),color="skyblue",lty=1)+
  geom_point(data=AnnualData,aes(x=Year,y=MixingAction_gigaJday))+
  ylim(3.0,4.6)+
  scale_x_continuous(limit=c(1984,2018),breaks=seq(1985,2015,by=10))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Year")+
  ylab(bquote(Mixing~Action~(GJ~d^-1)))

#***Export Phenology of stratification plot####
op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-StratificationPhenology.jpg",
     width = 3.3, height = 2.6,
     units = "in",res = 300)
par(mfrow=c(1,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

gg.stratificationPhenology

dev.off()  
par(op)

#***Export Length of stratification plot####
op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-LengthOfStratification.jpg",
     width = 3.3, height = 2.6,
     units = "in",res = 300)
par(mfrow=c(1,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

gg.lengthOfStratification

dev.off()  
par(op)

#***Export Mixing action solo plot####
op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-MixingAction.jpg",
     width = 3.3, height = 2.6,
     units = "in",res = 300)
par(mfrow=c(1,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

gg.mixingAction

dev.off()  
par(op)

#***arrange into a multi-panel plot####
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
gg.right.panel<-ggarrange(gg.stratificationPhenology+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),plot.margin=unit(c(2,2,0,2), "lines"),axis.ticks.length.x = unit(0, "pt"))+labs(x=NULL, title=NULL),
          gg.lengthOfStratification+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),plot.margin=unit(c(0,2,0,2), "lines"),axis.ticks.length.x = unit(0, "pt"))+labs(x=NULL, title=NULL),
          gg.mixingAction+theme(plot.margin=unit(c(0,2,2,2), "lines")),
          nrow=3,debug=FALSE)
gg.right.panel
#top,right,bottom,left 

#***Export stratification 3 panel plot####
op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-StratificationThreePanel.jpg",
     width = 3.5, height = 2.3*3,
     units = "in",res = 300)
par(mfrow=c(1,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

gg.right.panel

dev.off()  
par(op)

#*EXPLORATORY FIGURES: try to do ridgelines####
####This could also be one of those cool stacked histogram figures: density ridge plots
####http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/
####Would have to cut DailyInterpol by the start and end of strat (stability.cutoff) then
####https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

#**Subset out 1985 data to graph ridgeline
#temp1985<-DailyInterpol%>%select(year,dayofyear,stability_Jperm2)%>%filter(stability_Jperm2>Stability.cutoff)%>%filter(!is.na(stability_Jperm2))%>%filter(year==1985)
#ggplot(temp1,aes(x=dayofyear,y=rep(1,length(temp1$dayofyear)),height=stability_Jperm2))+
  #geom_ridgeline()

#**Overlapping ridgelines - subset out everything without 2014
Ridgelines.DF<-DailyInterpol%>%select(year,dayofyear,stability_Jperm2)%>%filter(stability_Jperm2>Stability.cutoff)%>%filter(!is.na(stability_Jperm2))%>%filter(year!=2014)%>%filter(year!=1997)
#ggplot(Ridgelines.DF,aes(x=dayofyear,y=year,height=stability_Jperm2,group=year))+
  #geom_ridgeline(fill="lightblue")

#**Here the heights are automatically scale such that the highest ridgeline just 
#touches the one above it at scale=1
#ggplot(Ridgelines.DF,aes(x=dayofyear,y=year,height=stability_Jperm2,group=year))+
  #geom_density_ridges(stat="identity",scale=5,fill="lightblue")

#Gridlines for the plot
segment_data = data.frame(
  x = c(seq(100,300,by=50)),
  xend = c(seq(100,300,by=50)), 
  y = rep(1985,length(seq(100,300,by=50))),
  yend =rep(2017,length(seq(100,300,by=50)))
)

#**Make them a little pretty
figure.ridgelines<-ggplot()+
  geom_hline(yintercept=c(seq(1985,2017,by=1)),col="light grey")+
  geom_segment(data=segment_data,aes(x=x,y=y,xend=xend,yend=yend),col="light grey")+
  #geom_vline(xintercept=c(seq(100,300,by=50)),col="light grey")+
  geom_density_ridges_gradient(data=Ridgelines.DF,aes(x=dayofyear,y=year,height=stability_Jperm2,group=year,fill=stat(height)),stat="identity",scale=2)+
  scale_fill_viridis_c(option="C")+
  ylab("Year")+
  xlab("Day of Year")+
  theme_bw()+  
  theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),plot.margin=unit(c(2,0.2,2,0.2), "lines"),legend.box.background = element_blank(),plot.background = element_blank(),legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,0,-10,-5))+
  labs(fill=expression(atop("St",paste("(J m"^-2*")"))))+
  scale_y_continuous(limit=c(1985,2019),breaks=c(seq(1985,2015,by=5)))
  


op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-StabilityRidgelines.jpg", width = 4, height = 5.5, units = "in",res = 300)
par(mfrow=c(1,1))
par(mar = c(0,0,0,0))
par(oma = c(4,4,2,2))

figure.ridgelines

dev.off()
par(op)

#**EXPLORATORY FIGURE: Plot into ridgelines on the left; three panel dot plots on the right####
#!!STOPPED HERE: MINIMIZE THE GAP BETWEEN THE PLOTS####
op <- par(no.readonly = TRUE)
jpeg("figures/exploratory/MTCC-ExploratoryFig-StabilityFourPanels.jpg", width = 6.6, height = 6.9, units = "in",res = 600)

grid.arrange(
  figure.ridgelines,gg.right.panel,
  widths = c(1, 1),
  layout_matrix = rbind(c(1, 2),
                        c(1, 2))
)

dev.off()
par(op)


#**!!!Make a horizontal ridgelines plot - NOT WORKING CORRECTLY, ####
figure.ridgelines.horizontal<-ggplot()+
  geom_vline(xintercept=c(seq(1985,2017,by=1)),col="light grey")+
  geom_segment(data=segment_data,aes(x=y,y=x,xend=yend,yend=xend),col="light grey")+
  #geom_vline(xintercept=c(seq(100,300,by=50)),col="light grey")+
  geom_density_ridges_gradient(data=Ridgelines.DF,aes(y=dayofyear,x=year,height=stability_Jperm2,group=year,fill=stat(height)),stat="identity",scale=5)+
  scale_fill_viridis_c(option="C")+
  ylab("Day of Year")+
  xlab("Year")+
  theme_bw()+  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),plot.margin=unit(c(2,0.5,2,2), "lines"))+
  labs(fill=bquote(St~(J~m^-2)))+
  scale_x_continuous(limit=c(1985,2019),breaks=c(seq(1985,2015,by=5)))
