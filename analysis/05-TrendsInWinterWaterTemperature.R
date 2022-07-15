#Script 05-TrendsInWinterWaterTemperature.R####
#Explore the winter water temperature and associated metrics for Mohonk Lake
#Created 15Jul2022, by David Richardson (DCR)


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

#Graph  each water year with ice in/out as vertical lines, and other variables as points####  
ggplot(data=DailyInterpol_winter%>%filter(wateryear==2001),aes(x=Date,y=DailyIceRecord_binomial))+geom_line()+
  geom_point(aes(x=Date,y=Temp_1m-Temp_11m))+
  scale_y_continuous(limit=c(-5,15))+
  theme_bw()


#Inverse stratification according to Woolway paper? Temp_1m-Temp_11m seems to go negative
#calculate the density gradient under the curve?
#Figure out other questions
#Create ridgeline plot in a new script
