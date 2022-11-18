ggplot(data=DailyInterpol%>%filter(year==2017),aes(x=Date,y=stability_Jperm2))+geom_point()

ggplot(data=DailyInterpol%>%filter(year==2017),aes(x=Date,y=thermoclineDepth_m_thresh0.1))+geom_smooth()

ggplot(data=MohonkWeeklyProfilesMetric.derivedData%>%filter(year==2017),aes(x=Date,y=stability_Jperm2))+geom_line()+
  geom_vline(xintercept=ymd("2017-04-07"),color="blue")+
  geom_vline(xintercept=ymd("2017-12-27"),color="blue")+
  geom_vline(xintercept=ymd("2017-01-01")+116,color="red")+
  geom_vline(xintercept=ymd("2017-01-01")+303,color="red")

ggplot(data=MohonkWeeklyProfilesMetric.derivedData%>%filter(year==2012),aes(x=Date,y=stability_Jperm2))+geom_line()+
  geom_vline(xintercept=ymd("2012-01-16"),color="blue")+
  geom_vline(xintercept=ymd("2012-03-16"),color="blue")+
  geom_vline(xintercept=ymd("2012-01-01")+97,color="red")+
  geom_vline(xintercept=ymd("2012-01-01")+308,color="red")

ggplot(data=MohonkWeeklyProfilesMetric.derivedData%>%filter(year==1989),aes(x=Date,y=stability_Jperm2))+geom_line()+
  geom_vline(xintercept=ymd("1989-12-05"),color="blue")+
  geom_vline(xintercept=ymd("1989-03-28"),color="blue")+
  geom_vline(xintercept=ymd("1989-01-01")+137,color="red")+
  geom_vline(xintercept=ymd("1989-01-01")+289,color="red")


ggplot(data=MohonkWeeklyProfilesMetric.derivedData%>%filter(year==1989),aes(x=Date,y=Temp_0m))+geom_line()+
  geom_vline(xintercept=ymd("1989-12-05"),color="blue")+
  geom_vline(xintercept=ymd("1989-03-28"),color="blue")+
  geom_vline(xintercept=ymd("1989-01-01")+137,color="red")+
  geom_vline(xintercept=ymd("1989-01-01")+289,color="red")+
  geom_hline(yintercept=0)

ggplot(data=MohonkWeeklyProfilesMetric.derivedData%>%filter(year==2012),aes(x=Date,y=Temp_0m))+geom_line()+
  geom_vline(xintercept=ymd("2012-01-16"),color="blue")+
  geom_vline(xintercept=ymd("2012-03-16"),color="blue")+
  geom_vline(xintercept=ymd("2012-01-01")+97,color="red")+
  geom_vline(xintercept=ymd("2012-01-01")+308,color="red")


