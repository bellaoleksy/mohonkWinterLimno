source('analysis/01_Isotherm.R') #this script already sources 00_main.R


# Table S3.  Sens slope climate trends ------------------------------------
options(scipen = 100, digits = 5)


Climate.SensSlopeSummary<-
  MohonkIceWeather %>%
  select(1,6:ncol(.)) %>%
  pivot_longer(-1) %>%
  filter(Year>=1932) %>%
  group_by(name)%>%
  dplyr::summarize(Sens_Slope=MTCC.sensSlope(x=Year,y=value)$coefficients["Year"],
                   Sens_Intercept=MTCC.sensSlope(x=Year,y=value)$coefficients["Intercept"],
                   Sens_pval=MTCC.sensSlope(x=Year,y=value)$pval,
                   Sens_z_stat=MTCC.sensSlope(x=Year,y=value)$z_stat,
                   Sens_n=MTCC.sensSlope(x=Year,y=value)$n) %>%
  mutate(Significance=case_when(Sens_pval>=0.05 ~ " ",
                                Sens_pval<0.05 & Sens_pval >0.01 ~ "*",
                                Sens_pval<=0.01 & Sens_pval >0.001 ~ "**",
                                Sens_pval<=0.001 ~ "***")) %>%
  mutate_if(is.numeric, round, 4) %>%
  # mutate(Significance=ifelse(Sens_pval<0.05,"*",""),
  mutate(P_value_new=paste(Sens_pval,Significance,sep="")) %>%
  arrange(name, Significance) %>%
  select(-Significance,-Sens_pval) %>%
  filter(!grepl('ENSO|NAO', name)) #There is no evidence of trends in ENSO or
#NAO variables so taking these out of the table for the sake of space

Climate.SensSlopeSummary_hux <- 
  hux(Climate.SensSlopeSummary) %>% 
  # add_colnames() %>% 
  set_bold(row = 1, col = everywhere, value = TRUE) %>% 
  set_all_borders(TRUE) 

theme_plain(Climate.SensSlopeSummary_hux)
quick_docx(Climate.SensSlopeSummary_hux, file = 'figures/MS/TableS3.SensSlopesClimateVariables.docx')



#Check a few
MohonkIceWeather %>%
  ggplot(aes(x=Year, y=nDaysMeanBelowZero_Oct))+
  geom_point()


# ~~ FIGURE S2.  MEAN Monthly temp.  trends  -----------------------------------



# Fitting GAMs for mean Jan temperature -------------------------------------------
JanWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="1") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=mean(TempMean_degC, na.rm=TRUE)) %>%
  mutate(Month="Jan")


### Model
modJanTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                       data = JanWx,
                       method = "REML")


###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
JanTempMeanPred <- with(JanWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                           max(water_year, na.rm=TRUE),
                                                           length.out = 200)))
JanTempMeanPred <- cbind(JanTempMeanPred, data.frame(predict(modJanTempMean$gam, JanTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
JanTempMeanPred <- transform(JanTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(Month="Jan")


m1.dsig <- signifD(JanTempMeanPred$fit,
                   d = JanTempMeanPred$deriv,
                   JanTempMeanPred$upper,
                   JanTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modJanTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(JanTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Jan_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=JanTempMeanPred$water_year) %>%
  left_join(.,JanTempMeanPred) %>%
  mutate(Month="Jan")

JanWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=JanTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=JanTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=JanTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Jan_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)



# Fitting GAMs for mean Feb temperature -------------------------------------------
FebWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="2") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=mean(TempMean_degC, na.rm=TRUE)) %>%
  mutate(Month="Feb")


### Model
modFebTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                       data = FebWx,
                       method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
FebTempMeanPred <- with(FebWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                           max(water_year, na.rm=TRUE),
                                                           length.out = 200)))
FebTempMeanPred <- cbind(FebTempMeanPred, data.frame(predict(modFebTempMean$gam, FebTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
FebTempMeanPred <- transform(FebTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(Month="Feb")


m1.dsig <- signifD(FebTempMeanPred$fit,
                   d = FebTempMeanPred$deriv,
                   FebTempMeanPred$upper,
                   FebTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modFebTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(FebTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Feb_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=FebTempMeanPred$water_year) %>%
  # left_join(.,FebTempMeanPred) %>%
  mutate(Month="Feb")

FebWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=FebTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=FebTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=FebTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Feb_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

#For ASLO talk
FebWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=FebTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=FebTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=FebTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Feb_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)+
  # geom_hline(yintercept=0, linetype="longdash", color="grey50")+
  ggdark::dark_theme_bw(base_size=10)+
  labs(y="Feb. mean air temperatures (°C)",
       x="Year")
ggsave(
  "figures/ASLO/Fig4a.FebGAMS.jpg",
  width = 3,
  height = 3,
  units = "in",
  dpi = 600
)


# Fitting GAMs for mean March temperature -------------------------------------------
MarchWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="3") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=mean(TempMean_degC, na.rm=TRUE)) %>%
  mutate(Month="March")


### Model
modMarchTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                         data = MarchWx,
                         method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
MarchTempMeanPred <- with(MarchWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                               max(water_year, na.rm=TRUE),
                                                               length.out = 200)))
MarchTempMeanPred <- cbind(MarchTempMeanPred, data.frame(predict(modMarchTempMean$gam, MarchTempMeanPred,
                                                                 type="response",
                                                                 se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
MarchTempMeanPred <- transform(MarchTempMeanPred, upper = fit + (2 * se.fit),
                               lower = fit - (2 * se.fit)) %>%
  mutate(Month="March")


m1.dsig <- signifD(MarchTempMeanPred$fit,
                   d = MarchTempMeanPred$deriv,
                   MarchTempMeanPred$upper,
                   MarchTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modMarchTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(MarchTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

March_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=MarchTempMeanPred$water_year) %>%
  # left_join(.,MarchTempMeanPred) %>%
  mutate(Month="March")

MarchWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=MarchTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=MarchTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=MarchTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=March_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)



# Fitting GAMs for mean April temperature -------------------------------------------
AprilWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="4") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=mean(TempMean_degC, na.rm=TRUE)) %>%
  mutate(Month="April")


### Model
modAprilTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                         data = AprilWx,
                         method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
AprilTempMeanPred <- with(AprilWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                               max(water_year, na.rm=TRUE),
                                                               length.out = 200)))
AprilTempMeanPred <- cbind(AprilTempMeanPred, data.frame(predict(modAprilTempMean$gam, AprilTempMeanPred,
                                                                 type="response",
                                                                 se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
AprilTempMeanPred <- transform(AprilTempMeanPred, upper = fit + (2 * se.fit),
                               lower = fit - (2 * se.fit)) %>%
  mutate(Month="April")


m1.dsig <- signifD(AprilTempMeanPred$fit,
                   d = AprilTempMeanPred$deriv,
                   AprilTempMeanPred$upper,
                   AprilTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modAprilTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(AprilTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

April_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=AprilTempMeanPred$water_year) %>%
  # left_join(.,AprilTempMeanPred) %>%
  mutate(Month="April")

AprilWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=AprilTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=AprilTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=AprilTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=April_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean May temperature -------------------------------------------
MayWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="5") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=mean(TempMean_degC, na.rm=TRUE)) %>%
  mutate(Month="May")


### Model
modMayTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                       data = MayWx,
                       method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
MayTempMeanPred <- with(MayWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                           max(water_year, na.rm=TRUE),
                                                           length.out = 200)))
MayTempMeanPred <- cbind(MayTempMeanPred, data.frame(predict(modMayTempMean$gam, MayTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
MayTempMeanPred <- transform(MayTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(Month="May")


m1.dsig <- signifD(MayTempMeanPred$fit,
                   d = MayTempMeanPred$deriv,
                   MayTempMeanPred$upper,
                   MayTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modMayTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(MayTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

May_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=MayTempMeanPred$water_year) %>%
  # left_join(.,MayTempMeanPred) %>%
  mutate(Month="May")

MayWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=MayTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=MayTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=MayTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=May_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)


# Fitting GAMs for mean June temperature -------------------------------------------
JuneWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="6") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=mean(TempMean_degC, na.rm=TRUE)) %>%
  mutate(Month="June")


### Model
modJuneTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                        data = JuneWx,
                        method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
JuneTempMeanPred <- with(JuneWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                             max(water_year, na.rm=TRUE),
                                                             length.out = 200)))
JuneTempMeanPred <- cbind(JuneTempMeanPred, data.frame(predict(modJuneTempMean$gam, JuneTempMeanPred,
                                                               type="response",
                                                               se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
JuneTempMeanPred <- transform(JuneTempMeanPred, upper = fit + (2 * se.fit),
                              lower = fit - (2 * se.fit)) %>%
  mutate(Month="June")


m1.dsig <- signifD(JuneTempMeanPred$fit,
                   d = JuneTempMeanPred$deriv,
                   JuneTempMeanPred$upper,
                   JuneTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modJuneTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(JuneTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

June_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=JuneTempMeanPred$water_year) %>%
  # left_join(.,JuneTempMeanPred) %>%
  mutate(Month="June")

JuneWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=JuneTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=JuneTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=JuneTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=June_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean July temperature -------------------------------------------
JulyWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="7") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=mean(TempMean_degC, na.rm=TRUE)) %>%
  mutate(Month="July")


### Model
modJulyTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                        data = JulyWx,
                        method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
JulyTempMeanPred <- with(JulyWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                             max(water_year, na.rm=TRUE),
                                                             length.out = 200)))
JulyTempMeanPred <- cbind(JulyTempMeanPred, data.frame(predict(modJulyTempMean$gam, JulyTempMeanPred,
                                                               type="response",
                                                               se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
JulyTempMeanPred <- transform(JulyTempMeanPred, upper = fit + (2 * se.fit),
                              lower = fit - (2 * se.fit)) %>%
  mutate(Month="July")


m1.dsig <- signifD(JulyTempMeanPred$fit,
                   d = JulyTempMeanPred$deriv,
                   JulyTempMeanPred$upper,
                   JulyTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modJulyTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(JulyTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

July_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=JulyTempMeanPred$water_year) %>%
  # left_join(.,JulyTempMeanPred) %>%
  mutate(Month="July")

JulyWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=JulyTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=JulyTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=JulyTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=July_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean Aug temperature -------------------------------------------
AugWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="8") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=mean(TempMean_degC, na.rm=TRUE)) %>%
  mutate(Month="Aug")


### Model
modAugTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                       data = AugWx,
                       method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
AugTempMeanPred <- with(AugWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                           max(water_year, na.rm=TRUE),
                                                           length.out = 200)))
AugTempMeanPred <- cbind(AugTempMeanPred, data.frame(predict(modAugTempMean$gam, AugTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
AugTempMeanPred <- transform(AugTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(Month="Aug")


m1.dsig <- signifD(AugTempMeanPred$fit,
                   d = AugTempMeanPred$deriv,
                   AugTempMeanPred$upper,
                   AugTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modAugTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(AugTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Aug_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=AugTempMeanPred$water_year) %>%
  # left_join(.,AugTempMeanPred) %>%
  mutate(Month="Aug")

AugWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=AugTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=AugTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=AugTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Aug_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean Sept temperature -------------------------------------------
SeptWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="9") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=mean(TempMean_degC, na.rm=TRUE)) %>%
  mutate(Month="Sept")


### Model
modSeptTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                        data = SeptWx,
                        method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
SeptTempMeanPred <- with(SeptWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                             max(water_year, na.rm=TRUE),
                                                             length.out = 200)))
SeptTempMeanPred <- cbind(SeptTempMeanPred, data.frame(predict(modSeptTempMean$gam, SeptTempMeanPred,
                                                               type="response",
                                                               se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
SeptTempMeanPred <- transform(SeptTempMeanPred, upper = fit + (2 * se.fit),
                              lower = fit - (2 * se.fit)) %>%
  mutate(Month="Sept")


m1.dsig <- signifD(SeptTempMeanPred$fit,
                   d = SeptTempMeanPred$deriv,
                   SeptTempMeanPred$upper,
                   SeptTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modSeptTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(SeptTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Sept_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=SeptTempMeanPred$water_year) %>%
  # left_join(.,SeptTempMeanPred) %>%
  mutate(Month="Sept")

SeptWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=SeptTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=SeptTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=SeptTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Sept_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean Oct temperature -------------------------------------------
OctWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="10") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=mean(TempMean_degC, na.rm=TRUE)) %>%
  mutate(Month="Oct")


### Model
modOctTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                       data = OctWx,
                       method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
OctTempMeanPred <- with(OctWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                           max(water_year, na.rm=TRUE),
                                                           length.out = 200)))
OctTempMeanPred <- cbind(OctTempMeanPred, data.frame(predict(modOctTempMean$gam, OctTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
OctTempMeanPred <- transform(OctTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(Month="Oct")


m1.dsig <- signifD(OctTempMeanPred$fit,
                   d = OctTempMeanPred$deriv,
                   OctTempMeanPred$upper,
                   OctTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modOctTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(OctTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Oct_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=OctTempMeanPred$water_year) %>%
  # left_join(.,OctTempMeanPred) %>%
  mutate(Month="Oct")

OctWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=OctTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=OctTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=OctTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Oct_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean Nov temperature -------------------------------------------
NovWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="11") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=mean(TempMean_degC, na.rm=TRUE)) %>%
  mutate(Month="Nov")


### Model
modNovTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                       data = NovWx,
                       method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
NovTempMeanPred <- with(NovWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                           max(water_year, na.rm=TRUE),
                                                           length.out = 200)))
NovTempMeanPred <- cbind(NovTempMeanPred, data.frame(predict(modNovTempMean$gam, NovTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
NovTempMeanPred <- transform(NovTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(Month="Nov")


m1.dsig <- signifD(NovTempMeanPred$fit,
                   d = NovTempMeanPred$deriv,
                   NovTempMeanPred$upper,
                   NovTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modNovTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(NovTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Nov_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=NovTempMeanPred$water_year) %>%
  # left_join(.,NovTempMeanPred) %>%
  mutate(Month="Nov")

NovWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=NovTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=NovTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=NovTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Nov_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean Dec temperature -------------------------------------------
DecWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="12") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=mean(TempMean_degC, na.rm=TRUE)) %>%
  mutate(Month="Dec")


### Model
modDecTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                       data = DecWx,
                       method = "REML")
# summary(modDecTempMean)
###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
DecTempMeanPred <- with(DecWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                           max(water_year, na.rm=TRUE),
                                                           length.out = 200)))
DecTempMeanPred <- cbind(DecTempMeanPred, data.frame(predict(modDecTempMean$gam, DecTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
DecTempMeanPred <- transform(DecTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(Month="Dec")


m1.dsig <- signifD(DecTempMeanPred$fit,
                   d = DecTempMeanPred$deriv,
                   DecTempMeanPred$upper,
                   DecTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modDecTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(DecTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Dec_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=DecTempMeanPred$water_year) %>%
  # left_join(.,DecTempMeanPred) %>%
  mutate(Month="Dec")

DecWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=DecTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=DecTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=DecTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Dec_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

#For ASLO talk
DecWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=DecTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=DecTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=DecTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Dec_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)+
  ggdark::dark_theme_bw(base_size=10)+
  labs(y="December mean air temperatures (°C)",
       x="Year")
ggsave(
  "figures/ASLO/Fig4.DecGAMS.jpg",
  width = 3,
  height = 3,
  units = "in",
  dpi = 600
)

# Fitting GAMs for mean Fall isotherm temperature -------------------------------------------

### Model
modDecTempMean <- gamm(isotherm_TempMax_degC_17_days_0_degC_WaterYear_date ~ s(Year, k=20),
                       data = MohonkIceWeather,
                       method = "REML")
# summary(modDecTempMean)
###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
DecTempMeanPred <- with(MohonkIceWeather, data.frame(Year = seq(1933,
                                                                2023,
                                                                length.out = 200)))
DecTempMeanPred <- cbind(DecTempMeanPred, data.frame(predict(modDecTempMean$gam, DecTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
DecTempMeanPred <- transform(DecTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) 


m1.dsig <- signifD(DecTempMeanPred$fit,
                   d = DecTempMeanPred$deriv,
                   DecTempMeanPred$upper,
                   DecTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "Year"
m1.d <- Deriv(modDecTempMean)

m1.dci <- confint(m1.d, term = "Year")
m1.dsig <- signifD(DecTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Dec_incr<-data.frame(value_pred=unlist(m1.dsig$incr), Year=DecTempMeanPred$Year) 


#For ASLO talk
labels_IsoMax_fed<-c(70,90,110)

MohonkIceWeather %>%
  filter(Year>=1932) %>%
  ggplot(aes(x=Year, y=isotherm_TempMax_degC_17_days_0_degC_WaterYear_date))+
  geom_point()+
  geom_line(data=DecTempMeanPred, aes(x=Year, y=fit)) +
  geom_line(data=DecTempMeanPred, aes(x=Year, y=upper), linetype="dashed") +
  geom_line(data=DecTempMeanPred, aes(x=Year, y=lower), linetype="dashed") +
  geom_line(data=Dec_incr, aes(x=Year, y=value_pred), color="red", linewidth=1.5)+
  ggdark::dark_theme_bw(base_size=10)+
  scale_y_continuous(breaks=labels_IsoMax_fed,labels=c("12-Dec","01-Jan","21-Jan"),limits=c(70,120))+
  labs(y="Fall isotherm date",
       x="Year")
ggsave(
  "figures/ASLO/Fig6.FallIosthermGAMS.jpg",
  width = 3,
  height = 3,
  units = "in",
  dpi = 600
)



# Fitting GAMs for mean Spring isotherm temperature -------------------------------------------

### Model
modDecTempMean <- gamm(isotherm_TempMean_degC_29_days_4_degC_WaterYear_date ~ s(Year, k=20),
                       data = MohonkIceWeather,
                       method = "REML")
# summary(modDecTempMean)
###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
DecTempMeanPred <- with(MohonkIceWeather, data.frame(Year = seq(1933,
                                                                2023,
                                                                length.out = 200)))
DecTempMeanPred <- cbind(DecTempMeanPred, data.frame(predict(modDecTempMean$gam, DecTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
DecTempMeanPred <- transform(DecTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) 


m1.dsig <- signifD(DecTempMeanPred$fit,
                   d = DecTempMeanPred$deriv,
                   DecTempMeanPred$upper,
                   DecTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "Year"
m1.d <- Deriv(modDecTempMean)

m1.dci <- confint(m1.d, term = "Year")
m1.dsig <- signifD(DecTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Dec_incr<-data.frame(value_pred=unlist(m1.dsig$decr), Year=DecTempMeanPred$Year) 


#For ASLO talk
breaks_IsoAvg_fed<-c(170,190,210) 

MohonkIceWeather %>%
  filter(Year>=1932) %>%
  ggplot(aes(x=Year, y=isotherm_TempMean_degC_29_days_4_degC_WaterYear_date))+
  geom_point()+
  geom_line(data=DecTempMeanPred, aes(x=Year, y=fit)) +
  geom_line(data=DecTempMeanPred, aes(x=Year, y=upper), linetype="dashed") +
  geom_line(data=DecTempMeanPred, aes(x=Year, y=lower), linetype="dashed") +
  geom_line(data=Dec_incr, aes(x=Year, y=value_pred), color="red", linewidth=1.5)+
  ggdark::dark_theme_bw(base_size=10)+
  scale_y_continuous(breaks=breaks_IsoAvg_fed,labels=c("22-Mar","11-Apr","01-May"),limits=c(170,210))+
  labs(y="Spring isotherm date",
       x="Year")
ggsave(
  "figures/ASLO/Fig6.SpringIosthermGAMS.jpg",
  width = 3,
  height = 3,
  units = "in",
  dpi = 600
)



# Fitting GAMs for cumulative snow Feb-Apr -------------------------------------------

### Model
modDecTempMean <- gamm(cumSnow_FebMarApr ~ s(Year, k=20),
                       data = MohonkIceWeather,
                       method = "REML")
# summary(modDecTempMean)
###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
DecTempMeanPred <- with(MohonkIceWeather, data.frame(Year = seq(1933,
                                                                2023,
                                                                length.out = 200)))
DecTempMeanPred <- cbind(DecTempMeanPred, data.frame(predict(modDecTempMean$gam, DecTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
DecTempMeanPred <- transform(DecTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) 


m1.dsig <- signifD(DecTempMeanPred$fit,
                   d = DecTempMeanPred$deriv,
                   DecTempMeanPred$upper,
                   DecTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "Year"
m1.d <- Deriv(modDecTempMean)

m1.dci <- confint(m1.d, term = "Year")
m1.dsig <- signifD(DecTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Dec_incr<-data.frame(value_pred=unlist(m1.dsig$decr), Year=DecTempMeanPred$Year) 


#For ASLO talk
breaks_IsoAvg_fed<-c(170,190,210) 

MohonkIceWeather %>%
  filter(Year>=1932) %>%
  ggplot(aes(x=Year, y=cumSnow_FebMarApr))+
  geom_point()+
  # geom_line(data=DecTempMeanPred, aes(x=Year, y=fit)) +
  # geom_line(data=DecTempMeanPred, aes(x=Year, y=upper), linetype="dashed") +
  # geom_line(data=DecTempMeanPred, aes(x=Year, y=lower), linetype="dashed") +
  # geom_line(data=Dec_incr, aes(x=Year, y=value_pred), color="red", linewidth=1.5)+
  ggdark::dark_theme_bw(base_size=10)+
  # scale_y_continuous(breaks=breaks_IsoAvg_fed,labels=c("22-Mar","11-Apr","01-May"),limits=c(170,210))+
  labs(y="Spring isotherm date",
       x="Year")
ggsave(
  "figures/ASLO/Fig6.SpringSnowCumul.jpg",
  width = 3,
  height = 3,
  units = "in",
  dpi = 600
)
# >> Combine -----------------------------------------------------------------
theme_MS <- function () {
  theme_base(base_size=6) %+replace%
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="white", colour=NA, size=1.0),
      plot.title=element_text(face="plain",hjust=0.5),
      plot.subtitle = element_text(color="dimgrey", hjust=0, size=6),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text.y = element_text(size=6, angle=270),
      strip.text.x = element_text(size=6, angle=45),
      panel.spacing=grid::unit(0,"lines"),
      axis.ticks.length = unit(0.1, "cm")
    )
}


theme_set(theme_MS())

# Combine...
emp_df<-bind_rows(JanWx, FebWx, MarchWx, AprilWx,
                  MayWx, JuneWx, JulyWx, AugWx, 
                  SeptWx, OctWx, NovWx, DecWx)%>%
  mutate(Month=factor(Month, 
                      levels=c("Jan","Feb","March","April",
                               "May","June","July","Aug",
                               "Sept","Oct","Nov","Dec")))
sim_df<-bind_rows(JanTempMeanPred,FebTempMeanPred,MarchTempMeanPred,
                  AprilTempMeanPred,MayTempMeanPred,JuneTempMeanPred,
                  JulyTempMeanPred,AugTempMeanPred,SeptTempMeanPred,
                  OctTempMeanPred,NovTempMeanPred,DecTempMeanPred)%>%
  mutate(Month=factor(Month, 
                      levels=c("Jan","Feb","March","April",
                               "May","June","July","Aug",
                               "Sept","Oct","Nov","Dec")))
incr_df<-bind_rows(Jan_incr, Feb_incr, March_incr, April_incr,
                   May_incr, June_incr, July_incr, Aug_incr,
                   Sept_incr, Oct_incr, Nov_incr, Dec_incr)%>%
  mutate(Month=factor(Month, 
                      levels=c("Jan","Feb","March","April",
                               "May","June","July","Aug",
                               "Sept","Oct","Nov","Dec")))


library(ggh4x) #add minor breaks to x-axis

emp_df %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point(size=0.2, color="grey80", alpha=0.8)+
  geom_line(data=sim_df, aes(x=water_year, y=fit), linewidth=0.2) +
  geom_ribbon(data=sim_df,aes(ymin = (lower), ymax = (upper), x = water_year),
              alpha = 0.5, inherit.aes = FALSE, fill="black") +
  # geom_line(data=sim_df, aes(x=water_year, y=upper), linetype="dashed") +
  # geom_line(data=sim_df, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=incr_df, aes(x=water_year, y=value_pred), color="maroon", linewidth=0.2) +
  facet_wrap(~Month,ncol=12)+
  scale_x_continuous(limit = c(1932, 2022),
                     breaks = seq(1960, 2000, by = 40),
                     minor_breaks = seq(1940, 2020, 20),
                     guide = "axis_minor" # add minor ticks
  )+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        strip.text.x = element_text(margin = margin(b = -0.9)),
        plot.margin=unit(c(0,0,0,0), "lines"))+
  labs(y="Air temperature (°C)",
       x="Year")

ggsave( 
  "figures/FigS2.MonthlyTempTrends.jpg",
  width = 4,
  height = 1.6,
  units = "in",
  dpi = 600
)




# ~~ FIGURE S2.  CUMULATIVE Monthly temp.  trends  -----------------------------------



# Fitting GAMs for mean Jan temperature -------------------------------------------
JanWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="1") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=sum(TempMean_degC)) %>%
  mutate(Month="Jan")


### Model
modJanTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                       data = JanWx,
                       method = "REML")


###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
JanTempMeanPred <- with(JanWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                           max(water_year, na.rm=TRUE),
                                                           length.out = 200)))
JanTempMeanPred <- cbind(JanTempMeanPred, data.frame(predict(modJanTempMean$gam, JanTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
JanTempMeanPred <- transform(JanTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(Month="Jan")


m1.dsig <- signifD(JanTempMeanPred$fit,
                   d = JanTempMeanPred$deriv,
                   JanTempMeanPred$upper,
                   JanTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modJanTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(JanTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Jan_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=JanTempMeanPred$water_year) %>%
  left_join(.,JanTempMeanPred) %>%
  mutate(Month="Jan")

JanWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=JanTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=JanTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=JanTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Jan_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)



# Fitting GAMs for mean Feb temperature -------------------------------------------
FebWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="2") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=sum(TempMean_degC)) %>%
  mutate(Month="Feb")


### Model
modFebTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                       data = FebWx,
                       method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
FebTempMeanPred <- with(FebWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                           max(water_year, na.rm=TRUE),
                                                           length.out = 200)))
FebTempMeanPred <- cbind(FebTempMeanPred, data.frame(predict(modFebTempMean$gam, FebTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
FebTempMeanPred <- transform(FebTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(Month="Feb")


m1.dsig <- signifD(FebTempMeanPred$fit,
                   d = FebTempMeanPred$deriv,
                   FebTempMeanPred$upper,
                   FebTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modFebTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(FebTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Feb_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=FebTempMeanPred$water_year) %>%
  # left_join(.,FebTempMeanPred) %>%
  mutate(Month="Feb")

FebWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=FebTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=FebTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=FebTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Feb_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean March temperature -------------------------------------------
MarchWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="3") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=sum(TempMean_degC)) %>%
  mutate(Month="March")


### Model
modMarchTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                         data = MarchWx,
                         method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
MarchTempMeanPred <- with(MarchWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                               max(water_year, na.rm=TRUE),
                                                               length.out = 200)))
MarchTempMeanPred <- cbind(MarchTempMeanPred, data.frame(predict(modMarchTempMean$gam, MarchTempMeanPred,
                                                                 type="response",
                                                                 se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
MarchTempMeanPred <- transform(MarchTempMeanPred, upper = fit + (2 * se.fit),
                               lower = fit - (2 * se.fit)) %>%
  mutate(Month="March")


m1.dsig <- signifD(MarchTempMeanPred$fit,
                   d = MarchTempMeanPred$deriv,
                   MarchTempMeanPred$upper,
                   MarchTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modMarchTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(MarchTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

March_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=MarchTempMeanPred$water_year) %>%
  # left_join(.,MarchTempMeanPred) %>%
  mutate(Month="March")

MarchWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=MarchTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=MarchTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=MarchTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=March_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)



# Fitting GAMs for mean April temperature -------------------------------------------
AprilWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="4") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=sum(TempMean_degC)) %>%
  mutate(Month="April")


### Model
modAprilTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                         data = AprilWx,
                         method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
AprilTempMeanPred <- with(AprilWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                               max(water_year, na.rm=TRUE),
                                                               length.out = 200)))
AprilTempMeanPred <- cbind(AprilTempMeanPred, data.frame(predict(modAprilTempMean$gam, AprilTempMeanPred,
                                                                 type="response",
                                                                 se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
AprilTempMeanPred <- transform(AprilTempMeanPred, upper = fit + (2 * se.fit),
                               lower = fit - (2 * se.fit)) %>%
  mutate(Month="April")


m1.dsig <- signifD(AprilTempMeanPred$fit,
                   d = AprilTempMeanPred$deriv,
                   AprilTempMeanPred$upper,
                   AprilTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modAprilTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(AprilTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

April_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=AprilTempMeanPred$water_year) %>%
  # left_join(.,AprilTempMeanPred) %>%
  mutate(Month="April")

AprilWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=AprilTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=AprilTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=AprilTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=April_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean May temperature -------------------------------------------
MayWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="5") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=sum(TempMean_degC)) %>%
  mutate(Month="May")


### Model
modMayTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                       data = MayWx,
                       method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
MayTempMeanPred <- with(MayWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                           max(water_year, na.rm=TRUE),
                                                           length.out = 200)))
MayTempMeanPred <- cbind(MayTempMeanPred, data.frame(predict(modMayTempMean$gam, MayTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
MayTempMeanPred <- transform(MayTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(Month="May")


m1.dsig <- signifD(MayTempMeanPred$fit,
                   d = MayTempMeanPred$deriv,
                   MayTempMeanPred$upper,
                   MayTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modMayTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(MayTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

May_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=MayTempMeanPred$water_year) %>%
  # left_join(.,MayTempMeanPred) %>%
  mutate(Month="May")

MayWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=MayTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=MayTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=MayTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=May_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)


# Fitting GAMs for mean June temperature -------------------------------------------
JuneWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="6") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=sum(TempMean_degC)) %>%
  mutate(Month="June")


### Model
modJuneTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                        data = JuneWx,
                        method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
JuneTempMeanPred <- with(JuneWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                             max(water_year, na.rm=TRUE),
                                                             length.out = 200)))
JuneTempMeanPred <- cbind(JuneTempMeanPred, data.frame(predict(modJuneTempMean$gam, JuneTempMeanPred,
                                                               type="response",
                                                               se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
JuneTempMeanPred <- transform(JuneTempMeanPred, upper = fit + (2 * se.fit),
                              lower = fit - (2 * se.fit)) %>%
  mutate(Month="June")


m1.dsig <- signifD(JuneTempMeanPred$fit,
                   d = JuneTempMeanPred$deriv,
                   JuneTempMeanPred$upper,
                   JuneTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modJuneTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(JuneTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

June_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=JuneTempMeanPred$water_year) %>%
  # left_join(.,JuneTempMeanPred) %>%
  mutate(Month="June")

JuneWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=JuneTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=JuneTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=JuneTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=June_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean July temperature -------------------------------------------
JulyWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="7") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=sum(TempMean_degC)) %>%
  mutate(Month="July")


### Model
modJulyTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                        data = JulyWx,
                        method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
JulyTempMeanPred <- with(JulyWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                             max(water_year, na.rm=TRUE),
                                                             length.out = 200)))
JulyTempMeanPred <- cbind(JulyTempMeanPred, data.frame(predict(modJulyTempMean$gam, JulyTempMeanPred,
                                                               type="response",
                                                               se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
JulyTempMeanPred <- transform(JulyTempMeanPred, upper = fit + (2 * se.fit),
                              lower = fit - (2 * se.fit)) %>%
  mutate(Month="July")


m1.dsig <- signifD(JulyTempMeanPred$fit,
                   d = JulyTempMeanPred$deriv,
                   JulyTempMeanPred$upper,
                   JulyTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modJulyTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(JulyTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

July_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=JulyTempMeanPred$water_year) %>%
  # left_join(.,JulyTempMeanPred) %>%
  mutate(Month="July")

JulyWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=JulyTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=JulyTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=JulyTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=July_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean Aug temperature -------------------------------------------
AugWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="8") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=sum(TempMean_degC)) %>%
  mutate(Month="Aug")


### Model
modAugTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                       data = AugWx,
                       method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
AugTempMeanPred <- with(AugWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                           max(water_year, na.rm=TRUE),
                                                           length.out = 200)))
AugTempMeanPred <- cbind(AugTempMeanPred, data.frame(predict(modAugTempMean$gam, AugTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
AugTempMeanPred <- transform(AugTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(Month="Aug")


m1.dsig <- signifD(AugTempMeanPred$fit,
                   d = AugTempMeanPred$deriv,
                   AugTempMeanPred$upper,
                   AugTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modAugTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(AugTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Aug_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=AugTempMeanPred$water_year) %>%
  # left_join(.,AugTempMeanPred) %>%
  mutate(Month="Aug")

AugWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=AugTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=AugTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=AugTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Aug_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean Sept temperature -------------------------------------------
SeptWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="9") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=sum(TempMean_degC)) %>%
  mutate(Month="Sept")


### Model
modSeptTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                        data = SeptWx,
                        method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
SeptTempMeanPred <- with(SeptWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                             max(water_year, na.rm=TRUE),
                                                             length.out = 200)))
SeptTempMeanPred <- cbind(SeptTempMeanPred, data.frame(predict(modSeptTempMean$gam, SeptTempMeanPred,
                                                               type="response",
                                                               se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
SeptTempMeanPred <- transform(SeptTempMeanPred, upper = fit + (2 * se.fit),
                              lower = fit - (2 * se.fit)) %>%
  mutate(Month="Sept")


m1.dsig <- signifD(SeptTempMeanPred$fit,
                   d = SeptTempMeanPred$deriv,
                   SeptTempMeanPred$upper,
                   SeptTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modSeptTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(SeptTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Sept_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=SeptTempMeanPred$water_year) %>%
  # left_join(.,SeptTempMeanPred) %>%
  mutate(Month="Sept")

SeptWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=SeptTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=SeptTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=SeptTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Sept_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean Oct temperature -------------------------------------------
OctWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="10") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=sum(TempMean_degC)) %>%
  mutate(Month="Oct")


### Model
modOctTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                       data = OctWx,
                       method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
OctTempMeanPred <- with(OctWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                           max(water_year, na.rm=TRUE),
                                                           length.out = 200)))
OctTempMeanPred <- cbind(OctTempMeanPred, data.frame(predict(modOctTempMean$gam, OctTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
OctTempMeanPred <- transform(OctTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(Month="Oct")


m1.dsig <- signifD(OctTempMeanPred$fit,
                   d = OctTempMeanPred$deriv,
                   OctTempMeanPred$upper,
                   OctTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modOctTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(OctTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Oct_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=OctTempMeanPred$water_year) %>%
  # left_join(.,OctTempMeanPred) %>%
  mutate(Month="Oct")

OctWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=OctTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=OctTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=OctTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Oct_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

# Fitting GAMs for mean Nov temperature -------------------------------------------
NovWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="11") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=sum(TempMean_degC)) %>%
  mutate(Month="Nov")


### Model
modNovTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                       data = NovWx,
                       method = "REML")


###Since we're concerned with the response, include "response" in type of predict()
NovTempMeanPred <- with(NovWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                           max(water_year, na.rm=TRUE),
                                                           length.out = 200)))
NovTempMeanPred <- cbind(NovTempMeanPred, data.frame(predict(modNovTempMean$gam, NovTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
NovTempMeanPred <- transform(NovTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(Month="Nov")


m1.dsig <- signifD(NovTempMeanPred$fit,
                   d = NovTempMeanPred$deriv,
                   NovTempMeanPred$upper,
                   NovTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modNovTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(NovTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Nov_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=NovTempMeanPred$water_year) %>%
  # left_join(.,NovTempMeanPred) %>%
  mutate(Month="Nov")

NovWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=NovTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=NovTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=NovTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Nov_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

#ASLO talk
NovWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=NovTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=NovTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=NovTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Nov_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5) + 
  ggdark::dark_theme_bw(base_size=10)+
  labs(y="Cumulative mean air temperature (°C)",
       x="Year")
ggsave(
  "figures/ASLO/Fig5.NovGAMS.jpg",
  width = 3,
  height = 3,
  units = "in",
  dpi = 600
)


# Fitting GAMs for mean Dec temperature -------------------------------------------
DecWx<-MohonkDailyWeatherFull %>%
  filter(water_year >= 1932 & Month=="12") %>%
  group_by(water_year) %>%
  dplyr::summarize(TempMean_degC=sum(TempMean_degC)) %>%
  mutate(Month="Dec")


### Model
modDecTempMean <- gamm(TempMean_degC ~ s(water_year, k=20),
                       data = DecWx,
                       method = "REML")

###Since we're concerned with the response, include "response" in type of predict()
###Since we're concerned with the response, include "response" in type of predict()
DecTempMeanPred <- with(DecWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                           max(water_year, na.rm=TRUE),
                                                           length.out = 200)))
DecTempMeanPred <- cbind(DecTempMeanPred, data.frame(predict(modDecTempMean$gam, DecTempMeanPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
DecTempMeanPred <- transform(DecTempMeanPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit)) %>%
  mutate(Month="Dec")


m1.dsig <- signifD(DecTempMeanPred$fit,
                   d = DecTempMeanPred$deriv,
                   DecTempMeanPred$upper,
                   DecTempMeanPred$lower)



# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modDecTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(DecTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

Dec_incr<-data.frame(value_pred=unlist(m1.dsig$incr), water_year=DecTempMeanPred$water_year) %>%
  # left_join(.,DecTempMeanPred) %>%
  mutate(Month="Dec")

DecWx %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point()+
  geom_line(data=DecTempMeanPred, aes(x=water_year, y=fit)) +
  geom_line(data=DecTempMeanPred, aes(x=water_year, y=upper), linetype="dashed") +
  geom_line(data=DecTempMeanPred, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=Dec_incr, aes(x=water_year, y=value_pred), color="red", linewidth=1.5)

# >> Combine -----------------------------------------------------------------
theme_MS <- function () {
  theme_base(base_size=6) %+replace%
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="white", colour=NA, size=1.0),
      plot.title=element_text(face="plain",hjust=0.5),
      plot.subtitle = element_text(color="dimgrey", hjust=0, size=6),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text.y = element_text(size=6, angle=270),
      strip.text.x = element_text(size=6, angle=45),
      panel.spacing=grid::unit(0,"lines"),
      axis.ticks.length = unit(0.1, "cm")
    )
}




theme_set(theme_MS())

# Combine...
emp_df<-bind_rows(JanWx, FebWx, MarchWx, AprilWx,
                  MayWx, JuneWx, JulyWx, AugWx, 
                  SeptWx, OctWx, NovWx, DecWx)%>%
  mutate(Month=factor(Month, 
                      levels=c("Jan","Feb","March","April",
                               "May","June","July","Aug",
                               "Sept","Oct","Nov","Dec")))
sim_df<-bind_rows(JanTempMeanPred,FebTempMeanPred,MarchTempMeanPred,
                  AprilTempMeanPred,MayTempMeanPred,JuneTempMeanPred,
                  JulyTempMeanPred,AugTempMeanPred,SeptTempMeanPred,
                  OctTempMeanPred,NovTempMeanPred,DecTempMeanPred)%>%
  mutate(Month=factor(Month, 
                      levels=c("Jan","Feb","March","April",
                               "May","June","July","Aug",
                               "Sept","Oct","Nov","Dec")))
incr_df<-bind_rows(Jan_incr, Feb_incr, March_incr, April_incr,
                   May_incr, June_incr, July_incr, Aug_incr,
                   Sept_incr, Oct_incr, Nov_incr, Dec_incr)%>%
  mutate(Month=factor(Month, 
                      levels=c("Jan","Feb","March","April",
                               "May","June","July","Aug",
                               "Sept","Oct","Nov","Dec")))


library(ggh4x) #add minor breaks to x-axis

emp_df %>%
  ggplot(aes(x=water_year, y=TempMean_degC))+
  geom_point(size=0.2, color="grey80", alpha=0.8)+
  geom_line(data=sim_df, aes(x=water_year, y=fit), linewidth=0.2) +
  geom_ribbon(data=sim_df,aes(ymin = (lower), ymax = (upper), x = water_year),
              alpha = 0.5, inherit.aes = FALSE, fill="black") +
  # geom_line(data=sim_df, aes(x=water_year, y=upper), linetype="dashed") +
  # geom_line(data=sim_df, aes(x=water_year, y=lower), linetype="dashed") +
  geom_line(data=incr_df, aes(x=water_year, y=value_pred), color="red", linewidth=0.2) +
  facet_wrap(~Month,ncol=12)+
  scale_x_continuous(limit = c(1932, 2022),
                     breaks = seq(1960, 2000, by = 40),
                     minor_breaks = seq(1940, 2020, 20),
                     guide = "axis_minor" # add minor ticks
  )+
  theme(axis.text.x=element_text(angle=45, hjust=1),
        axis.ticks=element_line(linewidth=0.2),
        strip.text.x = element_text(margin = margin(b = -0.9)),
        plot.margin=unit(c(0,0,0,0), "lines"))+
  labs(y="Cumulative mean daily\nair temperature (°C)",
       x="Year")

ggsave( 
  "figures/FigS2.MonthlyTempTrends_cumulative.jpg",
  width = 4,
  height = 1.6,
  units = "in",
  dpi = 600
)


# ~~ FIGURE S5 - correlation pairs  --------------------------------------


#Look for potentially spurious correlations
res3 <- rcorr(as.matrix(MohonkIceWeather[,3:ncol(MohonkIceWeather)]),
              type="spearman")
MohonkIceWeather_trim_correlations<-flattenCorrMatrix(res3$r, res3$P)
MohonkIceWeather_trim_correlations<-MohonkIceWeather_trim_correlations%>%
  filter(row %in% c("IceOutDayofYear","IceInDayofYear_fed","LengthOfIceCover_days")) %>%
  filter(p < 0.05) 

#Take a look at what variables are highly correlated with IceOut and IceIn
MohonkIceWeather_trim_correlations %>%
  group_by(row) %>%
  filter(p <= 0.05 & abs(cor) > 0.4) %>% #Somehow every predictor has a p-value < 0.05, so I added a secondary criteria of correlation strength
  arrange(row) %>%
  filter(row=="IceInDayofYear_fed")

#Look at the top 10 strongest correlations for each predictor variable
MohonkIce_top10<-MohonkIceWeather_trim_correlations %>%
  mutate(cor_abs=abs(cor)) %>%
  filter(!grepl('nDays|Jan|LengthOfIceCover_days', column)) %>% #Exclude any nDays variables
  group_by(row) %>%
  arrange(desc(cor_abs)) %>%
  group_by(row) %>%
  slice(1:10)

#Extract variable names 
IceInVars <- MohonkIce_top10 %>% filter(row=="IceInDayofYear_fed") %>% pull(column)
IceOutVars <- MohonkIce_top10 %>% filter(row=="IceOutDayofYear") %>% pull(column)
IceDurationVars <- MohonkIce_top10 %>% filter(row=="LengthOfIceCover_days") %>% pull(column)

# Graph the top 10 pairwise correlations for Ice-On and Ice-Off


#Make bar plot
jpeg(filename = 'figures/MS/FigureS5.top10_correlations.jpg',
     width = 6, height = 7, units = 'in', res = 300)

MohonkIce_top10 %>%
  filter(row %in% c("IceInDayofYear_fed","IceOutDayofYear")) %>%
  mutate(row =
           ifelse(row=="IceInDayofYear_fed","Ice-on",
                  ifelse(row=="IceOutDayofYear", "Ice-off",""))) %>%
  select(row, column, cor_abs) %>%
  mutate(column=
           ifelse(column=="cumMeanDailyT_OctNovDec", "cumul. mean daily temp. Oct-Dec",
                  ifelse(column=="cumMeanDailyT_Dec","cumul. mean daily temp. Dec",
                         ifelse(column=="cumMeanDailyT_Nov", "cumul. mean daily temp. Nov",
                                ifelse(column=="cumMeanDailyT_SepOctNov", "cumul. mean daily temp. Sept-Nov",
                                       ifelse(column=="cumMeanDailyT_OctNov", "cumul. mean daily temp. Oct-Nov",
                                              ifelse(column=="NAO_index_Nov", "NAO Nov.",
                                                     ifelse(column=="percPrecipRain_Nov", "% rain Nov.",
                                                            ifelse(column=="cumSnow_OctNovDec", "cumul. snow Oct-Dec.",
                                                                   ifelse(column=="GlobalTempanomaly_C", "global mean daily temp. anom.",
                                                                          ifelse(column=="cumSnow_Nov", "cumul. snow Nov.",
                                                                                 ifelse(column=="cumMeanDailyT_FebMar", "cumul. mean daily temp. Feb-Mar.",
                                                                                        ifelse(column=="cumMeanDailyT_FebMarApr", "cumul. mean daily temp. Feb-Apr.",
                                                                                               ifelse(column=="cumMeanDailyT_Mar", "cumul. mean daily temp. Mar.",
                                                                                                      ifelse(column=="cumMeanDailyT_MarApr", "cumul. mean daily temp. Mar-Apr.",
                                                                                                             ifelse(column=="cumSnow_FebMarApr", "cumul. snow Feb-Apr.",
                                                                                                                    ifelse(column=="cumSnow_FebMar", "cumul. snow Feb-Mar.",
                                                                                                                           ifelse(column=="maxSnowDepth_mm", "max. winter snow depth",
                                                                                                                                  ifelse(column=="cumSnow_MarApr", "cumul. snow Mar-Apr",
                                                                                                                                         ifelse(column=="cumSnow_Mar", "cumul. snow Mar.",
                                                                                                                                                ifelse(column=="cumMeanDailyT_Feb", "cumul. mean daily temp. Feb",
                                                                                                                                                       ifelse(column=="isotherm_TempMax_degC_17_days_0_degC_WaterYear_date", "Fall isotherm date",
                                                                                                                                                              ifelse(column=="isotherm_TempMean_degC_29_days_4_degC_WaterYear_date", "Spring isotherm date",""))))))))))))))))))))))) %>%
  mutate(name=paste(row, "-", column)) %>%
  ggplot(aes(x=cor_abs,y=fct_reorder(column, cor_abs, .desc = FALSE), fill=cor_abs))+
  geom_bar(stat="identity", width=0.75, color="black")+
  scale_fill_gradient2(low = "white", mid="darkolivegreen2", high = "darkseagreen4", midpoint=0.5, na.value = NA) +
  geom_text(aes(label=round(cor_abs,2)),color="black",size=3,position=position_stack(vjust=0.5)) +
  labs(y="Pairs",
       x="Correlation coefficient")+
  scale_x_continuous(limits = c(0, 1, breaks = seq(0, 1, by = 0.25)))+
  facet_wrap(~row, scales="free", nrow=2)+
  theme(legend.position="none")

dev.off()



#Export table of top 10 correlations
# write_csv(MohonkIce_top10, "data/exported/MohonkIce_CorrMatrix.csv")


#Just out of curiosity, is there any relationship between days since turnover and IceInDOY?
AnnualData %>%
  mutate(turnoverToIceIn_days=IceInDayofYear-EndOfStratification_Day) %>%
  select(Year,turnoverToIceIn_days,IceInDayofYear) %>%
  ggplot(aes(y=turnoverToIceIn_days,x=Year))+
  geom_point(size=3, shape=21)+
  geom_line(size=0.5)+
  xlab("Year")+
  geom_smooth(method="lm",color="grey50", size=0.5)






