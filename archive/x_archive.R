#Snippets of old code that might be helpful at some point, but were deleted in the code clean up process
# IAO 2024-10-29


# 02_IceTrends.R murdered darlings ----------------------------------------

### Ice cover percentage since 2012 where data coverage is highest

IceCover_perc %>%
  left_join(., MohonkIce_vis, by=c("water_year")) %>%
  filter(water_year>2011) %>%
  ggplot(aes(x=Date, y=IceCover_Percent))+
  geom_point()+
  facet_wrap(~water_year, scales="free_x")+
  geom_line(aes(x=ICEIN_1), color="blue") +
  geom_line(aes(x=ICEIN_2), color="blue") +
  geom_line(aes(x=ICEIN_3), color="blue") +
  geom_line(aes(x=ICEOUT_1), color="red") +
  geom_line(aes(x=ICEOUT_2), color="red") +
  geom_line(aes(x=ICEOUT_3), color="red") +
  scale_x_date(date_labels = "%m/%d",
               breaks = "6 weeks")
ggsave(
  "figures/MS/FigX.PercIceCover.jpg",
  width = 10,
  height = 6,
  units = "in",
  dpi = 600
)


## Here I am looking at the massive list of potential predictors of ice-in or ice-out and seeing which ones are highly correlated (>0.7)

# res2 <- rcorr(as.matrix(MohonkIceWeather[,7:ncol(MohonkIceWeather)]),
#               type="spearman")
# res2
#
#
# #Create dataframe of pearson r and p-values
# MohonkIceWeather_correlations<-flattenCorrMatrix(res2$r, res2$P)
#
# #In an effort to limit the number of covariates, here I am looking at all correlations > 0.7
# #and then below making a choice on which of the pair I should keep.
# MohonkIceWeather_correlations_trim <- MohonkIceWeather_correlations %>%
#   filter(abs(cor) > 0.7) %>%
#   arrange(desc(cor))
#
# head(MohonkIceWeather_correlations_trim)

#Visualize correlations with IceInDayofYear_fed
# MohonkIceWeather %>%
#   select(IceInDayofYear_fed, all_of(IceInVars)) %>%
#   ggpairs() 
# median(MohonkIceWeather$IceInDayofYear,na.rm=T)

# December 14th median ice-in DOY
# cumMeanDailyT_OctNovDec & nDaysMinBelowZero_OctNovDec -0.79 Keep the former since the latter had *slightly* weaker corr. w/ ice-in
# cumMeanDailyT_OctNovDec & cumMeanDailyT_Dec r=0.77. Keep the former since the latter had *slightly* weaker corr. w/ ice-in
# cumMeanDailyT_OctNovDec & nDaysMinBelowZero_Dec r=-0.63. Again, keep the former since the latter had *slightly* weaker corr. w/ ice-in
# cumMeanDailyT_OctNovDec & nDaysMeanBelowZero_Dec r=-0.67. Keep the former since the latter had *slightly* weaker corr. w/ ice-in
# cumMeanDailyT_OctNovDec & cumMeanDailyT_Nov r=0.67.
# cumMeanDailyT_OctNovDec & nDaysMinBelowZero_Nov r=-0.53.
# percPrecipRain_Jan ranks in the top 10 correlations with IceIn but considering that IceIn almost always happens before January, we should drop.
# cumMeanDailyT_OctNovDec & cumMeanDailyT_OctNov r=0.79. These two are highly correlated but corr between IceIn & cumMeanDailyT_OctNov is only 0.40.
# cumMeanDailyT_OctNovDec & nDaysMeanBelowZero_Nov r=-0.54. Keeping cumMeanDailyT_OctNovDec since it is much more highly correlated with IceIn.
## This basically only leaves us with one predictor variable...
## What about keeping nDaysMinBelowZero_Nov too since it isn't too strongly colinear with cumMeanDailyT_OctNovDec?

# MohonkIceWeather %>%
#   select(IceInDayofYear_fed, cumMeanDailyT_OctNovDec, nDaysMinBelowZero_Nov) %>%
#   ggpairs() 

#Alternatively, look at a matrix as above...
IceInVars_df<-MohonkIceWeather %>%
  select(all_of(IceInVars))
IceInCorrMat <- rcorr(as.matrix(IceInVars_df[,1:ncol(IceInVars_df)]))
#Create dataframe of pearson r and p-values
IceInDOY_corrMat<-flattenCorrMatrix(IceInCorrMat$r, IceInCorrMat$P) %>%
  filter(abs(cor)>=0.7) %>%
  arrange(row) %>%
  mutate(y="IceInDayofYear_fed") %>%
  relocate(y, .before = row) 
# write_csv(IceInDOY_corrMat, "data/exported/IceInDOY_CollinearMatrix.csv")

#Visualize correlations with IceOutDayofYear
MohonkIceWeather %>%
  select(IceOutDayofYear, all_of(IceOutVars)) %>%
  ggpairs() 
median(MohonkIceWeather$IceOutDayofYear,na.rm=T)
# cumMeanDailyT_MarApr & cumMeanDailyT_Mar r=0.82. The median ice-off DOY is April 7th so I could see making the case for including April temps.
# IceOutDayofYear & LengthOfIceCover_days r=0.72. I could also see making the case for including length of ice cover as a predictor because if the ice has been on the lake longer, it is also thicker.
# cumMeanDailyT_MarApr & nDaysMeanAboveZero_FebMar r=0.60  Keep the former since the latter had *slightly* weaker corr. w/ ice-out
# cumMeanDailyT_MarApr & nDaysMinAboveZero_Mar r=0.48 Keep both, since they aren't strong collinear
# cumMeanDailyT_MarApr & nDaysMinBelowZero_Mar r=-0.48 Drop the latter, since keeping nDaysMinBelowZero_Mar
# cumMeanDailyT_MarApr & nDaysMeanBelowZero_Mar r=-0.78 Strongly collinear, keep cumMeanDailyT_MarApr
# cumMeanDailyT_MarApr & nDaysMeanAboveZero_Mar r=0.78 Strongly collinear, keep cumMeanDailyT_MarApr
# cumMeanDailyT_MarApr & cumSnow_FebMar r=-0.43 Inclined to keep both. 
# cumMeanDailyT_MarApr & maxSnowDepth_mm r=-0.27 Inclined to keep both. 
# maxSnowDepth_mm & cumSnow_FebMar r=0.69. Strong collinear, maybe most of the snow tends to fall in Feb + March? cumSnow_FebMar more strongly correlated with IceOutDOY
MohonkIceWeather %>%
  select(IceOutDayofYear, cumMeanDailyT_MarApr, nDaysMinAboveZero_Mar, cumSnow_FebMar, maxSnowDepth_mm) %>%
  ggpairs() 
#We have less data for maxSnowDepth_mm than cumSnow_FebMar, so let's keep drop maxSnowDepth_mm.


#Visualize correlations with IceOutDayofYear
# MohonkIceWeather %>%
#   select(LengthOfIceCover_days, IceOutDayofYear, all_of(IceDurationVars)) %>%
#   ggpairs() 
median(MohonkIceWeather$LengthOfIceCover_days,na.rm=T) #100 days

#Alternatively, look at a matrix as above...
IceOutVars_df<-MohonkIceWeather %>%
  select(all_of(IceOutVars))
IceOutCorrMat <- rcorr(as.matrix(IceOutVars_df[,1:ncol(IceOutVars_df)]))
#Create dataframe of pearson r and p-values
IceOutDOY_corrMat<-flattenCorrMatrix(IceOutCorrMat$r, IceOutCorrMat$P) %>%
  filter(abs(cor)>=0.7) %>%
  arrange(row) %>%
  mutate(y="IceOutDayofYear") %>%
  relocate(y, .before = row) 
IceOutDOY_corrMat
# write_csv(IceOutDOY_corrMat, "data/exported/IceOutDOY_CollinearMatrix.csv")


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

# >> Trends in the ice phenology drivers ----------------------------------

# Question - is the maximum observed air temperature 17 days after the
# autumn 0 °C air temperature isotherm was crossed changing over time?

# What dates does the maximum observed air temperature 17 days after 0 °C air temperature isotherm was crossed
# correspond to?
as.Date(min(round(Isotherm_WaterYear_dates_IceIn$isotherm_TempMax_degC_17_days_0_degC_WaterYear_date,0),na.rm=TRUE)+274, origin="2014-01-02")
as.Date(max(round(Isotherm_WaterYear_dates_IceIn$isotherm_TempMax_degC_17_days_0_degC_WaterYear_date,0),na.rm=TRUE)+274, origin="2014-01-02")



#Distribution of y
hist(Isotherm_WaterYear_dates_IceIn$isotherm_TempMax_degC_17_days_0_degC_WaterYear_date)

### I added Family Gamma here for how errors should respond
mod0_iceOnIsotherm <- gam(isotherm_TempMax_degC_17_days_0_degC_WaterYear_date ~ s(WaterYear),
                          # family=Gamma(link="log"),
                          data = Isotherm_WaterYear_dates_IceIn,
                          correlation = corCAR1(form = ~ WaterYear),
                          method = "REML")
summary(mod0_iceOnIsotherm)

#PLOT Autocorrelation function of residuals from the additive model with AR(1) errors
ACF <- acf(resid(mod0_iceOnIsotherm, type = "response"), plot = FALSE)
ACF <- setNames(data.frame(unclass(ACF)[c("acf", "lag")]), c("ACF","Lag"))
ggplot(ACF, aes(x = Lag, y = ACF)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = Lag, yend = 0))
#Suggests that an AR(1) model isn't necessary


###Since we're concerned with the response, include "response" in type of predict()
iceOnIsothermPred <- with(Isotherm_WaterYear_dates_IceIn, data.frame(WaterYear = seq(min(WaterYear, na.rm=TRUE),
                                                                                     max(WaterYear, na.rm=TRUE),
                                                                                     length.out = 200)))
iceOnIsothermPred <- cbind(iceOnIsothermPred, data.frame(predict(mod0_iceOnIsotherm, iceOnIsothermPred,
                                                                 type="response",
                                                                 se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
iceOnIsothermPred <- transform(iceOnIsothermPred, upper = fit + (2 * se.fit),
                               lower = fit - (2 * se.fit))


# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "WaterYear"
m1.d <- Deriv(mod0_iceOnIsotherm)

m1.dci <- confint(m1.d, term = "WaterYear")
m1.dsig <- signifD(iceOnIsothermPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

ylim <- with(iceOnIsothermPred, range(upper, lower, fit))
ylab <- 'DOY max. temp 17 days after 0 isotherm is crossed'

plot(fit ~ WaterYear, data = iceOnIsothermPred, type = "n", ylab = ylab, ylim = ylim)
lines(fit ~ WaterYear, data = iceOnIsothermPred)
lines(upper ~ WaterYear, data = iceOnIsothermPred, lty = "dashed")
lines(lower ~ WaterYear, data = iceOnIsothermPred, lty = "dashed")
lines(unlist(m1.dsig$incr) ~ WaterYear, data = iceOnIsothermPred, col = "blue", lwd = 3)
lines(unlist(m1.dsig$decr) ~ WaterYear, data = iceOnIsothermPred, col = "red", lwd = 3)
#The date is getting later

plot.Deriv(m1.d)
#but first derivative plot shows that it is not accelerating at least

#Plot 'DOY max. temp 17 days after 0 isotherm is crossed' vs. water-year pretty
ggplot(iceOnIsothermPred,aes(x=WaterYear,y=fit))+
  geom_point(data=Isotherm_WaterYear_dates_IceIn,
             mapping=aes(x=WaterYear, y=isotherm_TempMax_degC_17_days_0_degC_WaterYear_date), size=2.5, alpha=0.7) +
  geom_line(linewidth=1)+
  geom_ribbon(aes(ymin = (lower), ymax = (upper), x = WaterYear), alpha = 0.5, inherit.aes = FALSE) +
  labs(x="Year",y="DOY max. temp 17 days after 0 isotherm is crossed")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))
# geom_text(x = 1945, y = 120,
#           aes(label = paste("Deviance explained",round(mod_sum$dev.expl,1)*100,"%")),
#           color="black",size=3)

mod_sum<-summary(mod0_iceOnIsotherm)

#How does this compare to sens slope?
Isotherm_WaterYear_dates_IceIn %>%
  select(WaterYear, isotherm_TempMax_degC_17_days_0_degC_WaterYear_date) %>%
  pivot_longer(-1) %>%
  group_by(name)%>%
  dplyr::summarize(Sens_Slope=MTCC.sensSlope(x=WaterYear,y=value)$coefficients["Year"],
                   Sens_Intercept=MTCC.sensSlope(x=WaterYear,y=value)$coefficients["Intercept"],
                   Sens_pval=MTCC.sensSlope(x=WaterYear,y=value)$pval,
                   Sens_z_stat=MTCC.sensSlope(x=WaterYear,y=value)$z_stat,
                   Sens_n=MTCC.sensSlope(x=WaterYear,y=value)$n) %>%
  mutate(Significance=ifelse(Sens_pval<0.05,"*","NS"))
#Moving back 2 days a decade or 19 days later than it was at the beginning of the record. 

#What about isotherm_TempMax_degC_1_days_0_degC_WaterYear_date?
#How does this compare to sens slope?
Isotherm_WaterYear_dates_IceIn %>%
  select(WaterYear, isotherm_TempMax_degC_1_days_0_degC_WaterYear_date) %>%
  pivot_longer(-1) %>%
  group_by(name)%>%
  dplyr::summarize(Sens_Slope=MTCC.sensSlope(x=WaterYear,y=value)$coefficients["Year"],
                   Sens_Intercept=MTCC.sensSlope(x=WaterYear,y=value)$coefficients["Intercept"],
                   Sens_pval=MTCC.sensSlope(x=WaterYear,y=value)$pval,
                   Sens_z_stat=MTCC.sensSlope(x=WaterYear,y=value)$z_stat,
                   Sens_n=MTCC.sensSlope(x=WaterYear,y=value)$n) %>%
  mutate(Significance=ifelse(Sens_pval<0.05,"*","NS"))




#How about trends in isotherm_TempMean_degC_29_days_4_degC_WaterYear_date?
Isotherm_WaterYear_dates_IceOut %>%
  select(WaterYear, isotherm_TempMean_degC_29_days_4_degC_WaterYear_date) %>%
  filter(WaterYear>1931) %>%
  pivot_longer(-1) %>%
  group_by(name)%>%
  dplyr::summarize(Sens_Slope=MTCC.sensSlope(x=WaterYear,y=value)$coefficients["Year"],
                   Sens_Intercept=MTCC.sensSlope(x=WaterYear,y=value)$coefficients["Intercept"],
                   Sens_pval=MTCC.sensSlope(x=WaterYear,y=value)$pval,
                   Sens_z_stat=MTCC.sensSlope(x=WaterYear,y=value)$z_stat,
                   Sens_n=MTCC.sensSlope(x=WaterYear,y=value)$n) %>%
  mutate(Significance=ifelse(Sens_pval<0.05,"*","NS"))




# Question - is the average observed air temperature 29 days after the
# spring 4℃ air temperature isotherm changing over time?

# What dates does the average observed air temperature 29 days after the spring 4℃ air temperature isotherm was crossed 
# correspond to? 
as.Date(min(round(Isotherm_WaterYear_dates_IceOut$isotherm_TempMean_degC_29_days_4_degC_WaterYear_date,0),na.rm=TRUE)-92, origin="2014-01-02")
as.Date(max(round(Isotherm_WaterYear_dates_IceOut$isotherm_TempMean_degC_29_days_4_degC_WaterYear_date,0),na.rm=TRUE)-92, origin="2014-01-02")


#Distribution of y
hist(Isotherm_WaterYear_dates_IceOut$isotherm_TempMean_degC_29_days_4_degC_WaterYear_date)

### I added Family Gamma here for how errors should respond
mod0_iceOffIsotherm <- gam(isotherm_TempMean_degC_29_days_4_degC_WaterYear_date ~ s(WaterYear),
                           # family=Gamma(link="log"),
                           data = Isotherm_WaterYear_dates_IceOut,
                           correlation = corCAR1(form = ~ WaterYear),
                           method = "REML")
summary(mod0_iceOffIsotherm)

#PLOT Autocorrelation function of residuals from the additive model with AR(1) errors
ACF <- acf(resid(mod0_iceOffIsotherm, type = "response"), plot = FALSE)
ACF <- setNames(data.frame(unclass(ACF)[c("acf", "lag")]), c("ACF","Lag"))
ggplot(ACF, aes(x = Lag, y = ACF)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = Lag, yend = 0))
#Suggests that an AR(1) model isn't necessary


###Since we're concerned with the response, include "response" in type of predict()
iceOffIsothermPred <- with(Isotherm_WaterYear_dates_IceOut, data.frame(WaterYear = seq(min(WaterYear, na.rm=TRUE),
                                                                                       max(WaterYear, na.rm=TRUE),
                                                                                       length.out = 200)))
iceOffIsothermPred <- cbind(iceOffIsothermPred, data.frame(predict(mod0_iceOffIsotherm, iceOffIsothermPred,
                                                                   type="response",
                                                                   se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
iceOffIsothermPred <- transform(iceOffIsothermPred, upper = fit + (2 * se.fit),
                                lower = fit - (2 * se.fit))


# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "WaterYear"
m1.d <- Deriv(mod0_iceOffIsotherm)

m1.dci <- confint(m1.d, term = "WaterYear")
m1.dsig <- signifD(iceOffIsothermPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

ylim <- with(iceOffIsothermPred, range(upper, lower, fit))
ylab <- 'DOY avg. temp 29 days after 4C isotherm is crossed'

plot(fit ~ WaterYear, data = iceOffIsothermPred, type = "n", ylab = ylab, ylim = ylim)
lines(fit ~ WaterYear, data = iceOffIsothermPred)
lines(upper ~ WaterYear, data = iceOffIsothermPred, lty = "dashed")
lines(lower ~ WaterYear, data = iceOffIsothermPred, lty = "dashed")
lines(unlist(m1.dsig$incr) ~ WaterYear, data = iceOffIsothermPred, col = "blue", lwd = 3)
lines(unlist(m1.dsig$decr) ~ WaterYear, data = iceOffIsothermPred, col = "red", lwd = 3)
#The date is getting earlier,  with an accelerating trend 1973 to 2012 or so. 

plot.Deriv(m1.d)

#Plot 'DOY avg. temp 29 days after 4 isotherm is crossed' vs. water-year pretty
ggplot(iceOffIsothermPred,aes(x=WaterYear,y=fit))+
  geom_point(data=Isotherm_WaterYear_dates_IceOut,
             mapping=aes(x=WaterYear, y=isotherm_TempMean_degC_29_days_4_degC_WaterYear_date), size=2.5, alpha=0.7) +
  geom_line(size=1)+
  geom_ribbon(aes(ymin = (lower), ymax = (upper), x = WaterYear), alpha = 0.5, inherit.aes = FALSE) +
  labs(x="Year",y="DOY avg. temp 29 days after 4 isotherm is crossed")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))
# geom_text(x = 1945, y = 120,
#           aes(label = paste("Deviance explained",round(mod_sum$dev.expl,1)*100,"%")),
#           color="black",size=3)

mod_sum<-summary(mod0_iceOffIsotherm)
mod_sum

#How about trends in isotherm_TempMean_degC_29_days_4_degC_WaterYear_date?
Isotherm_WaterYear_dates_IceOut %>%
  select(WaterYear, isotherm_TempMean_degC_29_days_4_degC_WaterYear_date) %>%
  filter(WaterYear>1931) %>%
  pivot_longer(-1) %>%
  group_by(name)%>%
  dplyr::summarize(Sens_Slope=MTCC.sensSlope(x=WaterYear,y=value)$coefficients["Year"],
                   Sens_Intercept=MTCC.sensSlope(x=WaterYear,y=value)$coefficients["Intercept"],
                   Sens_pval=MTCC.sensSlope(x=WaterYear,y=value)$pval,
                   Sens_z_stat=MTCC.sensSlope(x=WaterYear,y=value)$z_stat,
                   Sens_n=MTCC.sensSlope(x=WaterYear,y=value)$n) %>%
  mutate(Significance=ifelse(Sens_pval<0.05,"*","NS"))


#Original plot-- script works but now we want to explicitly show the years with intermittant ice cover

# jpeg(filename = 'figures/MS/Fig1.IcePhenology_withDates.jpg',
#     width = 3.14,
#     height = 2.5,
#     res = 600,
#     units = 'in')
# 
# ggplot() +
#   geom_segment(
#     data = MohonkIce.Predicted.test,
#     aes(
#       x = Year,
#       xend = Year,
#       y = IceInDate_newdate,
#       yend = IceOutDate_newdate,
#       col = LengthOfIceCover_days
#     )
#   ) +
#   # col="grey")+
#   geom_point(
#     data = MohonkIce.Predicted.test,
#     aes(x = Year, y = IceInDate_newdate, fill = LengthOfIceCover_days),
#     shape = 21,
#     color = "black",
#     size = 1
#   ) +
#   geom_smooth(
#     data = MohonkIce.Predicted.test,
#     aes(x = Year, y = IceInDayofYear_yhat_newdate),
#     color = "black",
#     lty = 1,
#     size= 0.5
#   ) +
#   geom_point(
#     data = MohonkIce.Predicted.test,
#     aes(x = Year,
#         y = IceOutDate_newdate,
#         fill = LengthOfIceCover_days),
#     shape = 21,
#     color = "black",
#     size = 1
#   ) +
#   
#   grafify::scale_fill_grafify(palette = "blue_conti", name = "Ice duration\n(days)")+ #yellow_conti scheme
#   grafify::scale_color_grafify(palette = "blue_conti", name = "Ice duration\n(days)")+ #yellow_conti scheme
#   # scale_color_continuous(high = "cyan", low = "red",
#   #                        name = "Ice duration\n(days)") +
#   # scale_fill_continuous(high = "cyan", low = "red",
#   #                       name = "Ice duration\n(days)") +
#   scale_x_continuous(limit = c(1932, 2022),
#                      breaks = seq(1940, 2020, by = 20)) +
#   scale_y_date(date_breaks = "45 days", date_minor_breaks = "15 days",
#                date_labels = "%d-%b")+
#   theme_MS() +
#   theme(
#     # panel.grid.major.y = element_line(color="grey90", size=0.5),
#     # panel.grid.major.x = element_line(color="grey90", size=0.5),
#     # panel.grid.minor.y = element_line(color="grey90", linetype="dashed", size=0.5),
#     #GET RID OF THESE-- a pesky LOL reviewer wanted these
#     axis.line = element_line(colour = "black"),
#     panel.border = element_rect(
#       fill = NA,
#       colour = "black",
#       size = 1
#     ),
#     plot.margin=unit(c(1,0,0,0), "lines"),
#     axis.text.x = element_text(color = "black"),
#     axis.text.y = element_text(color = "black", angle=90, hjust=0.5),
#     axis.ticks = element_line(color = "black")
#   ) +
#   xlab("Year") +
#   ylab("Ice phenology date")
# 
# dev.off()
# 
# ggsave(
#   "figures/MS/Fig1.IcePhenology_withDates_inches.jpg",
#   width = 3.14,
#   height = 2.5,
#   units = "in",
#   dpi = 600
# )

# 03-VariabilityRollingWindow.R -------------------------------------------


#Rolling CV using zoo package####
#Code snagged from Gavin in this response:
#https://stackoverflow.com/questions/13195442/moving-variance-in-r


##Ice Duration
iceDuration_days_withNAs<-MohonkIce %>%
  select(LengthOfIceCover_days,Year) 


temp.sensSlope<-MTCC.sensSlope(x=iceDuration_days_withNAs$Year,y=iceDuration_days_withNAs$LengthOfIceCover_days)

iceDuration_days_withNAs%>%mutate(sensSlope_fit=as.numeric(temp.sensSlope$coefficients["Intercept"])+as.numeric(temp.sensSlope$coefficients["Year"])*Year)
ggplot(data=iceDuration_days_withNAs,aes(y=LengthOfIceCover_days,x=Year))+geom_point()+
  geom_abline(intercept=as.numeric(temp.sensSlope$coefficients["Intercept"]),slope=as.numeric(temp.sensSlope$coefficients["Year"]))

plot(temp.sensSlope$residuals~iceDuration_days_withNAs$Year)

#Rolling window calculations of CV or standard deviation####
RW_length<-15 #set the rolling window length globally (in years)

#Calculate rolling window and store year as well as median of that same window####
iceDuration_variability<-
  tibble(year_median=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(Year), width =RW_length, FUN = median)),
         LengthOfIceCover_days_sd=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(LengthOfIceCover_days), width =RW_length, FUN = function(x, na.rm = TRUE)  {sd(x, na.rm = na.rm)})), #rolling window standard deviation of length RW_length
         LengthOfIceCover_days_cv=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(LengthOfIceCover_days), width =RW_length, FUN = cv)), #rolling window coefficient of variation of length RW_length
         RollingWindow_years=RW_length) #Record the Rolling window length in a column

#Plot the variability####
ggplot(data=iceDuration_variability,
       aes(x=year_median,y=LengthOfIceCover_days_cv))+
  geom_point()+
  xlab(bquote(Year~median))+
  ylab("Ice duration (c.v.)")+
  geom_smooth(method="gam", color="black", size=0.5)

#Plot the sd####
ggplot(data=iceDuration_variability,
       aes(x=year_median,y=LengthOfIceCover_days_sd))+
  geom_point()+
  xlab(bquote(Year~median))+
  ylab("Ice duration (s.d.)")+
  geom_smooth(method="gam", color="black", size=0.5)
#Effectively the same pattern. 

#Create dataframe all rolling windows####
datalist=list()  #initialize empty list for storing a bunch of ice duration variabilities of different lengths

#*Loop through all rolling window sizes from 3 to 30 years####
for(RW_length_i in 3:30){
  iceDuration_temporary<-
    tibble(year_median=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(Year), width =RW_length_i, FUN = median)),
           LengthOfIceCover_days_sd=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(LengthOfIceCover_days), width =RW_length_i, FUN = function(x, na.rm = TRUE)  {sd(x, na.rm = na.rm)})), #rolling window standard deviation of length RW_length
           LengthOfIceCover_days_cv=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(LengthOfIceCover_days), width =RW_length_i, FUN = cv)), #rolling window coefficient of variation of length RW_length
           LengthOfIceCover_days_mean=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(LengthOfIceCover_days), width =RW_length_i, FUN = function(x, na.rm = TRUE)  {mean(x, na.rm = na.rm)})), #rolling window mean of length RW_length
           LengthOfIceCover_days_n=as.vector(rollapply(iceDuration_days_withNAs%>%dplyr::select(LengthOfIceCover_days), width =RW_length_i, FUN = function(x, na.rm = TRUE)  {sum(!is.na(x))})), #rolling window count the number of nonNA measurements that went into length RW_length
           RollingWindow_years=RW_length_i) #Record the Rolling window length in a column
  #Figure out proportion of days used for rolling stats
  iceDuration_temporary<-iceDuration_temporary%>%mutate(proportion_used=LengthOfIceCover_days_n/RollingWindow_years)%>%
    mutate(LengthOfIceCover_days_sd=ifelse(proportion_used>=0.75,LengthOfIceCover_days_sd,NA),
           LengthOfIceCover_days_cv=ifelse(proportion_used>=0.75,LengthOfIceCover_days_cv,NA),
           LengthOfIceCover_days_mean=ifelse(proportion_used>=0.75,LengthOfIceCover_days_mean,NA))
  #Calculate slope
  sensSlope<-MTCC.sensSlope(iceDuration_temporary$year_median,iceDuration_temporary$LengthOfIceCover_days_sd)
  sensSlope$coefficients
  #Store the residuals and other sens slope fit stats in iceDuration_temporary  
  iceDuration_temporary<-iceDuration_temporary%>%
    mutate(sensSlope_fit=year_median*sensSlope$coefficients["Year"]+sensSlope$coefficients["Intercept"],
           sensSlope_slope=sensSlope$coefficients["Year"],
           sensSlope_intercept=sensSlope$coefficients["Intercept"],
           sensSlope_residuals=sensSlope$residuals,
           sensSlope_pval=sensSlope$pval,
           sensSlope_z_stat=sensSlope$z_stat,
           sensSlope_n=sensSlope$n)
  #Export each Rollingwindow length to the datalist  
  datalist[[RW_length_i]]<-iceDuration_temporary  #Store the temporary data frame in a list
  
  #Fit auto.arima with xreg as year
  #auto.arima(iceDuration_temporary$LengthOfIceCover_days_sd,
  #xreg=c(iceDuration_temporary$year_median),
  # xreg=cbind(AnnualData$GlobalTempAnomoly_C,
  # AnnualData$Year),
  #seasonal=FALSE,allowdrift = FALSE,
  #stationary=TRUE)
}   

#*compile them all in one data frame####    
iceDuration_variability_all<-do.call(bind_rows,datalist) 

#*Rolling sd vs. year facet wrapped by all rolling window sizes####
ggplot(data=iceDuration_variability_all,
       aes(x=year_median,y=LengthOfIceCover_days_sd))+
  geom_point()+
  geom_line(aes(y=sensSlope_fit),size=1.5,col="blue")+
  xlab(bquote(Year~median))+
  ylab("Ice duration (s.d.)")+
  geom_smooth(method="gam", color="black", size=0.5)+
  facet_wrap(~RollingWindow_years) #, scales="free_y"

#*Rolling sd residuals from sens slopes vs. year facet wrapped by all rolling window sizes####
ggplot(data=iceDuration_variability_all,
       aes(x=year_median,y=sensSlope_residuals))+
  geom_point()+
  xlab(bquote(Year~median))+
  ylab("Ice duration (s.d.)")+
  geom_smooth(method="gam", color="black", size=0.5)+
  facet_wrap(~RollingWindow_years) #, scales="free_y"

#*Rolling cv vs. year facet wrapped by all rolling window sizes####
ggplot(data=iceDuration_variability_all,
       aes(x=year_median,y=LengthOfIceCover_days_cv))+
  geom_point()+
  xlab(bquote(Year~median))+
  ylab("Ice duration (c.v.)")+
  geom_smooth(method="gam", color="black", size=0.5)+
  facet_wrap(~RollingWindow_years)

#*Rolling mean vs. year facet wrapped by all rolling window sizes####
ggplot(data=iceDuration_variability_all,
       aes(x=year_median,y=LengthOfIceCover_days_mean))+
  geom_point()+
  xlab(bquote(Year~median))+
  ylab("Ice duration (mean)")+
  geom_smooth(method="gam", color="black", size=0.5)+
  facet_wrap(~RollingWindow_years)

#Find periodicity in each plot#####
#https://stackoverflow.com/questions/17788859/acf-plot-with-ggplot2-setting-width-of-geom-bar
#probably have to loop through each one rolling window size, create acf data frame and plot
#Or create acf data frame as in here: https://stackoverflow.com/questions/44697596/using-ggplots-facet-wrap-with-autocorrelation-plot  
library(purrr)

#Calculate the acf for various lags for each one
df_acf <- iceDuration_variability_all %>% 
  group_by(RollingWindow_years) %>% 
  summarise(list_acf=list(acf(sensSlope_residuals, plot=FALSE,na.action = na.pass))) %>%
  mutate(acf_vals=purrr::map(list_acf, ~as.numeric(.x$acf))) %>% 
  select(-list_acf) %>% 
  unnest(cols=c(acf_vals)) %>% 
  group_by(RollingWindow_years) %>% 
  mutate(lag=row_number() - 1)

#Calculate confidence intervals for significance
df_ci <- iceDuration_variability_all %>% 
  group_by(RollingWindow_years) %>% 
  summarise(ci = qnorm((1 + 0.95)/2)/sqrt(n()))

#Merge the CI back in with df_acf
df_acf<-left_join(df_acf,df_ci,by="RollingWindow_years")%>%mutate(significant_acf=ifelse(abs(acf_vals)>ci,"*","NS"))

#Plot the acfs by rolling lag
ggplot(df_acf, aes(x=lag, y=acf_vals)) +
  geom_bar(stat="identity", width=.05) +
  geom_hline(yintercept = 0) +
  geom_hline(data = df_ci, aes(yintercept = -ci), color="blue", linetype="dotted") +
  geom_hline(data = df_ci, aes(yintercept = ci), color="blue", linetype="dotted") +
  labs(x="Lag", y="ACF") +
  facet_wrap(~RollingWindow_years)

#Summarize which lags are signficant
df_acf_summary<-df_acf%>%filter(significant_acf=="*")%>%ungroup()%>%group_by(lag)%>%dplyr::summarize(common_lag=n())
df_acf%>%filter(significant_acf=="*")%>%print(n=Inf)
#This finds the largest correlation for bigger than 6 year lag
df_acf%>%filter(significant_acf=="*")%>%filter(lag>6)%>%group_by(RollingWindow_years)%>%slice(which.max(abs(acf_vals)))%>%print(n=Inf)
ggplot(data=df_acf_summary,aes(x=lag,y=common_lag))+geom_point()



#Is the variability increasing?#####
#This tests the slope of each time series and determines significance correcting for 27 comparisons
iceDuration_variability_summary<-iceDuration_variability_all%>%group_by(RollingWindow_years)%>%dplyr::summarize(sensSlope_pval=mean(sensSlope_pval),sensSlope_slope=mean(sensSlope_slope),sensSlope_intercept=mean(sensSlope_intercept),sensSlope_z_stat=mean(sensSlope_z_stat),sensSlope_n=mean(sensSlope_n))%>%mutate(significance=ifelse(sensSlope_pval<0.05/27,"*","NS"))%>%print(n=Inf)
#Is the slope different depending on the rolling window
ggplot(data=iceDuration_variability_summary,aes(y=sensSlope_slope,x=RollingWindow_years))+geom_point()


#Merge for a single plot by selecting a specifc rolling window and merging with original data
#Calculate Bollinger Bands by doing the moving average +/- moving sd for that window
window_select<-4 #set window here
Merge_singleRollingWindow<-left_join(tibble(iceDuration_days_withNAs),iceDuration_variability_all%>%filter(RollingWindow_years==window_select)%>%mutate(Year=trunc(year_median)),by="Year")%>%mutate(max_sd=LengthOfIceCover_days_mean+LengthOfIceCover_days_sd,min_sd=LengthOfIceCover_days_mean-LengthOfIceCover_days_sd)

#pacf
pacf(Merge_singleRollingWindow$sensSlope_residuals,na.action = na.pass)

#time series of the residuals
auto.arima(Merge_singleRollingWindow$sensSlope_residuals)

#Graph a single plot with rolling window SDs as shaded region
#These are bollinger band in econ/finance: https://www.investopedia.com/terms/b/bollingerbands.asp
gg.duration<-ggplot(data=Merge_singleRollingWindow)+
  geom_ribbon(aes(x=Year,ymin=min_sd,ymax=max_sd),color="light grey",fill="sky blue")+
  geom_line(aes(x=Year,y=LengthOfIceCover_days_mean),color="light grey",size=0.5)+ #Moving average
  #geom_errorbar(aes(x=Year,ymin=min_sd,ymax=max_sd),size=1)+
  geom_point(aes(x=Year,y=LengthOfIceCover_days),shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+ #actual days of ice cover
  ylab("Ice duration (days)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#*Rolling sd vs. year####
gg.rollingwindow<-ggplot(data=Merge_singleRollingWindow)+
  geom_line(aes(x=year_median,y=sensSlope_fit),color="light grey",size=0.5)+
  geom_point(aes(x=year_median,y=LengthOfIceCover_days_sd),shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
  xlab(bquote(Year))+
  ylab("Ice duration (s.d.)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#*Rolling sd residuals from sens slopes vs. year####
gg.residuals<-ggplot(data=Merge_singleRollingWindow,
                     aes(x=year_median,y=sensSlope_residuals))+
  geom_smooth(method="gam", size=0.5,color=rgb(186,182,170,max=255),fill="sky blue")+
  geom_point(shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
  geom_errorbarh(aes(xmin=1953,xmax=1953+10,y=8))+
  xlab(bquote(Year))+
  ylab("Ice duration (s.d.)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#goes left to right
#panel letter size
panel.size<-10
List<-list(gg.duration+
             #theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
             #scale_y_continuous(limits=c(10,10000),trans="log10",breaks=c(10,100,1000,10000),labels=trans_format("log10",math_format(10^.x)))+
             #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="d",fontface="bold"))+
             ggtitle("(a)"),
           gg.rollingwindow+
             #theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_blank(),axis.text.x=element_blank(),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
             #scale_y_continuous(limits=c(1,10000),trans="log10",breaks=trans_breaks("log10", function(x) 10^x),labels=trans_format("log10",math_format(10^.x)))+ 
             ggtitle("(b)"),
           gg.residuals+
             #theme(axis.title.y=element_text(size=10),axis.text.y = element_text(angle = 90,hjust=0.5),axis.title.x=element_text(size=10),plot.title = element_text(size = panel.size, face = "bold"))+ #Rotate and center the x axis labels
             #scale_y_continuous(breaks=c(3,6,9))+ 
             #geom_text(data=panelLetter.data, aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label="b",fontface="bold"))+
             ggtitle("(c)")
)

#Plot them using patchwork####
(gg.3panel.patchwork.3x1<-wrap_plots(List,ncol = 3,nrow = 1)&theme(plot.margin = unit(c(3,3,3,3),"pt")))
#Could do a 3x3 with width 6, height = 5
ggsave(paste("figures/mohonkWinterLimno-FigureX-RollingWindow",window_select,"Years.jpg",sep=""), plot=gg.3panel.patchwork.3x1, width=6, height=2,units="in", dpi=300)

#Variance contribution calculation####
#Create dataframe all rolling windows####
datalist.variance=list()  #initialize empty list for storing a bunch of ice duration variabilities of different lengths

#Calculate a metric that is the residual^2 for each year, relative to the mean std####  
#*Loop through all the years####
#debug: year_i<-71
for(year_i in 1:length(iceDuration_days_withNAs$Year)){
  
  #find the overall variance
  overall_var<-var(iceDuration_days_withNAs$LengthOfIceCover_days,na.rm=TRUE)
  #drop a year
  dropped_year<-tibble(iceDuration_days_withNAs)%>%filter(Year!=iceDuration_days_withNAs$Year[year_i])
  #calculate new variance - if the year had an NA value, then it should be the same
  dropped_var<-var(dropped_year$LengthOfIceCover_days,na.rm=TRUE)
  #Calculate teh residual for each year
  residual<-iceDuration_days_withNAs$LengthOfIceCover_days[year_i]-mean(iceDuration_days_withNAs$LengthOfIceCover_days,na.rm=TRUE)
  residual_squared<-residual^2
  
  #Calculate some fits
  #Figure out delta var (+ is increase, - is decrease)
  if(is.na(iceDuration_days_withNAs$LengthOfIceCover_days[year_i])){delta_var<-NA}else{delta_var<-overall_var-dropped_var}
  #Figure out % change
  percent_change_var<-100*(delta_var/overall_var)
  
  #Export each year to the datalist  
  #Store the temporary data frame in a list
  datalist.variance[[year_i]]<-tibble(year=iceDuration_days_withNAs$Year[year_i],overall_var=overall_var,dropped_var=dropped_var,delta_var=delta_var,percent_change_var=percent_change_var,residual=residual,residual_squared=residual_squared)  
  
}   

#*compile them all in one data frame####    
iceDuration_variability_metric<-do.call(bind_rows,datalist.variance) 
ggplot(data=iceDuration_variability_metric,aes(x=year,y=delta_var))+geom_point()
ggplot(data=iceDuration_variability_metric,aes(x=year,y=residual_squared))+geom_point()

#See the relationship between teleconnections and residuals
#Find the NAO simple moving average at the same scale as the window above
NAO_SMA<-tibble(
  year=as.vector(rollapply(NAO_summary%>%dplyr::select(water_year), width =window_select, FUN = function(x, na.rm = TRUE)  {median(x, na.rm = na.rm)})),
  NAO_SMA=as.vector(rollapply(NAO_summary%>%dplyr::select(NAO_index_winter), width =window_select, FUN = function(x, na.rm = TRUE)  {mean(x, na.rm = na.rm)})))
#Find the NAO simple moving average at the same scale as the window above
ENSO_SMA<-tibble(
  year=as.vector(rollapply(ENSO_summary%>%dplyr::select(water_year), width =window_select, FUN = function(x, na.rm = TRUE)  {median(x, na.rm = na.rm)})),
  ENSO_SMA=as.vector(rollapply(ENSO_summary%>%dplyr::select(ENSO_index_winter), width =window_select, FUN = function(x, na.rm = TRUE)  {mean(x, na.rm = na.rm)})))

#Plot the residuals plus the points and line for NAO and ENSO####
ggplot(data=NAO_summary,aes(x=water_year,y=NAO_index_winter))+geom_point()+
  geom_line(data=NAO_SMA,aes(x=year,y=NAO_SMA))+
  geom_line(data=ENSO_SMA,aes(x=year,y=ENSO_SMA*100),color="red")+
  geom_point(data=ENSO_summary,aes(x=water_year,y=ENSO_index_winter*100),color="red")+
  #geom_point(data=Merge_singleRollingWindow,aes(x=Year,y=sensSlope_residuals*10),color="blue")+
  geom_smooth(data=Merge_singleRollingWindow,aes(x=Year,y=sensSlope_residuals*10),method="gam")



#Sens slope for each of the three metrics####
sensSlope<-MTCC.sensSlope(iceDuration_temporary$year_median,iceDuration_temporary$LengthOfIceCover_days_sd)

#*Create a new data frame with extra columns####
MohonkIce_resids<-MohonkIce

#*Sens slope for each metric####
duration.sensSlope<-MTCC.sensSlope(x=MohonkIce_resids$Year,y=MohonkIce_resids$LengthOfIceCover_days)
iceIn.sensSlope<-MTCC.sensSlope(x=MohonkIce_resids$Year,y=MohonkIce_resids$IceInDayofYear_fed)
iceOut.sensSlope<-MTCC.sensSlope(x=MohonkIce_resids$Year,y=MohonkIce_resids$IceOutDayofYear_fed)

#*Calculate residuals from the mean
duration.resids<-MohonkIce_resids$LengthOfIceCover_days-mean(MohonkIce_resids$LengthOfIceCover_days,na.rm=TRUE)
iceIn.resids<-MohonkIce_resids$IceInDayofYear_fed-mean(MohonkIce_resids$IceInDayofYear_fed,na.rm=TRUE)
iceOut.resids<-MohonkIce_resids$IceOutDayofYear_fed-mean(MohonkIce_resids$IceOutDayofYear_fed,na.rm=TRUE)

#Bind the residuals from the sens slopes on as extra columns####
MohonkIce_resids<-bind_cols(tibble(MohonkIce_resids),
                            tibble(duration_sens_resids=duration.sensSlope$residuals),
                            tibble(iceIn_sens_resids=iceIn.sensSlope$residuals),
                            tibble(iceOut_sens_resids=iceOut.sensSlope$residuals),
                            tibble(duration_resids=duration.resids),
                            tibble(iceIn_resids=iceIn.resids),
                            tibble(iceOut_resids=iceOut.resids))

#*Visualize the sens slope residuals from all three metrics####
ggplot(data=MohonkIce_resids,aes(x=Year,y=iceIn_sens_resids))+geom_point()+
  geom_point(aes(y=iceOut_sens_resids),col="red")+
  geom_point(aes(y=duration_sens_resids),col="blue")+
  theme_bw()

#*Visualize the residuals from the mean from all three metrics####
ggplot(data=MohonkIce_resids,aes(x=Year,y=iceIn_resids))+geom_point()+
  geom_point(aes(y=iceOut_resids),col="red")+
  geom_point(aes(y=duration_resids),col="blue")+
  theme_bw()

#Next steps. Figure out ENSO/NAO for fall and spring####
#Calculate fall,winter, Oct/Nov/Dec,Nov/Dec, Feb/Mar/Apr,Feb/Mar averages
ENSO_reduced<-ENSO_summary%>%
  dplyr::select(water_year,ENSO_index_fall,ENSO_index_winter,ENSO_Oct,ENSO_Nov,ENSO_Dec,ENSO_Feb,ENSO_Mar,ENSO_Apr)%>%
  mutate(ENSO_OND=(ENSO_Oct+ENSO_Nov+ENSO_Dec)/3,ENSO_ND=(ENSO_Nov+ENSO_Dec)/2,ENSO_FMA=(ENSO_Feb+ENSO_Mar+ENSO_Apr)/3,ENSO_FM=(ENSO_Feb+ENSO_Mar)/2)
#NAO only has seasonal
NAO_reduced<-NAO_summary%>%
  dplyr::select(water_year,NAO_index_fall,NAO_index_winter,NAO_index_spring)
#Convert the daily values into monthly averages
NAO_monthly<-NAO_daily%>%group_by(Year,Month)%>%summarize(NAO_index_monthly=mean(NAO_index))%>%
  mutate(water_year=ifelse(Month>=10,Year,Year-1))%>%print(n=25)

#Get out some needed months ONDFMA
NAO_Oct<-NAO_monthly%>%ungroup()%>%filter(Month==10)%>%mutate(NAO_Oct=NAO_index_monthly)%>%dplyr::select(water_year,NAO_Oct)
NAO_Nov<-NAO_monthly%>%ungroup()%>%filter(Month==11)%>%mutate(NAO_Nov=NAO_index_monthly)%>%dplyr::select(water_year,NAO_Nov)
NAO_Dec<-NAO_monthly%>%ungroup()%>%filter(Month==12)%>%mutate(NAO_Dec=NAO_index_monthly)%>%dplyr::select(water_year,NAO_Dec)
NAO_Feb<-NAO_monthly%>%ungroup()%>%filter(Month==2)%>%mutate(NAO_Feb=NAO_index_monthly)%>%dplyr::select(water_year,NAO_Feb)
NAO_Mar<-NAO_monthly%>%ungroup()%>%filter(Month==3)%>%mutate(NAO_Mar=NAO_index_monthly)%>%dplyr::select(water_year,NAO_Mar)
NAO_Apr<-NAO_monthly%>%ungroup()%>%filter(Month==4)%>%mutate(NAO_Apr=NAO_index_monthly)%>%dplyr::select(water_year,NAO_Apr)

#merge all those months together
NAO_Monthly_wide<-left_join(NAO_Oct,NAO_Nov,by="water_year")%>%
  left_join(.,NAO_Dec,by="water_year")%>%
  left_join(.,NAO_Feb,by="water_year")%>%
  left_join(.,NAO_Mar,by="water_year")%>%
  left_join(.,NAO_Apr,by="water_year")%>%
  mutate(NAO_OND=(NAO_Oct+NAO_Nov+NAO_Nov)/3,NAO_ND=(NAO_Nov+NAO_Nov)/2,NAO_FMA=(NAO_Feb+NAO_Mar+NAO_Apr)/3,NAO_FM=(NAO_Feb+NAO_Mar)/2) #Create new multi-month indices 


#Global climate anomaly
ClimateAnom_Oct<-NOAA_anomaly_monthly%>%ungroup()%>%filter(Month==10)%>%mutate(Anomaly_Oct=GlobalTempanomaly_C,water_year=ifelse(Month>=10,Year,Year-1))%>%dplyr::select(water_year,Anomaly_Oct)
ClimateAnom_Nov<-NOAA_anomaly_monthly%>%ungroup()%>%filter(Month==11)%>%mutate(Anomaly_Nov=GlobalTempanomaly_C,water_year=ifelse(Month>=10,Year,Year-1))%>%dplyr::select(water_year,Anomaly_Nov)
ClimateAnom_Dec<-NOAA_anomaly_monthly%>%ungroup()%>%filter(Month==12)%>%mutate(Anomaly_Dec=GlobalTempanomaly_C,water_year=ifelse(Month>=10,Year,Year-1))%>%dplyr::select(water_year,Anomaly_Dec)
ClimateAnom_Jan<-NOAA_anomaly_monthly%>%ungroup()%>%filter(Month==1)%>%mutate(Anomaly_Jan=GlobalTempanomaly_C,water_year=ifelse(Month>=10,Year,Year-1))%>%dplyr::select(water_year,Anomaly_Jan)
ClimateAnom_Feb<-NOAA_anomaly_monthly%>%ungroup()%>%filter(Month==2)%>%mutate(Anomaly_Feb=GlobalTempanomaly_C,water_year=ifelse(Month>=10,Year,Year-1))%>%dplyr::select(water_year,Anomaly_Feb)
ClimateAnom_Mar<-NOAA_anomaly_monthly%>%ungroup()%>%filter(Month==3)%>%mutate(Anomaly_Mar=GlobalTempanomaly_C,water_year=ifelse(Month>=10,Year,Year-1))%>%dplyr::select(water_year,Anomaly_Mar)
ClimateAnom_Apr<-NOAA_anomaly_monthly%>%ungroup()%>%filter(Month==4)%>%mutate(Anomaly_Apr=GlobalTempanomaly_C,water_year=ifelse(Month>=10,Year,Year-1))%>%dplyr::select(water_year,Anomaly_Apr)

#merge all those months together
ClimateAnom_Monthly_wide<-left_join(ClimateAnom_Oct,ClimateAnom_Nov,by="water_year")%>%
  left_join(.,ClimateAnom_Dec,by="water_year")%>%
  left_join(.,ClimateAnom_Jan,by="water_year")%>%
  left_join(.,ClimateAnom_Feb,by="water_year")%>%
  left_join(.,ClimateAnom_Mar,by="water_year")%>%
  left_join(.,ClimateAnom_Apr,by="water_year")%>%
  mutate(Anomaly_OND=(Anomaly_Oct+Anomaly_Nov+Anomaly_Nov)/3,Anomaly_ND=(Anomaly_Nov+Anomaly_Nov)/2,Anomaly_FMA=(Anomaly_Feb+Anomaly_Mar+Anomaly_Apr)/3,Anomaly_FM=(Anomaly_Feb+Anomaly_Mar)/2) #Create new multi-month indices 

#Merge with the seasonal data
NAO_reduced2<-left_join(NAO_reduced,NAO_Monthly_wide,by="water_year")

#Join the resids together with the ENSO and NAO results####
MohonkIce_resids_tele<-left_join(MohonkIce_resids%>%mutate(water_year=Year-1),ENSO_reduced,by=c("water_year"))%>%
  left_join(.,NAO_reduced2,by=c("water_year"))%>%
  left_join(.,ClimateAnom_Monthly_wide,by=c("water_year"))

#Visualize some of the relationships
#Check Ice In residuals vs. fall teleconnections
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_ND,y=iceIn_sens_resids))+geom_point()+geom_smooth(method="lm")
cor.test(MohonkIce_resids_tele$iceIn_sens_resids,MohonkIce_resids_tele$NAO_ND)
#For Ice In - look at ENSO_ND and NAO_ND in auto_arima
#Check Ice out residuals vs. spring teleconnections
ggplot(data=MohonkIce_resids_tele,aes(x=ENSO_FM,y=iceOut_sens_resids))+geom_point()+geom_smooth(method="lm")
cor.test(MohonkIce_resids_tele$iceIn_sens_resids,MohonkIce_resids_tele$ENSO_Feb)
#For Ice In - look at ENSO_FM, ENSO_Feb and NAO_Apr in auto_arima

#Check duration residuals vs. spring teleconnections
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_Nov,y=duration_sens_resids))+geom_point()+geom_smooth(method="lm")
cor.test(MohonkIce_resids_tele$duration_sens_resids,MohonkIce_resids_tele$NAO_Nov)
#For Ice In - look at ENSO_FM, ENSO_Feb and NAO_Apr in auto_arima

#Ice in Fit auto.arima with xreg as year####
arima.IceIn<-auto.arima(MohonkIce_resids_tele$iceIn_sens_resids,
                        #xreg=c(iceDuration_temporary$year_median),
                        xreg=cbind(MohonkIce_resids_tele$ENSO_ND,
                                   MohonkIce_resids_tele$NAO_ND),
                        seasonal=FALSE,allowdrift = FALSE,
                        stationary=TRUE)

arima.IceIn #print out model
(1-pnorm(abs(arima.IceIn$coef)/sqrt(diag(arima.IceIn$var.coef))))*2 #P-values of coefficients

#Biplot with two different indices and color as Ice In residuals
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_ND,y=ENSO_ND,fill=iceIn_sens_resids,size=iceIn_sens_resids))+geom_point(shape=21,color="black")+
  scale_fill_gradientn(colours=c("blue","cyan","white", "yellow","red"))


#Ice out Fit auto.arima with xreg as year####
arima.IceOut<-auto.arima(MohonkIce_resids_tele$iceOut_sens_resids,
                         #xreg=c(iceDuration_temporary$year_median),
                         xreg=cbind(MohonkIce_resids_tele$ENSO_Apr,
                                    MohonkIce_resids_tele$NAO_Apr),
                         seasonal=FALSE,allowdrift = FALSE,
                         stationary=TRUE)
arima.IceOut
(1-pnorm(abs(arima.IceOut$coef)/sqrt(diag(arima.IceOut$var.coef))))*2


#Ice out Fit auto.arima with xreg as year####
arima.duration<-auto.arima(MohonkIce_resids_tele$duration_sens_resids,
                           #xreg=c(iceDuration_temporary$year_median),
                           xreg=cbind(
                             MohonkIce_resids_tele$NAO_Nov
                             
                             
                             
                           ),
                           seasonal=FALSE,allowdrift = FALSE,
                           stationary=TRUE)
arima.duration
(1-pnorm(abs(arima.duration$coef)/sqrt(diag(arima.duration$var.coef))))*2

#Biplot with two different indices and color as Ice In residuals
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_ND,y=ENSO_ND,fill=duration_sens_resids,size=duration_sens_resids))+geom_point(shape=21,color="black")+
  scale_fill_gradientn(colours=rev(c("blue","cyan","white", "yellow","red")))



#Examine all the pairwise correlations of each of the different plots with our climate metrics####
#*Duration correlations####
duration.correlation <- MohonkIce_resids_tele %>% 
  select_if(is.numeric) %>%
  gather(Column, Value, -duration_resids) %>%
  group_by(Column) %>%
  nest() %>%
  mutate(Cor = map(data, ~cor.test(.x$Value, .x$duration_resids, method = "spearman"))) %>%
  mutate(Estimate = round(map_dbl(Cor, "estimate"), 2), 
         P_Value = map_dbl(Cor, "p.value"))%>% 
  select(-data, -Cor)%>%
  mutate(Significance = case_when(
    P_Value < 0.001  ~ "*** <0,001",
    P_Value < 0.01   ~ "** <0,01",
    P_Value < 0.05   ~ "*<0,05",
    TRUE             ~ "Not Significant"
  ))%>%arrange(-abs(Estimate))%>%print(n=Inf) #Rank by the correlation coefficient

#*put in some of the candidates for auto.arima####
#Ice out Fit auto.arima with xreg as year####
arima.duration<-auto.arima(MohonkIce_resids_tele$duration_resids,
                           #xreg=c(iceDuration_temporary$year_median),
                           xreg=cbind(
                             MohonkIce_resids_tele$NAO_ND,
                             MohonkIce_resids_tele$Anomaly_FMA
                             
                             
                           ),
                           seasonal=FALSE,allowdrift = FALSE,
                           stationary=TRUE)
arima.duration
(1-pnorm(abs(arima.duration$coef)/sqrt(diag(arima.duration$var.coef))))*2

#Biplot with two different indices and color as ice duration residuals
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_ND,y=Anomaly_FMA,fill=duration_resids,size=duration_resids))+geom_point(shape=21,color="black")+
  scale_fill_gradientn(colours=rev(c("blue","cyan","white", "yellow","red")))+theme_bw()
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_ND,y=duration_resids))+geom_point()+theme_bw()
ggplot(data=MohonkIce_resids_tele,aes(x=Anomaly_FMA,y=duration_resids))+geom_point()+theme_bw()


#*Ice In correlations####
iceIn.correlation <- MohonkIce_resids_tele %>% 
  select_if(is.numeric) %>%
  gather(Column, Value, -iceIn_resids) %>%
  group_by(Column) %>%
  nest() %>%
  mutate(Cor = map(data, ~cor.test(.x$Value, .x$iceIn_resids, method = "spearman"))) %>%
  mutate(Estimate = round(map_dbl(Cor, "estimate"), 2), 
         P_Value = map_dbl(Cor, "p.value"))%>% 
  select(-data, -Cor)%>%
  mutate(Significance = case_when(
    P_Value < 0.001  ~ "*** <0,001",
    P_Value < 0.01   ~ "** <0,01",
    P_Value < 0.05   ~ "*<0,05",
    TRUE             ~ "Not Significant"
  ))%>%arrange(-abs(Estimate))%>%print(n=Inf) #Rank by the correlation coefficient

#*put in some of the candidates for auto.arima####
#Ice out Fit auto.arima with xreg as year####
arima.iceIn<-auto.arima(MohonkIce_resids_tele$iceIn_resids,
                        #xreg=c(iceDuration_temporary$year_median),
                        xreg=cbind(
                          MohonkIce_resids_tele$NAO_Nov,
                          MohonkIce_resids_tele$NAO_Dec
                          
                          
                        ),
                        seasonal=FALSE,allowdrift = FALSE,
                        stationary=TRUE)
arima.iceIn
(1-pnorm(abs(arima.iceIn$coef)/sqrt(diag(arima.iceIn$var.coef))))*2

#Biplot with two different indices and color as ice duration residuals
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_Nov,y=NAO_Dec,fill=iceIn_resids,size=iceIn_resids))+geom_point(shape=21,color="black")+
  scale_fill_gradientn(colours=c("blue","cyan","white", "yellow","red"))+theme_bw()
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_Nov,y=iceIn_resids))+geom_point()+theme_bw()
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_Dec,y=iceIn_resids))+geom_point()+theme_bw()


#*Ice out correlations####
iceOut.correlation <- MohonkIce_resids_tele %>% 
  select_if(is.numeric) %>%
  gather(Column, Value, -iceOut_resids) %>%
  group_by(Column) %>%
  nest() %>%
  mutate(Cor = map(data, ~cor.test(.x$Value, .x$iceOut_resids, method = "spearman"))) %>%
  mutate(Estimate = round(map_dbl(Cor, "estimate"), 2), 
         P_Value = map_dbl(Cor, "p.value"))%>% 
  select(-data, -Cor)%>%
  mutate(Significance = case_when(
    P_Value < 0.001  ~ "*** <0,001",
    P_Value < 0.01   ~ "** <0,01",
    P_Value < 0.05   ~ "*<0,05",
    TRUE             ~ "Not Significant"
  ))%>%arrange(-abs(Estimate))%>%print(n=Inf) #Rank by the correlation coefficient

#*put in some of the candidates for auto.arima####
#Ice out Fit auto.arima with xreg as year####
arima.iceIn<-auto.arima(MohonkIce_resids_tele$iceIn_resids,
                        #xreg=c(iceDuration_temporary$year_median),
                        xreg=cbind(
                          MohonkIce_resids_tele$NAO_Nov,
                          MohonkIce_resids_tele$NAO_Dec
                          
                          
                        ),
                        seasonal=FALSE,allowdrift = FALSE,
                        stationary=TRUE)
arima.iceIn
(1-pnorm(abs(arima.iceIn$coef)/sqrt(diag(arima.iceIn$var.coef))))*2

#Biplot with two different indices and color as ice duration residuals
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_Nov,y=NAO_Dec,fill=iceIn_resids,size=iceIn_resids))+geom_point(shape=21,color="black")+
  scale_fill_gradientn(colours=c("blue","cyan","white", "yellow","red"))+theme_bw()
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_Nov,y=iceIn_resids))+geom_point()+theme_bw()
ggplot(data=MohonkIce_resids_tele,aes(x=NAO_Dec,y=iceIn_resids))+geom_point()+theme_bw()

#Alternative window analysis######
#consecutive windows##

#Create a data list####
#Create dataframe non-rolling windows####
datalist_sequential=list()  #initialize empty list for storing a bunch of ice duration variabilities of different lengths
datalist_index=1 #initialize counter to tick through datalist

#how many years are in each segment
#segment_length=30
for(segment_length in 3:30){
  #Creates a grouping variable that is a wee bit bigger than the original by rounding up
  grouping_var<-tibble(grouping_var=rep(1:ceiling((length(iceDuration_days_withNAs$Year)+segment_length-1)/segment_length), each=segment_length))
  #loop through all possible arrangements of the segment_length
  #debug j.index=20
  for(j.index in 1:segment_length){
    #start at j.index, remove the last segment_length-j.index
    grouping_var_sub<-slice(grouping_var,(j.index):(length(iceDuration_days_withNAs$Year)+j.index-1))
    #Merge with the data of interest
    temp_segments<-bind_cols(iceDuration_days_withNAs,grouping_var_sub)%>%
      group_by(grouping_var)%>%
      summarize(segment_start_year=min(Year),segment_end_year=max(Year),sd_segment=sd(LengthOfIceCover_days,na.rm=TRUE),number_nonNA=sum(!is.na(LengthOfIceCover_days)),prop_nonNA=number_nonNA/segment_length)%>%
      mutate(sd_segment=ifelse(prop_nonNA>=0.75,sd_segment,NA),
             segment_midpoint_year=(segment_start_year+segment_end_year)/2)
    #ggplot(data=temp_segments,aes(x=segment_midpoint_year,y=sd_segment))+geom_point()
    
    
    #Calculate slope
    sensSlope_sequential<-MTCC.sensSlope(temp_segments$segment_midpoint_year,temp_segments$sd_segment)
    #Store the residuals and other sens slope fit stats in iceDuration_temporary  
    sensSlopes_temporaryStats<-temp_segments%>%
      mutate(sensSlope_fit=segment_midpoint_year*sensSlope_sequential$coefficients["Year"]+sensSlope_sequential$coefficients["Intercept"],
             sensSlope_slope=sensSlope_sequential$coefficients["Year"],
             sensSlope_intercept=sensSlope_sequential$coefficients["Intercept"],
             sensSlope_residuals=sensSlope_sequential$residuals,
             sensSlope_pval=sensSlope_sequential$pval,
             sensSlope_z_stat=sensSlope_sequential$z_stat,
             sensSlope_n=sensSlope_sequential$n,
             segment_length=segment_length,
             starting_index=j.index)
    #Export each Rollingwindow length to the datalist  
    datalist_sequential[[datalist_index]]<-sensSlopes_temporaryStats
    datalist_index<-datalist_index+1 #increment datalist forward by 1
  } #End of looping through the various starting positions
} #End of looping through the different segment lengths
#*compile them all in one data frame####    
iceDuration_variability_sequential<-do.call(bind_rows,datalist_sequential) 

#Unpack some results
#Plot segment sd with starting position as a color
#this kind of gets at the rolling sd with all of them together
iceDuration_variability_sequential%>%filter(segment_length==4)%>%ggplot(.,aes(x=segment_midpoint_year,y=sd_segment,color=as.factor(starting_index)))+geom_point()
iceDuration_variability_sequential%>%filter(segment_length==9)%>%ggplot(.,aes(x=segment_midpoint_year,y=sd_segment,color=as.factor(starting_index)))+geom_point()
iceDuration_variability_sequential%>%filter(segment_length==20)%>%ggplot(.,aes(x=segment_midpoint_year,y=sd_segment,color=as.factor(starting_index)))+geom_point()

#Plot segment sd fit with starting position as a color
iceDuration_variability_sequential%>%filter(segment_length==4)%>%ggplot(.,aes(x=segment_midpoint_year,y=sensSlope_fit,color=as.factor(starting_index)))+geom_line()
iceDuration_variability_sequential%>%filter(segment_length==9)%>%ggplot(.,aes(x=segment_midpoint_year,y=sensSlope_fit,color=as.factor(starting_index)))+geom_line()
iceDuration_variability_sequential%>%filter(segment_length==20)%>%ggplot(.,aes(x=segment_midpoint_year,y=sensSlope_fit,color=as.factor(starting_index)))+geom_line()

#Make into 
iceDuration_variability_sequential_4years<-left_join(iceDuration_variability_sequential%>%filter(segment_length==4),iceDuration_variability_sequential%>%filter(grouping_var==1,segment_length==4)%>%mutate(YearOfStart=segment_end_year+1)%>%dplyr::select(starting_index,YearOfStart),by="starting_index")  #%>%arrange(Min_segment_start_year,grouping_var)
iceDuration_variability_sequential_9years<-left_join(iceDuration_variability_sequential%>%filter(segment_length==9),iceDuration_variability_sequential%>%filter(grouping_var==1,segment_length==9)%>%mutate(YearOfStart=segment_end_year+1)%>%dplyr::select(starting_index,YearOfStart),by="starting_index")  
iceDuration_variability_sequential_13years<-left_join(iceDuration_variability_sequential%>%filter(segment_length==13),iceDuration_variability_sequential%>%filter(grouping_var==1,segment_length==13)%>%mutate(YearOfStart=segment_end_year+1)%>%dplyr::select(starting_index,YearOfStart),by="starting_index")  


#Plot segment sd with facet wrap by starting position
#plot with points and slope together
#13 seems to be the cutoff for having 5+ points
(gg.seq4<-ggplot(iceDuration_variability_sequential_4years,aes(x=segment_midpoint_year,y=sd_segment))+geom_point()+geom_line(aes(y=sensSlope_fit))+facet_wrap(vars(YearOfStart))+theme_bw()+ylab("Duration sd (days)")+xlab("Year"))
(gg.seq9<-ggplot(iceDuration_variability_sequential_9years,aes(x=segment_midpoint_year,y=sd_segment))+geom_point()+geom_line(aes(y=sensSlope_fit))+facet_wrap(vars(YearOfStart))+theme_bw()+ylab("Duration sd (days)")+xlab("Year"))
(gg.seq13<-ggplot(iceDuration_variability_sequential_13years,aes(x=segment_midpoint_year,y=sd_segment))+geom_point()+geom_line(aes(y=sensSlope_fit))+facet_wrap(vars(YearOfStart))+theme_bw()+ylab("Duration sd (days)")+xlab("Year"))

#Print out each of these
ggsave(paste("figures/FigXSupplement_Sequential4years.jpg",sep=""), plot=gg.seq4, width=6, height=5,units="in", dpi=300)
ggsave(paste("figures/FigXSupplement_Sequential9years.jpg",sep=""), plot=gg.seq9, width=6, height=5,units="in", dpi=300)
ggsave(paste("figures/FigXSupplement_Sequential13years.jpg",sep=""), plot=gg.seq13, width=6, height=5,units="in", dpi=300)


#Find the significant sens slopes
#note, there are only 2
iceDuration_variability_sequential%>%filter(sensSlope_pval<0.05)%>%ggplot(.,aes(x=segment_midpoint_year,y=sensSlope_fit,color=as.factor(segment_length)))+geom_line()

#Graph them all, facet_wrap by segment length, filter to 13 years segment length or less
iceDuration_variability_sequential%>%filter(segment_length<=13)%>%ggplot(.,aes(x=segment_midpoint_year,y=sensSlope_fit,color=as.factor(starting_index)))+geom_line()+facet_wrap(vars(segment_length))

#Summarize the stats and visualize
iceDuration_variability_sequential%>%
  group_by(segment_length)%>%
  summarize(sensSlope_median=median(sensSlope_slope,na.rm=TRUE))%>%
  print(n=Inf)%>%ggplot(.,aes(x=segment_length,y=sensSlope_median))+geom_point()

#Group by segment_length and starting index, 
#slice takes the first row of each to avoid redundancy as there are many slopes in each
#cut to 3 to 13 segment lengths, longer and there are just too few points in each slope
iceDuration_variability_sequential%>%
  group_by(segment_length,starting_index)%>%slice(1)%>%
  filter(segment_length<=13)%>%
  ggplot(.,aes(x=as.factor(segment_length),y=sensSlope_slope))+geom_boxplot()+geom_jitter()+theme_bw()

#Summarize one row for each segment_length and starting index
iceDuration_variability_sequential_fits<-
  iceDuration_variability_sequential%>%
  group_by(segment_length,starting_index)%>%slice(1)%>%
  filter(segment_length<=13)

datalist_sensfits=list()  #initialize empty list for storing a bunch of ice duration variabilities of different lengths
#model.index=1
#loop through to generate sens slope fits for each year, each segment_length, each fit location####
for(model.index in 1:length(iceDuration_variability_sequential_fits$sensSlope_slope)){
  year=seq(min(iceDuration_days_withNAs$Year),max(iceDuration_days_withNAs$Year))
  datalist_sensfits[[model.index]]<-tibble(year=year,sensSlope_fit_interpolate=iceDuration_variability_sequential_fits$sensSlope_slope[model.index]*year+iceDuration_variability_sequential_fits$sensSlope_intercept[model.index],segment_length=iceDuration_variability_sequential_fits$segment_length[model.index],starting_index=iceDuration_variability_sequential_fits$starting_index[model.index])
}

#*bind all the interpolated sens slope fits####
iceDuration_sensSlopeFitsInterpolated_sequential<-do.call(bind_rows,datalist_sensfits)
#calculate fit stats for each segment length
#median, and percentiles
temp<-iceDuration_sensSlopeFitsInterpolated_sequential%>%
  group_by(segment_length,year)%>%
  summarize(median_sensSlope_fit=median(sensSlope_fit_interpolate,na.rm=TRUE),
            q5_sensSlope_fit=quantile(sensSlope_fit_interpolate,probs=0.05,na.rm=TRUE),
            q25_sensSlope_fit=quantile(sensSlope_fit_interpolate,probs=0.25,na.rm=TRUE),
            q75_sensSlope_fit=quantile(sensSlope_fit_interpolate,probs=0.75,na.rm=TRUE),
            q95_sensSlope_fit=quantile(sensSlope_fit_interpolate,probs=0.95,na.rm=TRUE))
#*plot the median sens slope fits surrounded by 25, 75 and 5 and 95 ribbons####
ggplot(temp,aes(x=year,y=median_sensSlope_fit))+
  facet_wrap(vars(segment_length))+
  geom_ribbon(aes(ymin=q5_sensSlope_fit,ymax=q95_sensSlope_fit),alpha=0.1,fill="grey",color="dark grey")+
  #geom_ribbon(aes(ymin=q25_sensSlope_fit,ymax=q75_sensSlope_fit),alpha=0.1,fill="light grey",color="grey")+
  geom_line(size=1.2)+
  theme_bw()


#Questions remain:
#periodicity in the residuals? Is it a residuals of the rolling window?
#GARCH models in econ get at volatility: https://www.idrisstsafack.com/post/garch-models-with-r-programming-a-practical-example-with-tesla-stock



# 07_HighFrequencyAnalysis.R ----------------------------------------------
#Mostly data vis / exploratory plots

 #plot ice % cover####
 ggplot(data=IceDataExtraction%>%filter(year>=2016&year<=2018),aes(x=Date_format,y=IceCover_Percent))+geom_point()

 #Check out individual plots####
 ggplot(data=SensorData%>%pivot_longer(-1),aes(x=DateTime,y=value,color=name))+geom_line()+
     geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
     geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))

#Zoom in on 2016-2017 winter####
# *Set limits####
lims <- as.POSIXct(strptime(c("2016-12-10 19:00", "2017-04-20 20:00"),
                            format = "%Y-%m-%d %H:%M"))
#Plot that winter####
ggplot(data=SensorData%>%pivot_longer(-1)%>%filter(DateTime>=as.POSIXct("2016-11-24 19:00:00 EST")&DateTime<=as.POSIXct("2017-04-20 20:00:00 EDT")),
       aes(x=DateTime,y=value,color=name))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  scale_x_datetime(limits=lims)+scale_y_continuous(limits=c(0,7))

#Graph the Percent ice cover
ggplot(data=IceDataExtraction,aes(x=as.POSIXct(Date_format),y=IceCover_Percent))+geom_point()+
  scale_x_datetime(limits=lims)

#Graph the windspeed or other met data here####
ggplot(data=MesonetData,aes(x=DateTime,y=temp_2m_max_degC))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  scale_x_datetime(limits=lims)

#Zoom in on 2017-2018 winter####
#*Set limits####
lims <- as.POSIXct(strptime(c("2017-12-12 19:00", "2018-04-24 20:00"),
                            format = "%Y-%m-%d %H:%M"))
#Plot that winter####
ggplot(data=SensorData%>%pivot_longer(-1)%>%filter(DateTime>=as.POSIXct("2017-12-12 19:00:00 EST")&DateTime<=as.POSIXct("2018-04-24 20:00:00 EDT")),
       aes(x=DateTime,y=value,color=name))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  scale_x_datetime(limits=lims)

#Graph the Percent ice cover
ggplot(data=IceDataExtraction,aes(x=as.POSIXct(Date_format),y=IceCover_Percent))+geom_point()+
  scale_x_datetime(limits=lims)

#Plot some daily values of schmidt stability####
#*Add in the ice on/off phenology and ice cover percentage####
ggplot(data=dailySensorData_derivedFill,aes(x=DateTime,y=stability_Jperm2))+geom_line()+
  geom_point(aes(y=IceCover_Percent/10))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  scale_y_continuous(limits=c(-0.5,10.5),breaks=c(0,2.5,5,7.5,10),sec.axis=sec_axis(~.*10,name="IceCover_percent",breaks=c(0,25,50,75,100)))

#Plot some daily values of CV of schmidt stability a la Bruesewitz####
#*Add in the ice on/off phenology and ice cover percentage####
ggplot(data=dailySensorData_derivedFill,aes(x=DateTime,y=stability_Jperm2_dailyCV))+geom_line()+
  geom_point(aes(y=IceCover_Percent/10))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  scale_y_continuous(limits=c(0,27),breaks=c(0,10,20,30),sec.axis=sec_axis(~.*10,name="IceCover_percent",breaks=c(0,25,50,75,100)))

#Plot some daily values of delta schmidt stability a la Bruesewitz####
#*Add in the ice on/off phenology and ice cover percentage####
ggplot(data=dailySensorData_derivedFill,aes(x=DateTime,y=stability_Jperm2_diff))+geom_point()+
  geom_point(aes(y=IceCover_Percent/10),color="black",shape=23,fill="light blue")+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  geom_hline(yintercept=stability_diff_icecoverBounds$mean_stability_Jperm2_diff,color="red")+
  geom_hline(yintercept=stability_diff_icecoverBounds$mean_stability_Jperm2_diff+stability_diff_icecoverBounds$sd_stability_Jperm2_diff_3x,color="red")+
  geom_hline(yintercept=stability_diff_icecoverBounds$mean_stability_Jperm2_diff-stability_diff_icecoverBounds$sd_stability_Jperm2_diff_3x,color="red")+
  scale_y_continuous(limits=c(-2,27),breaks=c(0,10,20,30),sec.axis=sec_axis(~.*10,name="IceCover_percent",breaks=c(0,25,50,75,100)))


#Plot some daily values of buoyancy frequency####
#*Add in the ice on/off phenology and ice cover percentage####
ggplot(data=dailySensorData_derivedFill,aes(x=DateTime,y=buoyancyfrequency_1_s2*(60*60)))+geom_line()+
  geom_point(aes(y=IceCover_Percent/30))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  scale_y_continuous(limits=c(-0.5,3.5),breaks=c(0,1,2,3),sec.axis=sec_axis(~.*30,name="IceCover_percent",breaks=c(0,25,50,75,100)))

#Plot some daily values of CV of buoyancy freq. a la Bruesewitz####
#*Add in the ice on/off phenology and ice cover percentage####
ggplot(data=dailySensorData_derivedFill,aes(x=DateTime,y=buoyancyfrequency_1_s2_dailyCV))+geom_line()+
  geom_point(aes(y=IceCover_Percent/100))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  scale_y_continuous(limits=c(0,3),breaks=c(0,1,2,3),sec.axis=sec_axis(~.*100,name="IceCover_percent",breaks=c(0,25,50,75,100)))

#Plot some daily values of delta buoyancy frequency####
#*Add in the ice on/off phenology and ice cover percentage####
ggplot(data=dailySensorData_derivedFill,aes(x=DateTime,y=buoyancyfrequency_1_s2_diff))+geom_point()+
  geom_point(aes(y=IceCover_Percent/100000),color="black",shape=23,fill="light blue")+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  geom_hline(yintercept=stability_diff_icecoverBounds$mean_stability_Jperm2_diff,color="red")+
  geom_hline(yintercept=stability_diff_icecoverBounds$mean_stability_Jperm2_diff+stability_diff_icecoverBounds$sd_stability_Jperm2_diff_3x,color="red")+
  geom_hline(yintercept=stability_diff_icecoverBounds$mean_stability_Jperm2_diff-stability_diff_icecoverBounds$sd_stability_Jperm2_diff_3x,color="red")+
  scale_y_continuous(limits=c(-0.0005,0.001),breaks=c(-0.0005,0,0.0005,0.001),sec.axis=sec_axis(~.*100000,name="IceCover_percent",breaks=c(0,25,50,75,100)))


#Plot temperature graphs with different ice phenology####
ggplot(data=SensorData_derivedFill%>%dplyr::select(DateTime:Temp_9m)%>%pivot_longer(-1),aes(x=DateTime,y=value,color=name))+geom_line()+
  geom_vline(data=IceOnIceOff_hfYears,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff_hfYears,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  geom_vline(data=IceOnIceOff_hfYears,aes(xintercept=as.POSIXct(IceIn_1_date_Pierson)),color="red")+
  geom_vline(data=IceOnIceOff_hfYears,aes(xintercept=as.POSIXct(IceOut_1_date_Pierson)),color="red")+
  geom_vline(data=IceOnIceOff_hfYears,aes(xintercept=as.POSIXct(IceIn_1_date_Bruesewitz)),color="green")+
  geom_point(data=SensorData_derivedFill,aes(x=DateTime,y=IceCover_Percent/10),color="black",shape=23,fill="light blue")+
  geom_point(data=dailySensorData_derivedFill,aes(x=DateTime,y=inverseStratification_numeric*5),inherit.aes = FALSE)+ #Make sure to not inherit the aes from the main ggplot statement####
  scale_y_continuous(limits=c(0,11),breaks=c(0,2.5,5,7.5,10),sec.axis=sec_axis(~.*10,name="IceCover_percent",breaks=c(0,25,50,75,100)))+
  ylab(bquote(Water~Temp~(degree*C)))+
  theme_bw()

#Graph some data###
#*temperature data####
ggplot(data=SensorData_derivedFill%>%dplyr::select(DateTime:Temp_9m)%>%pivot_longer(-1),aes(x=DateTime,y=value,color=name))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))
#*Stability data####
ggplot(data=SensorData_derived,aes(x=DateTime,y=stability_Jperm2))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  scale_y_continuous(limits=c(-0.5,10))
#*BuoyancyFreq. data####
ggplot(data=SensorData_derivedFill,aes(x=DateTime,y=buoyancyfrequency_1_s2))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))
#*Thermocline based on max diff or threshold doesn't show much####
ggplot(data=SensorData_derivedFill,aes(x=DateTime,y=thermoclineDepth_m_thresh0.1))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))
#*Top vs. bottom (Pierson et al. indicates inverse strat at top is 0.1C below that of bottom)####
ggplot(data=SensorData_derivedFill,aes(x=DateTime,y=temperatureDifferenceTop0mvsBottom9m))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  geom_hline(yintercept=-0.1,color="red")
#*Graph % ice cover for those two winters####
ggplot(data=SensorData_derivedFill,aes(x=DateTime,y=IceCover_Percent))+geom_point()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))
#*Graph some met data for those two winters####
ggplot(data=SensorData_derivedFill,aes(x=DateTime,y=temp_2m_min_degC))+geom_point()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))
#*Delta of Stability data####
ggplot(data=SensorData_derivedFill,aes(x=DateTime,y=stability_Jperm2_diff))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  scale_y_continuous(limits=c(-0.5,10))
#*Delta of BuoyancyFreq. data####
ggplot(data=SensorData_derivedFill,aes(x=DateTime,y=buoyancyfrequency_1_s2_diff))+geom_line()+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))



##################STOPPED HERE##############################
#STICH THESE 6 MET DATA FIGURES TOGETHER IN A 2 COLUMN, 3 ROW PLOT USING THE CODE ABOVE
#INCLUDE YEARS AND PANEL LABELS
#EXPORT TOO

ggplot(data=dailySensorData_derivedFill,aes(x=DateTime,y=(temp_2m_avg_degC+lag(temp_2m_avg_degC,1)+lag(temp_2m_avg_degC,2))/3))+geom_point(size=2,color="purple")+
  geom_hline(yintercept=-2.5,color="red")+
  geom_vline(data=IceOnIceOff_hfYears,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff_hfYears,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  geom_vline(data=IceOnIceOff_hfYears,aes(xintercept=as.POSIXct(IceIn_1_date_Pierson)),color="red")+
  geom_vline(data=IceOnIceOff_hfYears,aes(xintercept=as.POSIXct(IceOut_1_date_Pierson)),color="red")+
  #geom_point(data=SensorData_derivedFill,aes(x=DateTime,y=temp_2m_avg_degC))+
  geom_line(data=SensorData_derivedFill%>%mutate(ma_temp_avg_degC=forecast::ma(temp_2m_avg_degC,order=48*4,centre=TRUE)),aes(x=DateTime,y=ma_temp_avg_degC),color="blue")+
  #geom_line(data=SensorData_derivedFill,aes(x=DateTime,y=solar_insolation_total_MJpm2*15),color="orange")+
  #geom_line(data=SensorData_derivedFill,aes(x=DateTime,y=wind_speed_prop_avg_mps*20),color="grey")+
  #geom_point(data=SensorData_derivedFill,aes(x=DateTime,y=IceCover_Percent),color="black",shape=23,fill="light blue")+
  scale_x_datetime(limits=lims)

#Calculate the air temperatute Moving average and see how that matches up with metrics of meteorology and inverse strat####
#*2016####
ggplot(data=SensorData_derivedFill%>%mutate(ma_temp_avg_degC=forecast::ma(temp_2m_avg_degC,order=96*3,centre=TRUE))%>%filter(DateTime<=as.POSIXct("2016-12-19 00:00:00 EST")&DateTime>=as.POSIXct("2016-12-01 00:00:00 EST")),aes(x=DateTime,y=ma_temp_avg_degC))+geom_line()+geom_hline(yintercept=-0.1,color="red")+
  geom_line(aes(y=station_pressure_avg_mbar-1000),color="green")+
  geom_line(aes(y=temperatureDifferenceTop0mvsBottom9m*10),color="blue")+
  geom_line(aes(y=stability_Jperm2*5),color="red")+
  geom_line(aes(y=wind_speed_prop_max_mps),color="grey")
ggplot(data=SensorData_derivedFill%>%mutate(ma_temp_avg_degC=forecast::ma(temp_2m_avg_degC,order=96*3,centre=TRUE))%>%filter(DateTime<=as.POSIXct("2016-12-16 00:00:00 EST")&DateTime>=as.POSIXct("2016-12-01 00:00:00 EST")),aes(x=ma_temp_avg_degC,y=temperatureDifferenceTop0mvsBottom9m))+geom_line()+geom_hline(yintercept=-0.1,color="red")
ggplot(data=SensorData_derivedFill%>%mutate(ma_temp_avg_degC=forecast::ma(temp_2m_avg_degC,order=96*3,centre=TRUE))%>%filter(DateTime<=as.POSIXct("2016-12-16 00:00:00 EST")&DateTime>=as.POSIXct("2016-12-01 00:00:00 EST")),aes(x=ma_temp_avg_degC,y=stability_Jperm2))+geom_line()+geom_hline(yintercept=-0.1,color="red")
ggplot(data=SensorData_derivedFill%>%mutate(ma_temp_avg_degC=forecast::ma(temp_2m_avg_degC,order=96*3,centre=TRUE))%>%filter(DateTime<=as.POSIXct("2016-12-16 00:00:00 EST")&DateTime>=as.POSIXct("2016-12-01 00:00:00 EST")),aes(x=ma_temp_avg_degC,y=station_pressure_avg_mbar))+geom_line()
SensorData_derivedFill%>%mutate(ma_temp_avg_degC=forecast::ma(temp_2m_avg_degC,order=96*3,centre=TRUE))%>%filter(DateTime<=as.POSIXct("2016-12-16 00:00:00 EST")&DateTime>=as.POSIXct("2016-12-01 00:00:00 EST"))%>%filter(temperatureDifferenceTop0mvsBottom9m<(-0.1))%>%dplyr::select(DateTime,temperatureDifferenceTop0mvsBottom9m,stability_Jperm2,ma_temp_avg_degC)
#*2017####
ggplot(data=SensorData_derivedFill%>%mutate(ma_temp_avg_degC=forecast::ma(temp_2m_avg_degC,order=96*3,centre=TRUE))%>%filter(DateTime<=as.POSIXct("2017-12-19 00:00:00 EST")&DateTime>=as.POSIXct("2017-12-01 00:00:00 EST")),aes(x=DateTime,y=ma_temp_avg_degC))+geom_line()+geom_hline(yintercept=-0.1,color="red")+
  geom_line(aes(y=station_pressure_avg_mbar-1000),color="green")+
  geom_line(aes(y=temperatureDifferenceTop0mvsBottom9m*10),color="blue")+
  geom_line(aes(y=stability_Jperm2*5),color="red")+
  geom_line(aes(y=wind_speed_prop_max_mps),color="grey")
ggplot(data=SensorData_derivedFill%>%mutate(ma_temp_avg_degC=forecast::ma(temp_2m_avg_degC,order=96*3,centre=TRUE))%>%filter(DateTime<=as.POSIXct("2017-12-14 00:00:00 EST")&DateTime>=as.POSIXct("2017-12-01 00:00:00 EST")),aes(x=ma_temp_avg_degC,y=temperatureDifferenceTop0mvsBottom9m))+geom_line()+geom_hline(yintercept=-0.1,color="red")
ggplot(data=SensorData_derivedFill%>%mutate(ma_temp_avg_degC=forecast::ma(temp_2m_avg_degC,order=96*3,centre=TRUE))%>%filter(DateTime<=as.POSIXct("2017-12-14 00:00:00 EST")&DateTime>=as.POSIXct("2017-12-01 00:00:00 EST")),aes(x=ma_temp_avg_degC,y=stability_Jperm2))+geom_line()+geom_hline(yintercept=-0.1,color="red")
ggplot(data=SensorData_derivedFill%>%mutate(ma_temp_avg_degC=forecast::ma(temp_2m_avg_degC,order=96*3,centre=TRUE))%>%filter(DateTime<=as.POSIXct("2017-12-14 00:00:00 EST")&DateTime>=as.POSIXct("2017-12-01 00:00:00 EST")),aes(x=ma_temp_avg_degC,y=station_pressure_avg_mbar))+geom_line()
SensorData_derivedFill%>%mutate(ma_temp_avg_degC=forecast::ma(temp_2m_avg_degC,order=96*3,centre=TRUE))%>%filter(DateTime<=as.POSIXct("2017-12-14 00:00:00 EST")&DateTime>=as.POSIXct("2017-12-01 00:00:00 EST"))%>%filter(temperatureDifferenceTop0mvsBottom9m<(-0.1))%>%dplyr::select(DateTime,temperatureDifferenceTop0mvsBottom9m,stability_Jperm2,ma_temp_avg_degC)

#Create Z-scores for the met data and response variables####
#Response variables: "stability_Jperm2","buoyancyfrequency_1_s2","temperatureDifferenceTop0mvsBottom9m"
#Met predictor variables: temp_2m_avg_degC, relative_humidity_avg_percent, precip_local_mm, wind_speed_prop_avg_mps, solar_insolation_avg_Wpm2, station_pressure_avg_mbar

#Convert relevant columns to long data####
SensorData_derivedFill_long<-SensorData_derivedFill%>%dplyr::select(DateTime,
                                                                    stability_Jperm2,
                                                                    buoyancyfrequency_1_s2,
                                                                    temperatureDifferenceTop0mvsBottom9m,
                                                                    temp_2m_avg_degC, relative_humidity_avg_percent, precip_local_mm, wind_speed_prop_avg_mps, solar_insolation_avg_Wpm2, station_pressure_avg_mbar)%>%
  group_by(DateTime)%>%
  pivot_longer(-DateTime, 
               names_to = "variable",                                                         
               values_to = "value")

# convert to narrow format tidy, calculate means, sds, and zScores
SensorData_derivedFill_long_zscore <- SensorData_derivedFill_long %>% 
  group_by(variable) %>% 
  summarise(avg = mean(value,na.rm=TRUE), sd = sd(value,na.rm=TRUE)) %>%
  full_join(SensorData_derivedFill_long,.) %>% 
  mutate(.,zScore = (value - avg) / sd)

##*Graph the response and predictor variables as z-scores####
ggplot(data=SensorData_derivedFill_long_zscore%>%filter(variable=="stability_Jperm2"|variable=="buoyancyfrequency_1_s2"|variable=="temperatureDifferenceTop0mvsBottom9m"),aes(x=DateTime,y=zScore,color=variable))+geom_line()+scale_y_continuous(limits=c(-2,3))
#*Graph the predictor variables####
ggplot(data=SensorData_derivedFill_long_zscore%>%filter(!(variable=="stability_Jperm2"|variable=="buoyancyfrequency_1_s2"|variable=="temperatureDifferenceTop0mvsBottom9m")),aes(x=DateTime,y=zScore,color=variable))+
  geom_point()+
  scale_y_continuous(limits=c(-2,3))

# Graph some of the response variables for 2016####
lims_2016 <- as.POSIXct(strptime(c("2016-12-01 19:00", "2016-12-30 20:00"), 
                                 format = "%Y-%m-%d %H:%M"))

#*Graph the predictor variables####
ggplot(data=SensorData_derivedFill_long_zscore,aes(x=DateTime,y=zScore,color=variable))+
  geom_point()+
  scale_y_continuous(limits=c(-2,3))+
  facet_wrap(~variable)+
  geom_smooth(method="loess",color="black")+
  scale_x_datetime(limits=lims_2016)+
  geom_vline(data=segmented2016_results_stability%>%filter(optimal==TRUE),aes(xintercept=ymd_hms(date_bp)),color=color_bp)


# Graph some of the response variables for 2016####
lims_2017 <- as.POSIXct(strptime(c("2017-12-01 19:00", "2017-12-30 20:00"), 
                                 format = "%Y-%m-%d %H:%M"))


#*Graph the predictor variables####
ggplot(data=SensorData_derivedFill_long_zscore,aes(x=DateTime,y=zScore,color=variable))+
  geom_point()+
  scale_y_continuous(limits=c(-2,3))+
  facet_wrap(~variable)+
  geom_smooth(method="loess",color="black")+
  #geom_line(aes(y=forecast::ma(zScore, order=24*4*10, centre=TRUE)),color="black") +
  scale_x_datetime(limits=lims_2017)+
  geom_vline(data=segmented2017_results_stability%>%filter(optimal==TRUE),aes(xintercept=ymd_hms(date_bp)),color=color_bp)



####day/night daily###############################
#Summarize all columns that are numeric for the mean by day, sunset to sunset####
dailyDayNight_numeric<-SensorData_derivedFill%>%group_by(day_count,DayOrNight)%>%
  summarise_if(is.numeric, mean, na.rm = TRUE)
#SUmmarize all date columns for the mean by day, sunset to sunset####
dailyDayNight_date<-SensorData_derivedFill%>%group_by(day_count,DayOrNight)%>%
  summarise_if(is.POSIXct, mean, na.rm = TRUE)

#Merge those together for a daily data frame####
dailyDayOrNightSensorData_derivedFill<-left_join(dailyDayNight_date,dailyDayNight_numeric,by=c("day_count","DayOrNight"))

#Plot some daily values of schmidt stability####
#*Add in the ice on/off phenology and ice cover percentage####
ggplot(data=dailyDayOrNightSensorData_derivedFill,aes(x=DateTime,y=stability_Jperm2,color=DayOrNight))+geom_line()+
  geom_point(aes(y=IceCover_Percent/10))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceIn_1_date)))+
  geom_vline(data=IceOnIceOff,aes(xintercept=as.POSIXct(IceOut_1_date)))+
  scale_y_continuous(limits=c(-0.5,10.5),breaks=c(0,2.5,5,7.5,10),sec.axis=sec_axis(~.*10,name="IceCover_percent",breaks=c(0,25,50,75,100)))


