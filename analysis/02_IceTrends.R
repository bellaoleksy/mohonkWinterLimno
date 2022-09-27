


#Run the main script to bring in all data and functions####
# source('00_main.R')MohonkIceWeather
source('analysis/01_Isotherm.R') #this script already sources 00_main.R


#Load libraries 
if(!require(huxtable)){install.packages("huxtable")}
if(!require(magrittr)){install.packages("magrittr")}
if(!require(forecast)){install.packages("forecast")}
if(!require(officer)){install.packages("officer")}
if(!require(flextable)){install.packages("flextable")}
if(!require(egg)){install.packages("egg")} #tag_facet ftn
if(!require(viridis)){install.packages("viridis")} 
if(!require(lubridate)){install.packages("lubridate")} 
if(!require(viridis)){install.packages("viridis")} 
if(!require(tidyverse)){install.packages("tidyverse")} 
if(!require(scales)){install.packages("scales")} 
if(!require(urca)){install.packages("urca")} 
if(!require(fracdiff)){install.packages("fracdiff")} 
if(!require(tseries)){install.packages("tseries")} 
if(!require(Hmisc)){install.packages("Hmisc")} 
if(!require(GGally)){install.packages("GGally")} 
if(!require(ggcorrplot)){install.packages("ggcorrplot")} 
if(!require(patchwork)){install.packages("patchwork")} 
if(!require(ggpubr)){install.packages("ggpubr")} 
if(!require(ggthemes)){install.packages("ggthemes")} 
if(!require(mgcv)){install.packages("mgcv")} 
if(!require(scam)){install.packages("scam")} 
if(!require(cowplot)){install.packages("cowplot")} 
if(!require(grid)){install.packages("grid")} 
if(!require(schoenberg)){install.packages("schoenberg")} 
if(!require(nlme)){install.packages("nlme")} 
if(!require(gratia)){install.packages("gratia")} 
if(!require(itsadug)){install.packages("itsadug")} 
if(!require(visreg)){install.packages("visreg")} 
if(!require(visreg)){install.packages("huxtable")} 


library(huxtable) #Pretty tables
library(magrittr)
library(forecast)
library(officer) #exporting pretty tables to word
library(flextable) #exporting pretty tables to word
library(egg)
library(viridis)
library(lubridate)
library(tidyverse)
library(scales)
library(urca)
library(fracdiff)
library(tseries)
library(Hmisc, exclude="summarize") #correlation matrices
library(GGally)
library(ggcorrplot)
library(patchwork)
library(ggpubr) #for ggarrange()
library(ggthemes)
library(mgcv)
library(scam)
library(cowplot)
library(grid) # for unit.pmax(), unit.list()
library(schoenberg)
library(nlme)
library(gratia)
library(itsadug)
library(visreg)
library(huxtable)


# Set theme ---------------------------------------------------------------


theme_MS <- function () { 
  theme_base(base_size=10) %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="white", colour=NA, size=1.0),
      plot.title=element_text(face="plain",hjust=0.5),
      plot.subtitle = element_text(color="dimgrey", hjust=0, size=10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      strip.text.y = element_text(size=10, angle=270),
      strip.text.x = element_text(size=10),
      panel.spacing=grid::unit(0,"lines"),
      axis.ticks.length = unit(0.1, "cm")
    )
}

# theme_MS <- function () {
#   theme_base(base_size=8) %+replace%
#     theme(
#       panel.background  = element_blank(),
#       plot.background = element_rect(fill="white", colour=NA, size=1.0),
#       plot.title=element_text(face="plain",hjust=0.5),
#       plot.subtitle = element_text(color="dimgrey", hjust=0, size=8),
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       strip.background = element_blank(),
#       strip.text.y = element_text(size=8, angle=270),
#       strip.text.x = element_text(size=8),
#       panel.spacing=grid::unit(0,"lines"),
#       axis.ticks.length = unit(0.1, "cm")
#     )
# }


theme_set(theme_MS())


#annotate panel letters inside plot
panelLetter.normal <- data.frame(
  xpos = c(-Inf),
  ypos =  c(Inf),
  hjustvar = c(-0.5) ,
  vjustvar = c(1.5))




# Correlations of winter predictor variables -------------------------------------------
## Here I am looking at the massive list of potential predictors of ice-in or ice-out and seeing which ones are highly correlated (>0.7)

res2 <- rcorr(as.matrix(MohonkIceWeather[,7:ncol(MohonkIceWeather)]))
res2


#Create dataframe of pearson r and p-values
MohonkIceWeather_correlations<-flattenCorrMatrix(res2$r, res2$P)

#In an effort to limit the number of covariates, here I am looking at all correlations > 0.7
#and then below making a choice on which of the pair I should keep.
MohonkIceWeather_correlations_trim <- MohonkIceWeather_correlations %>%
  filter(abs(cor) > 0.7) %>%
  arrange(desc(cor)) 

head(MohonkIceWeather_correlations_trim)

#Look for potentially spurious correlations
res3 <- rcorr(as.matrix(MohonkIceWeather[,3:ncol(MohonkIceWeather)]))
# res3 <- rcorr(as.matrix(MohonkIceWeather[,3:ncol(MohonkIceWeather)]))
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
jpeg(filename = 'figures/FigureSX.top10_correlations.jpg',
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
                                         ifelse(column=="cumMeanDailyT_Feb", "cumul. mean daily temp. Feb",""))))))))))))))))))))) %>%
  mutate(name=paste(row, "-", column)) %>%
  ggplot(aes(x=cor_abs,y=fct_reorder(column, cor_abs, .desc = FALSE), fill=cor_abs))+
  geom_bar(stat="identity", width=0.75, color="black")+
  scale_fill_gradient2(low = "white", mid="darkolivegreen2", high = "darkseagreen4", midpoint=0.5, na.value = NA) +
  geom_text(aes(label=round(cor_abs,1)),color="black",size=3,position=position_stack(vjust=0.5)) +
  labs(y="Pairs",
       x="Correlation coefficient")+
  scale_x_continuous(limits = c(0, 1, breaks = seq(0, 1, by = 0.25)))+
  facet_wrap(~row, scales="free", nrow=2)+
  theme(legend.position="none")

dev.off()



#Export table of top 10 correlations
# write_csv(MohonkIce_top10, "data/exported/MohonkIce_CorrMatrix.csv")


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


#Just out of curiosity, is there any relationship between days since turnover and IceInDOY?
AnnualData %>%
  mutate(turnoverToIceIn_days=IceInDayofYear-EndOfStratification_Day) %>%
  select(Year,turnoverToIceIn_days,IceInDayofYear) %>%
  ggplot(aes(y=turnoverToIceIn_days,x=Year))+
  geom_point(size=3, shape=21)+
  geom_line(size=0.5)+
  xlab("Year")+
  geom_smooth(method="lm",color="grey50", size=0.5)




# ~~ Summary stats for manuscript --------------------------------------------


# >> Dates of ice on, off, etc ----------------------------------

#Earliest year
min(MohonkIce$Year)

#Latest year
max(MohonkIce$Year)


#Earliest ice-on
as.Date(min(MohonkIceWeather$IceInDayofYear_fed, na.rm=TRUE)+274, origin="2014-01-02")
#Dec 5

#Latest ice-on
as.Date(max(MohonkIceWeather$IceInDayofYear_fed, na.rm=TRUE)-92, origin="2014-01-02")
#February 8


#Median ice-on
as.Date(median(MohonkIceWeather$IceInDayofYear_fed, na.rm=TRUE)-92, origin="2014-01-02")
#February 8



#Earliest ice-off
as.Date(min(MohonkIceWeather$IceOutDayofYear, na.rm=TRUE), origin="2014-01-02")
#March 11

#Latest ice-off
as.Date(max(MohonkIceWeather$IceOutDayofYear, na.rm=TRUE), origin="2014-01-02")
#May 2

#Median ice-off
as.Date(median(MohonkIceWeather$IceOutDayofYear, na.rm=TRUE), origin="2014-01-02")
#April 9

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
  geom_line(size=1)+
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
#but first derivative plot shows that it is not accelerating at least

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


# ~~FIGURE 1~~ Ice in phenology timeseries ------------------------------------------------------------
# Sen slope trends in ice phenology --------------------------------------------------
#Sen slopes using DCR defined function that is drawn from zyp and trend functions#
#No longer need to impute values or use zyp.sen
#Deals with NA values
#create new df with variable name, sen slope, intercept, z, n, pvalue

#Runs Theil Sen's slopes on each variable
#Includes summary stats of significant slopes
Ice.SensSlopeSummary<-
  MohonkIce %>%
  select(Year, IceOutDayofYear,LengthOfIceCover_days,IceInDayofYear_fed) %>%
  pivot_longer(-1) %>%
  group_by(name)%>%
  dplyr::summarize(Sens_Slope=MTCC.sensSlope(x=Year,y=value)$coefficients["Year"],
                   Sens_Intercept=MTCC.sensSlope(x=Year,y=value)$coefficients["Intercept"],
                   Sens_pval=MTCC.sensSlope(x=Year,y=value)$pval,
                   Sens_z_stat=MTCC.sensSlope(x=Year,y=value)$z_stat,
                   Sens_n=MTCC.sensSlope(x=Year,y=value)$n) %>%
  mutate(Significance=ifelse(Sens_pval<0.05,"*","NS"))


#Visualize
glimpse(MohonkIce)

#**Phenology of stratification plot####
#Immitate the figure from our GRL paper
#Create yhat for date of stratification onset
MohonkIce.Predicted <- MohonkIce %>% dplyr::select(Year) %>%
  mutate(
    IceOutDayofYear_fed_yhat = Year * as.numeric(
      Ice.SensSlopeSummary %>% filter(name == "IceOutDayofYear") %>% dplyr::select(Sens_Slope)
    ) + as.numeric(
      Ice.SensSlopeSummary %>% filter(name == "IceOutDayofYear") %>% dplyr::select(Sens_Intercept)
    )
  ) %>%
  mutate(
    LengthOfIceCover_days_yhat = Year * as.numeric(
      Ice.SensSlopeSummary %>% filter(name == "LengthOfIceCover_days") %>% dplyr::select(Sens_Slope)
    ) + as.numeric(
      Ice.SensSlopeSummary %>% filter(name == "LengthOfIceCover_days") %>% dplyr::select(Sens_Intercept)
    )
  ) %>%
  mutate(
    IceInDayofYear_fed_yhat = Year * as.numeric(
      Ice.SensSlopeSummary %>% filter(name == "IceInDayofYear_fed") %>% dplyr::select(Sens_Slope)
    ) + as.numeric(
      Ice.SensSlopeSummary %>% filter(name == "IceInDayofYear_fed") %>% dplyr::select(Sens_Intercept)
    )
  )



# ggplot() +
#   geom_segment(
#     data = MohonkIce,
#     aes(
#       x = Year,
#       xend = Year,
#       y = IceInDayofYear_fed,
#       yend = IceOutDayofYear_fed,
#       col = LengthOfIceCover_days
#     )
#   ) +
#   # col="grey")+
#   geom_point(
#     data = MohonkIce,
#     aes(x = Year, y = IceInDayofYear_fed, fill = LengthOfIceCover_days),
#     shape = 21,
#     color = "black",
#     size = 1
#   ) +
#   geom_line(
#     data = MohonkIce.Predicted,
#     aes(x = Year, y = IceInDayofYear_fed_yhat),
#     color = "black",
#     lty = 1
#   ) +
#   geom_point(
#     data = MohonkIce,
#     aes(x = Year,
#         y = IceOutDayofYear_fed,
#         fill = LengthOfIceCover_days),
#     shape = 21,
#     color = "black",
#     size = 1
#   ) +
#   scale_color_continuous(high = "green", low = "red",
#                          name = "Ice duration\n(days)") +
#   scale_fill_continuous(high = "green", low = "red",
#                         name = "Ice duration\n(days)") +
#   scale_y_continuous(lim = c(50, 200),
#                      breaks = seq(50, 250, by = 25)) +
#   scale_x_continuous(limit = c(1930, 2020),
#                      breaks = seq(1930, 2020, by = 10)) +
#   theme_MS() +
#   theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(colour = "black"),
#     panel.border = element_rect(
#       fill = NA,
#       colour = "black",
#       size = 1
#     ),
#     axis.text.x = element_text(color = "black"),
#     axis.text.y = element_text(color = "black"),
#     axis.ticks = element_line(color = "black")
#   ) +
#   xlab("Year") +
#   ylab("Days since Oct 1 (beginning of water-year)")
# ggsave(
#   "figures/Fig1.IcePhenology.jpg",
#   width = 80,
#   height = 60,
#   units = "mm",
#   dpi = 300
# )
# 


#DCR requested date labels for the y-axis... so will have to do something crafty here. 

MohonkIce.Predicted.test <- MohonkIce.Predicted %>%
  left_join(., MohonkIce) %>%
  mutate(IceInDayofYear_fed_yhat_DOY = case_when(IceInDayofYear_fed_yhat< 93 ~ IceInDayofYear_fed_yhat + 274,
                                                 IceInDayofYear_fed_yhat>=93 ~ IceInDayofYear_fed_yhat - 92),
         IceInDayofYear_yhat=as.Date(IceInDayofYear_fed_yhat_DOY, origin="2014-01-02"),
         IceInDate_month=month(IceInDate),
         IceInDate_day=day(IceInDate),
         IceInDate_newdate=case_when(IceInDate_month %in% c("10","11","12") ~ ymd(paste("2014", IceInDate_month, IceInDate_day, sep = "-")),
                                     IceInDate_month %in% c("1","2","3","4") ~ ymd(paste("2015", IceInDate_month, IceInDate_day, sep = "-"))),
         IceOutDate_month=month(IceOutDate),
         IceOutDate_day=day(IceOutDate),
         IceOutDate_newdate=case_when(IceOutDate_month %in% c("10","11","12") ~ ymd(paste("2014", IceOutDate_month, IceOutDate_day, sep = "-")),
                                       IceOutDate_month %in% c("1","2","3","4") ~ ymd(paste("2015", IceOutDate_month, IceOutDate_day, sep = "-"))),
         IceInDayofYear_yhat_month=month(IceInDayofYear_yhat),
         IceInDayofYear_yhat_day=day(IceInDayofYear_yhat),
         IceInDayofYear_yhat_newdate=case_when(IceInDayofYear_yhat_month %in% c("10","11","12") ~ ymd(paste("2014", IceInDayofYear_yhat_month, IceInDayofYear_yhat_day, sep = "-")),
                                               IceInDayofYear_yhat_month %in% c("1","2","3","4") ~ ymd(paste("2015", IceInDayofYear_yhat_month, IceInDayofYear_yhat_day, sep = "-"))))



jpeg(filename = 'figures/Fig1.IcePhenology_withDates.jpg',
    width = 3.14,
    height = 2.5,
    res = 300,
    units = 'in')

ggplot() +
  geom_segment(
    data = MohonkIce.Predicted.test,
    aes(
      x = Year,
      xend = Year,
      y = IceInDate_newdate,
      yend = IceOutDate_newdate,
      col = LengthOfIceCover_days
    )
  ) +
  # col="grey")+
  geom_point(
    data = MohonkIce.Predicted.test,
    aes(x = Year, y = IceInDate_newdate, fill = LengthOfIceCover_days),
    shape = 21,
    color = "black",
    size = 1
  ) +
  geom_smooth(
    data = MohonkIce.Predicted.test,
    aes(x = Year, y = IceInDayofYear_yhat_newdate),
    color = "black",
    lty = 1,
    size= 0.5
  ) +
  geom_point(
    data = MohonkIce.Predicted.test,
    aes(x = Year,
        y = IceOutDate_newdate,
        fill = LengthOfIceCover_days),
    shape = 21,
    color = "black",
    size = 1
  ) +
  scale_color_continuous(high = "green", low = "red",
                         name = "Ice duration\n(days)") +
  scale_fill_continuous(high = "green", low = "red",
                        name = "Ice duration\n(days)") +
  # scale_y_continuous(lim = c(50, 200),
  #                    breaks = seq(50, 250, by = 25)) +
  scale_x_continuous(limit = c(1932, 2022),
                     breaks = seq(1940, 2020, by = 20)) +
  # scale_y_continuous(
  #   trans = c("date", "reverse2")
  # )
  scale_y_date(date_breaks = "1 month", date_minor_breaks = "1 week",
               date_labels = "%d-%b")+
  theme_MS() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(
      fill = NA,
      colour = "black",
      size = 1
    ),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black", angle=90, hjust=0.5),
    axis.ticks = element_line(color = "black")
  ) +
  xlab("Year") +
  ylab("Ice phenology date")

dev.off()

ggsave(
  "figures/Fig1.IcePhenology_withDates_inches.jpg",
  width = 3.14,
  height = 2.5,
  units = "in",
  dpi = 600
)

# Fitting GAMs for iceOnDOY_fed -------------------------------------------

hist(MohonkIceWeather$IceInDayofYear_fed)

MohonkIceWeather <- MohonkIceWeather %>%
  left_join(., Isotherm_WaterYear_dates_IceIn %>%
              select(WaterYear,isotherm_TempMax_degC_17_days_0_degC_WaterYear_date),
            by = c("Year"="WaterYear")) %>%
  left_join(., Isotherm_WaterYear_dates_IceOut %>%
              select(WaterYear,isotherm_TempMean_degC_29_days_4_degC_WaterYear_date),
            by = c("Year"="WaterYear")) 

### IceInDayofYear_fed~nDaysMeanBelowZero_OctNovDec
modIceOn1 <- gam(IceInDayofYear_fed ~  s(nDaysMeanBelowZero_OctNovDec),
                 family=Gamma(link="log"),
                 data = MohonkIceWeather,
                 # correlation = corCAR1(form = ~ Year),
                 method = "REML")
summary(modIceOn1)
# summary(modIceOn1$gam)
#Fit improves substantially over null model

draw(modIceOn1, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals

plot(modIceOn1,
     shift = coef(modIceOn1)[1],
     pages =1, all.terms=TRUE)


### IceInDayofYear_fed~cumMeanDailyT_OctNovDec
modIceOn2 <- gam(IceInDayofYear_fed ~ s(cumMeanDailyT_OctNovDec) ,
                 family=Gamma(link="log"),
                 data = MohonkIceWeather,
                 # correlation = corCAR1(form = ~ Year),
                 method = "REML")
summary(modIceOn2)
# summary(modIceOn2$gam)
#Fit improves substantially over null model

draw(modIceOn2, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals

plot(modIceOn2,
     shift = coef(modIceOn2)[1],
     pages =1, all.terms=TRUE)


### IceInDayofYear_fed~Sept + Oct + Nov + Dec
modIceOn3 <- gam(IceInDayofYear_fed ~  s(cumMeanDailyT_Sep) + s(cumMeanDailyT_Oct) + s(cumMeanDailyT_Nov) + s(cumMeanDailyT_Dec),
                 family=Gamma(link="log"),
                 data = MohonkIceWeather,
                 # correlation = corCAR1(form = ~ Year),
                 method = "REML")
summary(modIceOn3)
# summary(modIceOn3$gam)
#Fit improves substantially over null model

plot(modIceOn3,
     shift = coef(modIceOn3)[1],
     pages =1, all.terms=TRUE)

### IceInDayofYear_fed~Nov + Dec
### Last model contains only Nov + Dec since individually Sep and Oct were not statistically significantly. 
modIceOn4 <- gam(IceInDayofYear_fed ~  s(cumMeanDailyT_Nov) + s(cumMeanDailyT_Dec),
                 family=Gamma(link="log"),
                 data = MohonkIceWeather,
                 # correlation = corCAR1(form = ~ Year),
                 method = "REML")
summary(modIceOn4)
#Fit improves substantially over null model

  
  plot(modIceOn4,
     shift = coef(modIceOn4)[1],
     pages =1, all.terms=TRUE)

appraise(modIceOn4)

gam.check(modIceOn4)
#Suggests that default k values are fine

draw(modIceOn4, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals
qq_plot(modIceOn4)
draw(modIceOn4,
     residuals = TRUE,
     scales="free",
     ci_level=0.95,
     rug=FALSE,
     smooth_col="navyblue",
     resid_col="navyblue")

### IceInDayofYear_fed~isotherm_TempMax_degC_17_days_0_degC_WaterYear_date
modIceOn5 <- gam(IceInDayofYear_fed ~  s(isotherm_TempMax_degC_17_days_0_degC_WaterYear_date),
                 family=Gamma(link="log"),
                 data = MohonkIceWeather,
                 # correlation = corCAR1(form = ~ Year),
                 method = "REML")
summary(modIceOn5)
#Fit improves substantially over null model

plot(modIceOn5,
     shift = coef(modIceOn5)[1],
     pages =1, all.terms=TRUE)

appraise(modIceOn5)

gam.check(modIceOn5)
#Suggests that default k values are fine

draw(modIceOn5, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals
qq_plot(modIceOn5)
draw(modIceOn5,
     residuals = TRUE,
     scales="free",
     ci_level=0.95,
     rug=FALSE,
     smooth_col="navyblue",
     resid_col="navyblue")


### IceInDayofYear_fed~isotherm_TempMax_degC_17_days_0_degC_WaterYear_date+cumMeanDailyT_Nov+cumMeanDailyT_Dec
#but before running this particular model, how correlated are these variables to each other? 
MohonkIceWeather %>%
  select(isotherm_TempMax_degC_17_days_0_degC_WaterYear_date, cumMeanDailyT_Nov, cumMeanDailyT_Dec) %>%
  ggpairs() 
#cumMeanDailyT_Dec & isotherm_TempMax_degC_17_days_0_degC_WaterYear_date are highly correlated (0.733)
#so let's just keep Nov and see if how that fit looks

modIceOn6 <- gam(IceInDayofYear_fed ~  s(isotherm_TempMax_degC_17_days_0_degC_WaterYear_date) + s(cumMeanDailyT_Nov),
                 family=Gamma(link="log"),
                 data = MohonkIceWeather,
                 # correlation = corCAR1(form = ~ Year),
                 method = "REML")
summary(modIceOn6)
#Fit improves substantially over null model

plot(modIceOn6,
     shift = coef(modIceOn6)[1],
     pages =1, all.terms=TRUE)

appraise(modIceOn6)

gam.check(modIceOn6)
#Suggests that default k values are fine

draw(modIceOn6, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals
qq_plot(modIceOn6)
draw(modIceOn6,
     residuals = TRUE,
     scales="free",
     ci_level=0.95,
     rug=FALSE,
     smooth_col="navyblue",
     resid_col="navyblue")


#How to compare the fits of multiple GAMs models? 
#Would be worth digging into more but found this as a solution:
#https://rdrr.io/cran/itsadug/man/compareML.html
#From the documentation: "This method is preferred over other functions such as AIC for models that include an AR1 model or random effects (especially nonlinear random smooths using bs='fs'). CompareML also reports the AIC difference, but that value should be treated with care."

compareML(modIceOn3, modIceOn4) #Very similar, mod4 might slightly better than 3
compareML(modIceOn2, modIceOn4)
compareML(modIceOn1, modIceOn4)
compareML(modIceOn5, modIceOn4) #model 5 preferred
compareML(modIceOn5, modIceOn6) #model 6 has lower AIC

visreg::visreg2d(modIceOn6, xvar='isotherm_TempMax_degC_17_days_0_degC_WaterYear_date', yvar='cumMeanDailyT_Nov', scale='response')

# > Final Ice On model ----------------------------------------------------

#Top 3 models 


#Effective degrees of freedom (as a metric for model complexity)
sum(influence(modIceOn4))
sum(influence(modIceOn5))
sum(influence(modIceOn6))

#terms
modIceOn4$terms
modIceOn5$terms
modIceOn6$terms

#AIC
modIceOn4$aic
modIceOn5$aic
modIceOn6$aic

#Dev explained
summary(modIceOn4)$dev.expl
summary(modIceOn5)$dev.expl
summary(modIceOn6)$dev.expl





#Final variables for paper--
summary(modIceOn6)



### Panel A -- Ice On vs. cumMeanDailyT_Dec
new_data <-
  with(MohonkIceWeather,
       expand.grid(
         isotherm_TempMax_degC_17_days_0_degC_WaterYear_date = seq(
           min(isotherm_TempMax_degC_17_days_0_degC_WaterYear_date, na.rm = TRUE),
           max(isotherm_TempMax_degC_17_days_0_degC_WaterYear_date, na.rm =
                 TRUE),
           length = 200
         ),
         cumMeanDailyT_Nov = median(cumMeanDailyT_Nov, na.rm =
                                      TRUE)
       ))

ilink <- family(modIceOn6)$linkinv
pred_isotherm <- predict(modIceOn6, new_data, type = "link", se.fit = TRUE)
pred_isotherm <- cbind(pred_isotherm, new_data)
pred_isotherm <- transform(pred_isotherm, lwr_ci = ilink(fit - (2 * se.fit)),
                           upr_ci = ilink(fit + (2 * se.fit)),
                           fitted = ilink(fit))
pred_isotherm <- pred_isotherm %>%
  select(isotherm_TempMax_degC_17_days_0_degC_WaterYear_date, lwr_ci:fitted) %>%
  rename(lwr_ci_isotherm = lwr_ci,
         upr_ci_isotherm = upr_ci,
         fitted_isotherm = fitted)

#Modify axis labels: fed DOY -> dates
labels_IceOnDayofYear_fed<-c(50,70,90,110,130)
labels_IsoMax_fed<-c(70,90,110)
#To figure out conversion of FED DOY to Date use something like: as.Date(274+110, origin="2014-01-02")


IceOn_isotherm<-
  ggplot(pred_isotherm, aes(x = isotherm_TempMax_degC_17_days_0_degC_WaterYear_date, y = fitted_isotherm)) +
  geom_ribbon(aes(ymin = lwr_ci_isotherm, ymax = upr_ci_isotherm), alpha = 0.2) +
  geom_line() +
  geom_point(data=MohonkIceWeather, aes(x=isotherm_TempMax_degC_17_days_0_degC_WaterYear_date,
                                        y=IceInDayofYear_fed))+
  labs(x=expression(Iso["max,"]["17day,"]["0°C"]),
    # x="Isotherm Formula: TempMax in degC, 17 day window, 0 degC threshold",
       y="Ice-on date")+
  scale_y_continuous(breaks=labels_IceOnDayofYear_fed,labels=c("22-Nov","12-Dec","01-Jan","21-Jan","10-Feb"),limits=c(50,130))+
  scale_x_continuous(breaks=labels_IsoMax_fed,labels=c("12-Dec","01-Jan","21-Jan"),limits=c(70,120))+
    theme(plot.margin=unit(c(0.5,0,0.5,0.5), "lines"),
        axis.text.y = element_text(angle = 90, hjust=0.5)) +
  geom_text(data=panelLetter.normal,
            aes(x=xpos,
                y=ypos,
                hjust=hjustvar,
                vjust=vjustvar,
                label="a",
                fontface="bold"))

### Panel B -- Ice On vs. cumMeanDailyT_Nov
new_data <-
  with(MohonkIceWeather,
       expand.grid(
         cumMeanDailyT_Nov = seq(
           min(cumMeanDailyT_Nov, na.rm = TRUE),
           max(cumMeanDailyT_Nov, na.rm =
                 TRUE),
           length = 200
         ),
         isotherm_TempMax_degC_17_days_0_degC_WaterYear_date = median(isotherm_TempMax_degC_17_days_0_degC_WaterYear_date, na.rm =
                                                                        TRUE)
       ))

ilink <- family(modIceOn6)$linkinv
pred_Nov <- predict(modIceOn6, new_data, type = "link", se.fit = TRUE)
pred_Nov <- cbind(pred_Nov, new_data)
pred_Nov <- transform(pred_Nov, lwr_ci = ilink(fit - (2 * se.fit)),
                      upr_ci = ilink(fit + (2 * se.fit)),
                      fitted = ilink(fit))
pred_Nov <- pred_Nov %>%
  select(cumMeanDailyT_Nov, lwr_ci:fitted) %>%
  rename(lwr_ci_Nov = lwr_ci,
         upr_ci_Nov = upr_ci,
         fitted_Nov = fitted)


IceOn_CumuNov<-ggplot(pred_Nov, aes(x = cumMeanDailyT_Nov, y = fitted_Nov)) +
  geom_ribbon(aes(ymin = lwr_ci_Nov, ymax = upr_ci_Nov), alpha = 0.2) +
  geom_line() +
  geom_point(data=MohonkIceWeather, aes(x=cumMeanDailyT_Nov,
                                        y=IceInDayofYear_fed))+
  labs(x="Nov. cumulative\nmean daily temp. (°C)",
       y="Ice on (days since Oct 1)")+
  # scale_y_continuous(breaks = seq(50, 130, by = 20) )+
  # coord_cartesian(ylim = c(50, 130), expand = TRUE) +
  scale_y_continuous(breaks=labels_IceOnDayofYear_fed,labels=c("22-Nov","12-Dec","01-Jan","21-Jan","10-Feb"),limits=c(50,130))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0.5,0,0.5,0), "lines"),
        axis.ticks.length.y = unit(0, "pt"))+
  geom_text(data=panelLetter.normal,
            aes(x=xpos,
                y=ypos,
                hjust=hjustvar,
                vjust=vjustvar,
                label="b",
                fontface="bold"))


# ~~ FIGURE 2.  Ice On Predictors -----------------------------------------



Row1a<-(IceOn_isotherm+IceOn_CumuNov)
Row1a


ggsave("figures/Figure2.GamPredictions_IceOn.png", plot=Row1a, width=8, height=4,units="in", dpi=300)

ggsave("figures/Figure2.GamPredictions_IceOn.jpg", plot=Row1a, width=180, height=120,units="mm", dpi=300)




# Fitting GAMs for IceOutDayofYear -------------------------------------------

#Distribution of y
hist(MohonkIceWeather$IceOutDayofYear)

### I added Family Gamma here for how errors should respond
modIceOut0 <- gam(IceOutDayofYear ~ s(Year),
                  # family=Gamma(link="log"),
                  data = MohonkIceWeather,
                  correlation = corCAR1(form = ~ Year),
                  method = "REML")
summary(modIceOut0)
## summary object
# modIceOut0_S <- summary(modIceOut0$gam)
# modIceOut0_S #Gives you the P values, degrees of freedom...

#PLOT Autocorrelation function of residuals from the additive model with AR(1) errors
ACF <- acf(resid(modIceOut0, type = "response"), plot = FALSE)
ACF <- setNames(data.frame(unclass(ACF)[c("acf", "lag")]), c("ACF","Lag"))
ggplot(ACF, aes(x = Lag, y = ACF)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = Lag, yend = 0))
#Suggests that an AR(1) model isn't necessary? 


###Since we're concerned with the response, include "response" in type of predict()
IceOffPred <- with(MohonkIce, data.frame(Year = seq(min(Year, na.rm=TRUE),
                                                    max(Year, na.rm=TRUE),
                                                    length.out = 200)))
IceOffPred <- cbind(IceOffPred, data.frame(predict(modIceOut0, IceOffPred,
                                                   type="response",
                                                   se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
IceOffPred <- transform(IceOffPred, upper = fit + (2 * se.fit),
                        lower = fit - (2 * se.fit))


# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "Year"
m1.d <- Deriv(modIceOut0)

m1.dci <- confint(m1.d, term = "Year")
m1.dsig <- signifD(IceOffPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

ylim <- with(IceOffPred, range(upper, lower, fit))
ylab <- 'Ice on DOY (water year)'

plot(fit ~ Year, data = IceOffPred, type = "n", ylab = ylab, ylim = ylim)
lines(fit ~ Year, data = IceOffPred)
lines(upper ~ Year, data = IceOffPred, lty = "dashed")
lines(lower ~ Year, data = IceOffPred, lty = "dashed")
lines(unlist(m1.dsig$incr) ~ Year, data = IceOffPred, col = "blue", lwd = 3)
lines(unlist(m1.dsig$decr) ~ Year, data = IceOffPred, col = "red", lwd = 3)
#No significant change in IceOut DOY, which we already knew from the sens slopes 

#But another way to visualize it is there would be a significant period of change if the error bar around
#the first derivative didn't overlap the horizontal black line.
plot.Deriv(m1.d)

#Plot Ice off DOY vs. year pretty
ggplot(IceOffPred,aes(x=Year,y=fit))+
  geom_point(data=MohonkIce,
             mapping=aes(x=Year, y=IceOutDayofYear), size=2.5, alpha=0.7) +
  geom_line(size=1)+
  geom_ribbon(aes(ymin = (lower), ymax = (upper), x = Year), alpha = 0.5, inherit.aes = FALSE) +
  labs(x="Year",y="Ice off (DOY)")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))
## BUT the fit is poor. Can we add in additional predictors to improve the model fit? 



### IceOutDayofYear~cumMeanDailyT_FebMarApr+cumSnow_FebMarApr
modIceOut1 <- gam(IceOutDayofYear ~  s(cumMeanDailyT_FebMarApr) + s(cumSnow_FebMarApr),
                  # family=Gamma(link="log"),
                  data = MohonkIceWeather,
                  # correlation = corCAR1(form = ~ Year),
                  method = "REML")
summary(modIceOut1)
# summary(modIceOut1$gam)
#Fit improves substantially over null model

draw(modIceOut1, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals


### IceOutDayofYear~cumMeanDailyT_FebMar+cumSnow_FebMar (same as 1, but exclude April)
modIceOut2 <- gam(IceOutDayofYear ~ s(cumMeanDailyT_FebMar) + s(cumSnow_FebMar) ,
                  # family=Gamma(link="log"),
                  data = MohonkIceWeather,
                  # correlation = corCAR1(form = ~ Year),
                  method = "REML")
summary(modIceOut2)
# summary(modIceOut2$gam)
# Fit is a bit better than modIceOut2, in terms of deviance explained

draw(modIceOut2, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals


### IceOutDayofYear ~ nDaysMinAboveZero_FebMar + cumSnow_FebMar
modIceOut3 <- gam(IceOutDayofYear ~  s(nDaysMinAboveZero_FebMar) + s(cumSnow_FebMar) ,
                  # family=Gamma(link="log"),
                  data = MohonkIceWeather,
                  # correlation = corCAR1(form = ~ Year),
                  method = "REML")
summary(modIceOut3)
# summary(modIceOut3$gam)

draw(modIceOut3, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals

### IceInDayofYear~ nDaysMinAboveZero_FebMar + cumSnow_FebMarApr
### Last model contains only Nov + Dec since individually Sep and Oct were not statistically significantly. 
modIceOut4 <- gam(IceOutDayofYear ~  s(nDaysMinAboveZero_FebMar) + s(cumSnow_FebMarApr),
                  # family=Gamma(link="log"),
                  data = MohonkIceWeather,
                  # correlation = corCAR1(form = ~ Year),
                  method = "REML")
summary(modIceOut4)
# modIceOut4_S<-summary(modIceOut4$gam)
# modIceOut4_S
#Fit improves if you include cumulative snowfall through April 

draw(modIceOut4, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals

### IceInDayofYear~ cumMeanDailyT_Feb + cumMeanDailyT_Mar + cumMeanDailyT_Apr + cumSnow_FebMarApr
### Last model contains only Nov + Dec since individually Sep and Oct were not statistically significantly. 
modIceOut5 <- gam(IceOutDayofYear ~  s(cumMeanDailyT_Feb) + s(cumMeanDailyT_Mar) + s(cumMeanDailyT_Apr) + s(cumSnow_FebMarApr),
                  family=Gamma(link="log"),
                  data = MohonkIceWeather,
                  # correlation = corCAR1(form = ~ Year),
                  method = "REML")
summary(modIceOut5)
#Looks like the best model fit yet! cumMeanDailyT_Apr marginally significant. 

draw(modIceOut5, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals

### IceInDayofYear~ cumMeanDailyT_Feb + cumMeanDailyT_Mar +  cumSnow_FebMarApr
### Last model contains only Nov + Dec since individually Sep and Oct were not statistically significantly. 
modIceOut6 <- gam(IceOutDayofYear ~  s(cumMeanDailyT_Feb) + s(cumMeanDailyT_Mar) + s(cumSnow_FebMarApr),
                  # family=Gamma(link="log"),
                  data = MohonkIceWeather,
                  # correlation = corCAR1(form = ~ Year),
                  method = "REML")
summary(modIceOut6)
#Shows that there is probably some benefit in keeping April in the model. 

draw(modIceOut6, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals


### IceInDayofYear~ cumMeanDailyT_Feb + cumMeanDailyT_Mar +  cumSnow_FebMarApr + IceInDayofYear_fed
### Identical to model 6 but includes IceInDayofYear_fed as a proxy for ice thickness...?



modIceOut7 <- gam(IceOutDayofYear ~  s(cumMeanDailyT_Feb) + s(cumMeanDailyT_Mar) + s(cumSnow_FebMarApr) 
                  + s(IceInDayofYear_fed),
                  # family=Gamma(link="log"),
                  data = MohonkIceWeather,
                  # correlation = corCAR1(form = ~ Year),
                  method = "REML")
summary(modIceOut7)


draw(modIceOut7, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals

### IceInDayofYear~ cumMeanDailyT_FebMar +  cumSnow_FebMarApr + LengthOfIceCover_days
### Identical to model 2 but includes length of ice cover as a proxy for ice thickness...?
modIceOut8 <- gam(IceOutDayofYear ~  s(cumMeanDailyT_FebMar) + 
                    s(cumSnow_FebMarApr) +
                    s(IceInDayofYear_fed),
                  # family=Gamma(link="log"),
                  data = MohonkIceWeather,
                  # correlation = corCAR1(form = ~ Year),
                  method = "REML")
summary(modIceOut8)


draw(modIceOut8, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals

gam.check(modIceOut8)
#Suggests that default k values are fine

### IceInDayofYear~ cumMeanDailyT_FebMar +  cumSnow_FebMarApr + LengthOfIceCover_days
### Identical to model 2 but includes length of ice cover as a proxy for ice thickness...?

#but before running this particular model, how correlated are these variables to each other? 
MohonkIceWeather %>%
  select(isotherm_TempMean_degC_29_days_4_degC_WaterYear_date, cumSnow_FebMarApr, cumMeanDailyT_Feb, cumMeanDailyT_Mar) %>%
  ggpairs() 
#Everything is correlated but it is really only isotherm_TempMean_degC_29_days_4_degC_WaterYear_date & cumMeanDailyT_Mar that are potentially problematic
# What is we use the identical model structure to model 7 but swap those two variables? 

modIceOut9 <- gam(IceOutDayofYear ~   s(cumMeanDailyT_Feb) + s(isotherm_TempMean_degC_29_days_4_degC_WaterYear_date) + s(cumSnow_FebMarApr) 
                  + s(IceInDayofYear_fed),
                  # family=Gamma(link="log"),
                  data = MohonkIceWeather,
                  # correlation = corCAR1(form = ~ Year),
                  method = "REML")
summary(modIceOut9)


draw(modIceOut9, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals

gam.check(modIceOut9)
#Suggests that default k values are fine


#Just for good measure, same as above but without IceInDOY_fed
modIceOut10 <- gam(IceOutDayofYear ~   s(cumMeanDailyT_Feb) + s(isotherm_TempMean_degC_29_days_4_degC_WaterYear_date) + s(cumSnow_FebMarApr),
                   # family=Gamma(link="log"),
                   data = MohonkIceWeather,
                   # correlation = corCAR1(form = ~ Year),
                   method = "REML")
summary(modIceOut10)


draw(modIceOut10, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals

gam.check(modIceOut10)
#Suggests that default k values are fine



modIceOut11 <- gam(IceOutDayofYear ~   s(isotherm_TempMean_degC_29_days_4_degC_WaterYear_date) ,
                   # family=Gamma(link="log"),
                   data = MohonkIceWeather,
                   # correlation = corCAR1(form = ~ Year),
                   method = "REML")
summary(modIceOut11)


draw(modIceOut10, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals

gam.check(modIceOut10)
#Suggests that default k values are fine


#How to compare the fits of multiple GAMs models? 
#Would be worth digging into more but found this as a solution:
#https://rdrr.io/cran/itsadug/man/compareML.html
#From the documentation: "This method is preferred over other functions such as AIC for models that include an AR1 model or random effects (especially nonlinear random smooths using bs='fs'). CompareML also reports the AIC difference, but that value should be treated with care."

compareML(modIceOut1, modIceOut2) #ModIceOut2 preferred
compareML(modIceOut2, modIceOut3) #ModIceOut2 preferred
compareML(modIceOut2, modIceOut4) #ModIceOut4 preferred
compareML(modIceOut5, modIceOut4) #ModIceOut4 preferred
compareML(modIceOut4, modIceOut6) #Basically a tie, but doesn't mod6 have higher dev explained?

modIceOut6_summary<- summary.gam(modIceOut6)
modIceOut4_summary<- summary.gam(modIceOut4)
modIceOut6_summary$dev.expl
modIceOut4_summary$dev.expl
#Mod6 explains more variation

compareML(modIceOut5, modIceOut6) #ModIceOut6 preferred
compareML(modIceOut7, modIceOut6) #ModIceOut7 preferred


compareML(modIceOut8, modIceOut7) #Basically a tie
modIceOut7_summary<- summary.gam(modIceOut7)
modIceOut8_summary<- summary.gam(modIceOut8)
modIceOut7_summary$dev.expl
modIceOut8_summary$dev.expl
#Mod7 explains more variation


compareML(modIceOut9, modIceOut7) #ModIceOut9 preferred
compareML(modIceOut9, modIceOut10) #ModIceOut9 has lower AIC. How does dev % compare?

modIceOut9_summary<- summary.gam(modIceOut9)
modIceOut10_summary<- summary.gam(modIceOut10)
modIceOut9_summary$dev.expl
modIceOut10_summary$dev.expl
#9 explains more variation



#Report gam smoothness estimates as variance components
variance_comp(modIceOut9, rescale=TRUE, coverage=0.95) 
variance_comp(modIceOut7, rescale=TRUE, coverage=0.95) 



# Model 6 compared to Model 2 has lower AIC and %dev explained is 75%
# Model 2 has a %dev explained of 73.4% and is more parsimonious, so I would lean toward using that instead. We can make the final call together, and also report all of them in a supplement. 

## Look at model diagnostics
appraise(modIceOut6)
appraise(modIceOut7)
appraise(modIceOut8)
appraise(modIceOut9)


## Look at the predictions a few different ways
visreg::visreg(modIceOut9, "isotherm_TempMean_degC_29_days_4_degC_WaterYear_date", "cumSnow_FebMarApr", gg=TRUE, ylab="Ice out DOY")
#Not entirely sure what the values on top mean, and it looks like not all the values are plotted?

mgcv::vis.gam(modIceOut9, view=c("cumSnow_FebMarApr","isotherm_TempMean_degC_29_days_4_degC_WaterYear_date"),
              plot.type="contour", color="cm", type="response")

vis.gam(modIceOut9, view=c("cumSnow_FebMarApr","isotherm_TempMean_degC_29_days_4_degC_WaterYear_date"),
        theta= 24, type="response",
        ticktype="detailed",
        color="cm",
        zlab="\nIce Out DOY (Julian day)",
        xlab="\nCumulative snowfall Feb-Mar",
        ylab="\n29-day isotherm > 4 deg") #Adjust theta to get a different view


# > Final Ice Out model ---------------------------------------------------



#Effective degrees of freedom (as a metric for model complexity)
sum(influence(modIceOut7))
sum(influence(modIceOut8))
sum(influence(modIceOut9))

#terms
modIceOut7$terms
modIceOut8$terms
modIceOut9$terms

#AIC
modIceOut7$aic
modIceOut8$aic
modIceOut9$aic

#Dev explained
summary(modIceOut7)$dev.expl
summary(modIceOut8)$dev.expl
summary(modIceOut9)$dev.expl

modIceOut9_summary<-summary(modIceOut9)
modIceOut9_summary


#Final variables for paper--
modIceOut9_summary


### Ice Out vs. cumMeanDailyT_Feb
new_data <-
  with(MohonkIceWeather,
       expand.grid(
         cumMeanDailyT_Feb = seq(
           min(cumMeanDailyT_Feb, na.rm = TRUE),
           max(cumMeanDailyT_Feb, na.rm =
                 TRUE),
           length = 200
         ),
         isotherm_TempMean_degC_29_days_4_degC_WaterYear_date = median(isotherm_TempMean_degC_29_days_4_degC_WaterYear_date, na.rm =
                                                                         TRUE),
         cumSnow_FebMarApr = median(cumSnow_FebMarApr, na.rm =
                                      TRUE),
         IceInDayofYear_fed = median(IceInDayofYear_fed, na.rm =
                                       TRUE)
       ))

ilink <- family(modIceOut9)$linkinv
pred_FebT <- predict(modIceOut9, new_data, type = "link", se.fit = TRUE)
pred_FebT <- cbind(pred_FebT, new_data)
pred_FebT <- transform(pred_FebT, lwr_ci = ilink(fit - (2 * se.fit)),
                       upr_ci = ilink(fit + (2 * se.fit)),
                       fitted = ilink(fit))
pred_FebT <- pred_FebT %>%
  select(cumMeanDailyT_Feb, lwr_ci:fitted) %>%
  rename(lwr_ci_FebT = lwr_ci,
         upr_ci_FebT = upr_ci,
         fitted_FebT = fitted)


#Modify axis labels: fed DOY -> dates
breaks_IceOnDayofYear_fed<-c(60,80,100,120) #these are days since oct 1
breaks_IceOffDayofYear_fed<-c(70,80,90,100,110,120) #these are julian day
#To figure out conversion of FED DOY to Date use something like: as.Date(274+110, origin="2014-01-02")


IceOut_FebT<-ggplot(pred_FebT, aes(x = cumMeanDailyT_Feb, y = fitted_FebT)) +
  geom_ribbon(aes(ymin = lwr_ci_FebT, ymax = upr_ci_FebT), alpha = 0.2) +
  geom_line() +
  geom_point(data=MohonkIceWeather, aes(x=cumMeanDailyT_Feb,
                                        y=IceOutDayofYear))+
  labs(x="Feb cumulative\nmean daily temp. (°C)",
       y="Ice Off (Julian Day)")+
  # scale_y_continuous(breaks = seq(70, 120, by = 10) )+
  # coord_cartesian(ylim = c(65, 120), expand = TRUE)+
  scale_y_continuous(breaks=breaks_IceOffDayofYear_fed,labels=c("13-Mar","23-Mar","02-Apr","12-Apr","22-Apr","02-May"),limits=c(70,120))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0.5,0,0,0), "lines"),
        axis.ticks.length.y = unit(0, "pt")) +
  geom_text(data=panelLetter.normal,
            aes(x=xpos,
                y=ypos,
                hjust=hjustvar,
                vjust=vjustvar,
                label="d",
                fontface="bold"))

### Ice Out vs. cumMeanDailyT_Mar
modIceOut9_summary

new_data <-
  with(MohonkIceWeather,
       expand.grid(
         isotherm_TempMean_degC_29_days_4_degC_WaterYear_date = seq(
           min(isotherm_TempMean_degC_29_days_4_degC_WaterYear_date, na.rm = TRUE),
           max(isotherm_TempMean_degC_29_days_4_degC_WaterYear_date, na.rm =
                 TRUE),
           length = 200
         ),
         cumMeanDailyT_Feb = median(cumMeanDailyT_Feb, na.rm =
                                      TRUE),
         cumSnow_FebMarApr = median(cumSnow_FebMarApr, na.rm =
                                      TRUE),
         IceInDayofYear_fed = median(IceInDayofYear_fed, na.rm =
                                       TRUE)
       ))

ilink <- family(modIceOut9)$linkinv
pred_isotherm <- predict(modIceOut9, new_data, type = "link", se.fit = TRUE)
pred_isotherm <- cbind(pred_isotherm, new_data)
pred_isotherm <- transform(pred_isotherm, lwr_ci = ilink(fit - (2 * se.fit)),
                           upr_ci = ilink(fit + (2 * se.fit)),
                           fitted = ilink(fit))
pred_isotherm <- pred_isotherm %>%
  select(isotherm_TempMean_degC_29_days_4_degC_WaterYear_date, lwr_ci:fitted) %>%
  rename(lwr_ci_isotherm = lwr_ci,
         upr_ci_isotherm = upr_ci,
         fitted_isotherm = fitted)


breaks_IsoAvg_fed<-c(170,190,210) #these are julian day
as.Date(210, origin="2014-01-02")

IceOut_isotherm<-ggplot(pred_isotherm, aes(x = isotherm_TempMean_degC_29_days_4_degC_WaterYear_date, y = fitted_isotherm)) +
  geom_ribbon(aes(ymin = lwr_ci_isotherm, ymax = upr_ci_isotherm), alpha = 0.2) +
  geom_line() +
  geom_point(data=MohonkIceWeather, aes(x=isotherm_TempMean_degC_29_days_4_degC_WaterYear_date,
                                        y=IceOutDayofYear))+
  labs(x=expression(Iso["avg,"]["29day,"]["4°C"]),
    # x="Isotherm Formula: TempAvg in degC, 29 day window, 4 degC threshold",
       y="Ice-off date")+
  # scale_y_continuous(breaks = seq(70, 120, by = 10) )+
  # coord_cartesian(ylim = c(65, 120), expand = TRUE)+
  scale_y_continuous(breaks=breaks_IceOffDayofYear_fed,labels=c("13-Mar","23-Mar","02-Apr","12-Apr","22-Apr","02-May"),limits=c(70,120))+
  scale_x_continuous(breaks=breaks_IsoAvg_fed,labels=c("21-Jun","11-Jul","31-Jul"),limits=c(170,220))+
  theme(plot.margin=unit(c(0.5,0,0,0.5), "lines"),
        axis.text.y = element_text(angle = 90, hjust=0.5)) +
  geom_text(data=panelLetter.normal,
            aes(x=xpos,
                y=ypos,
                hjust=hjustvar,
                vjust=vjustvar,
                label="c",
                fontface="bold"))


#Ice Out vs. FebMarAprSnow
new_data <-
  with(MohonkIceWeather,
       expand.grid(
         cumSnow_FebMarApr = seq(
           min(cumSnow_FebMarApr, na.rm = TRUE),
           max(cumSnow_FebMarApr, na.rm =
                 TRUE),
           length = 200
         ),
         cumMeanDailyT_Feb = median(cumMeanDailyT_Feb, na.rm =
                                      TRUE),
         isotherm_TempMean_degC_29_days_4_degC_WaterYear_date = median(isotherm_TempMean_degC_29_days_4_degC_WaterYear_date, na.rm =
                                                                         TRUE),
         IceInDayofYear_fed = median(IceInDayofYear_fed, na.rm =
                                       TRUE)
       ))


ilink <- family(modIceOut9)$linkinv
pred_FebMarAprSnow <- predict(modIceOut9, new_data, type = "link", se.fit = TRUE)
pred_FebMarAprSnow <- cbind(pred_FebMarAprSnow, new_data)
pred_FebMarAprSnow <- transform(pred_FebMarAprSnow, lwr_ci = ilink(fit - (2 * se.fit)),
                                upr_ci = ilink(fit + (2 * se.fit)),
                                fitted = ilink(fit))
pred_FebMarAprSnow <- pred_FebMarAprSnow %>%
  select(cumSnow_FebMarApr, lwr_ci:fitted) %>%
  rename(lwr_ci_FebMarAprSnow = lwr_ci,
         upr_ci_FebMarAprSnow = upr_ci,
         fitted_FebMarAprSnow = fitted)


IceOut_FebMarAprSnow<-ggplot(pred_FebMarAprSnow, aes(x = cumSnow_FebMarApr, y = fitted_FebMarAprSnow)) +
  geom_ribbon(aes(ymin = lwr_ci_FebMarAprSnow, ymax = upr_ci_FebMarAprSnow), alpha = 0.2) +
  geom_line() +
  geom_point(data=MohonkIceWeather, aes(x=cumSnow_FebMarApr,
                                        y=IceOutDayofYear))+
  labs(x="Feb-Apr Cumulative\nSnowfall (mm)",
       y="Ice-off date")+
  # scale_y_continuous(breaks = seq(70, 120, by = 10) )+
  # coord_cartesian(ylim = c(65, 120), expand = TRUE)+
  scale_y_continuous(breaks=breaks_IceOffDayofYear_fed,labels=c("13-Mar","23-Mar","02-Apr","12-Apr","22-Apr","02-May"),limits=c(70,120))+
  theme(plot.margin=unit(c(0,0,0.5,0.5), "lines")) +
  geom_text(data=panelLetter.normal,
            aes(x=xpos,
                y=ypos,
                hjust=hjustvar,
                vjust=vjustvar,
                label="e",
                fontface="bold"))



### Ice Out vs. IceInDayofYear_fed
modIceOut9_summary

new_data <-
  with(MohonkIceWeather,
       expand.grid(
         IceInDayofYear_fed = seq(
           min(IceInDayofYear_fed, na.rm = TRUE),
           max(IceInDayofYear_fed, na.rm =
                 TRUE),
           length = 200
         ),
         cumMeanDailyT_Feb = median(cumMeanDailyT_Feb, na.rm =
                                      TRUE),
         isotherm_TempMean_degC_29_days_4_degC_WaterYear_date = median(isotherm_TempMean_degC_29_days_4_degC_WaterYear_date, na.rm =
                                                                         TRUE),
         cumSnow_FebMarApr = median(cumSnow_FebMarApr, na.rm =
                                      TRUE)
       ))

ilink <- family(modIceOut9)$linkinv
pred_IceIn <- predict(modIceOut9, new_data, type = "link", se.fit = TRUE)
pred_IceIn <- cbind(pred_IceIn, new_data)
pred_IceIn <- transform(pred_IceIn, lwr_ci = ilink(fit - (2 * se.fit)),
                        upr_ci = ilink(fit + (2 * se.fit)),
                        fitted = ilink(fit))
pred_IceIn <- pred_IceIn %>%
  select(IceInDayofYear_fed, lwr_ci:fitted) %>%
  rename(lwr_ci_IceIn = lwr_ci,
         upr_ci_IceIn = upr_ci,
         fitted_IceIn = fitted)


IceOut_IceIn<-ggplot(pred_IceIn, aes(x = IceInDayofYear_fed, y = fitted_IceIn)) +
  geom_ribbon(aes(ymin = lwr_ci_IceIn, ymax = upr_ci_IceIn), alpha = 0.2) +
  geom_line() +
  geom_point(data=MohonkIceWeather, aes(x=IceInDayofYear_fed,
                                        y=IceOutDayofYear))+
  labs(x="Ice-on date",
       y="Ice-off date")+
  # scale_y_continuous(breaks = seq(70, 120, by = 10) )+
  # coord_cartesian(ylim = c(65, 120), expand = TRUE)+
  scale_y_continuous(breaks=breaks_IceOffDayofYear_fed,labels=c("13-Mar","23-Mar","02-Apr","12-Apr","22-Apr","02-May"),limits=c(70,120))+
  scale_x_continuous(breaks=breaks_IceOnDayofYear_fed,labels=c("02-Dec","22-Dec","11-Jan","31-Jan"),limits=c(60,120))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0,0.5,0.5,0), "lines"),
        axis.ticks.length.y = unit(0, "pt")) +
  geom_text(data=panelLetter.normal,
            aes(x=xpos,
                y=ypos,
                hjust=hjustvar,
                vjust=vjustvar,
                label="d",
                fontface="bold"))



# ~~FIGURE 3  Ice Off Predictors --------------------------------------


Row2a<-(IceOut_isotherm+IceOut_FebT)/(IceOut_FebMarAprSnow+IceOut_IceIn)
Row2a


ggsave("figures/Figure3.GamPredictions_IceOff.png", plot=Row2a, width=8, height=5,units="in", dpi=600)




# ~~ FIGURE 2-3 combined --------------------------------------------------


Combined23 <-(IceOn_isotherm+IceOn_CumuNov+plot_spacer())/(IceOut_isotherm+IceOut_FebT+
                                                             IceOut_FebMarAprSnow+  theme(axis.text.y=element_blank(),
                                                                                          axis.ticks.y=element_blank(),
                                                                                          axis.title.y=element_blank(),
                                                                                          plot.margin=unit(c(0.5,0.5,0,0), "lines"),
                                                                                          axis.ticks.length.y = unit(0, "pt")))
Combined23

# ggsave("figures/Figure2-3.IceOn_IceOff_combined.png", plot=Combined23, width=8, height=8,units="in", dpi=600)

ggsave("figures/Fig3.IceOn_IceOff_combined.jpg", plot=Combined23, width=180, height=160,units="mm", dpi=300)




# Fitting GAMs for iceDuration_days -------------------------------------------


#Distribution of y
hist(MohonkIceWeather$LengthOfIceCover_days)

### I added Family Gamma here for how errors should respond
modIceDuration0 <- gam(LengthOfIceCover_days ~ s(Year),
                       # family=Gamma(link="log"),
                       data = MohonkIceWeather,
                       correlation = corCAR1(form = ~ Year),
                       method = "REML")
summary(modIceDuration0)
## summary object
# modIceDuration0_S <- summary(modIceDuration0$gam)
# modIceDuration0_S #Gives you the P values, degrees of freedom...

#PLOT Autocorrelation function of residuals from the additive model with AR(1) errors
ACF <- acf(resid(modIceDuration0, type = "response"), plot = FALSE)
ACF <- setNames(data.frame(unclass(ACF)[c("acf", "lag")]), c("ACF","Lag"))
ggplot(ACF, aes(x = Lag, y = ACF)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = Lag, yend = 0))
#Suggest that an AR(1) model isn't necessary


###Since we're concerned with the response, include "response" in type of predict()
IceDurationPred <- with(MohonkIce, data.frame(Year = seq(min(Year, na.rm=TRUE),
                                                         max(Year, na.rm=TRUE),
                                                         length.out = 200)))
IceDurationPred <- cbind(IceDurationPred, data.frame(predict(modIceDuration0, IceDurationPred,
                                                             type="response",
                                                             se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
IceDurationPred <- transform(IceDurationPred, upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit))


# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "Year"
m1.d <- Deriv(modIceDuration0)

m1.dci <- confint(m1.d, term = "Year")
m1.dsig <- signifD(IceDurationPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

ylim <- with(IceDurationPred, range(upper, lower, fit))
ylab <- 'Ice duration (days)'

plot(fit ~ Year, data = IceDurationPred, type = "n", ylab = ylab, ylim = ylim)
lines(fit ~ Year, data = IceDurationPred)
lines(upper ~ Year, data = IceDurationPred, lty = "dashed")
lines(lower ~ Year, data = IceDurationPred, lty = "dashed")
lines(unlist(m1.dsig$incr) ~ Year, data = IceDurationPred, col = "blue", lwd = 3)
lines(unlist(m1.dsig$decr) ~ Year, data = IceDurationPred, col = "red", lwd = 3)

#the first derivative didn't overlap the horizontal black line.
plot.Deriv(m1.d)

#Plot ice duration vs. year
ggplot(IceDurationPred,aes(x=Year,y=fit))+
  geom_point(data=MohonkIceWeather,
             mapping=aes(x=Year, y=LengthOfIceCover_days), size=2.5, alpha=0.7) +
  geom_line(size=1)+
  geom_ribbon(aes(ymin = (lower), ymax = (upper), x = Year), alpha = 0.5, inherit.aes = FALSE) +
  labs(x="Year",y="Ice Duration (days)")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))



## Create model that includes climate anomoly and some climate teleconnections
# but first look at some of the relationships.

#All variables
IceCover_Vars<-MohonkIceWeather %>%
  filter(Year >= 1950) %>% #When the teleconnection data start
  select(LengthOfIceCover_days, GlobalTempanomaly_C, contains("NAO"))

IceCover_Vars %>%
  ggpairs() 
#Global temp anomaly, NAO_winter, NAO_Nov, and Nov_Dec all have a significant correlation


#Create table
res4 <- rcorr(as.matrix(IceCover_Vars))
# res3 <- rcorr(as.matrix(MohonkIceWeather[,3:ncol(MohonkIceWeather)]))
IceCover_correlations<-flattenCorrMatrix(res4$r, res4$P) %>%
  filter(row %in% "LengthOfIceCover_days") %>%
  arrange(p) 


hist(MohonkIceWeather$GlobalTempanomaly_C)
hist(MohonkIceWeather$LengthOfIceCover_days)
hist(MohonkIceWeather$NAO_index_winter)
hist(MohonkIceWeather$NAO_index_Nov)
hist(MohonkIceWeather$NAO_index_Dec)


### Mod1
set.seed(11)
modIceDuration1 <- gam(LengthOfIceCover_days ~ s(GlobalTempanomaly_C, k=50) +
                         s(NAO_index_Nov, k=10)+
                         s(NAO_index_Dec, k=10),
                       # family=Gamma(link="log"),
                       family=scat(link="identity"), #for heavy tail
                       data = MohonkIceWeather,
                       # correlation = corCAR1(form = ~ Year),
                       method = "REML")
summary(modIceDuration1)

gam.check(modIceDuration1)
#Want to set k sufficiently high. At default, was getting low p-value for GlobalTempanomaly_C

draw(modIceDuration1, residuals = TRUE)
# OVerfitting problem with NAO_index_winter?

plot(modIceDuration1,
     shift = coef(modIceDuration1)[1],
     pages =1)



### Mod 2
#Drop November
modIceDuration2 <- gam(LengthOfIceCover_days ~  s(GlobalTempanomaly_C, k=30) +
                         s(NAO_index_Dec, k=10),
                       # family=Gamma(link="log"),
                       family=scat(link="identity"), #for heavy tail
                       data = MohonkIceWeather,
                       # correlation = corCAR1(form = ~ Year),
                       method = "REML")
summary(modIceDuration2)
#Doesn't improve things


gam.check(modIceDuration2)
#Want to set k sufficiently high. At default, was getting low p-value for GlobalTempanomaly_C

plot(modIceDuration2,
     shift = coef(modIceDuration2)[1],
     pages =1)

draw(modIceDuration2, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals
# Kind of interesting/confusing. So basically in years with both high fall and winter NAO indices,
# you have longer ice cover. And vice versa.

### Mod3
set.seed(11)
modIceDuration3 <- gam(LengthOfIceCover_days ~  s(GlobalTempanomaly_C, k=50) +
                         s(NAO_index_winter, k=3),
                       # family=Gamma(link="log"),
                       family=scat(link="identity"), #for heavy tail
                       data = MohonkIceWeather,
                       # correlation = corCAR1(form = ~ Year),
                       method = "REML")
summary(modIceDuration3)
#Worse fit than NAO_Nov and NAO_Dec separately. 

gam.check(modIceDuration3)
#Want to set k sufficiently high. At default, was getting low p-value for GlobalTempanomaly_C

draw(modIceDuration3, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals

plot(modIceDuration3,
     shift = coef(modIceDuration3)[1],
     pages =1)



# Compare models
compareML(modIceDuration1, modIceDuration2) # Basically indistinguishable?
compareML(modIceDuration1, modIceDuration3) # Similar performance-- could pick based on %Dev explained?

modIceDuration1_summary<- summary.gam(modIceDuration1)
modIceDuration2_summary<- summary.gam(modIceDuration2)
modIceDuration3_summary<- summary.gam(modIceDuration3)
modIceDuration1_summary$dev.expl
modIceDuration2_summary$dev.expl
modIceDuration3_summary$dev.expl
# Including NAO_Nov and NAO_Dec separately leads to more variance explained though still low!

appraise(modIceDuration1)


# ~~FIGURE 4 ~ IceCoverDuration vs.  ABC ---------------------------------



#Final variables for paper--
modIceDuration1_summary


### Panel A -- Ice Duration vs. GlobalTempanomaly_C
new_data <-
  with(MohonkIceWeather,
       expand.grid(
         GlobalTempanomaly_C = seq(
           min(GlobalTempanomaly_C, na.rm = TRUE),
           max(GlobalTempanomaly_C, na.rm =
                 TRUE),
           length = 200
         ),
         NAO_index_Dec = median(NAO_index_Dec, na.rm =
                                  TRUE),
         NAO_index_Nov = median(NAO_index_Nov, na.rm =
                                  TRUE)
       ))

ilink <- family(modIceDuration1)$linkinv
pred_GlobalT <- predict(modIceDuration1, new_data, type = "link", se.fit = TRUE)
pred_GlobalT <- cbind(pred_GlobalT, new_data)
pred_GlobalT <- transform(pred_GlobalT, lwr_ci = ilink(fit - (2 * se.fit)),
                          upr_ci = ilink(fit + (2 * se.fit)),
                          fitted = ilink(fit))
pred_GlobalT <- pred_GlobalT %>%
  select(GlobalTempanomaly_C, lwr_ci:fitted) %>%
  rename(lwr_ci_GlobalT = lwr_ci,
         upr_ci_GlobalT = upr_ci,
         LengthOfIceCover_days = fitted)

IceDuration_GlobalT<-
  ggplot(pred_GlobalT, aes(x = GlobalTempanomaly_C, y = LengthOfIceCover_days)) +
  geom_ribbon(aes(ymin = lwr_ci_GlobalT, ymax = upr_ci_GlobalT), alpha = 0.2) +
  geom_line() +
  geom_point(data=MohonkIceWeather , aes(x=GlobalTempanomaly_C,
                                        y=LengthOfIceCover_days))+
  labs(x="Global Temperature Anomaly (°C)",
       y="Ice duration (days)")+
  scale_y_continuous(breaks = seq(30, 150, by = 30) )+
  coord_cartesian(ylim = c(30, 150), expand = TRUE) +
  theme(plot.margin=unit(c(0.5,0,0.5,0.5), "lines"))+
        # legend.justification = c(1, 1),
        # legend.position = c(.98,.98),
        # legend.background = element_rect(fill = "white", color = NA)) +
  # scale_fill_gradient(low = "green", high = "red", na.value = NA)+
  geom_text(data=panelLetter.normal,
            aes(x=xpos,
                y=ypos,
                hjust=hjustvar,
                vjust=vjustvar,
                label="a",
                fontface="bold"))


### Panel B-- Ice Duration vs. NAO_index_Dec
modIceDuration1_summary

new_data <-
  with(MohonkIceWeather,
       expand.grid(
         NAO_index_Dec = seq(
           min(NAO_index_Dec, na.rm = TRUE),
           max(NAO_index_Dec, na.rm =
                 TRUE),
           length = 200
         ),
         GlobalTempanomaly_C = median(GlobalTempanomaly_C, na.rm =
                                        TRUE),
         NAO_index_Nov = median(NAO_index_Nov, na.rm =
                                  TRUE)
       ))

ilink <- family(modIceDuration1)$linkinv
pred_DecNAO <- predict(modIceDuration1, new_data, type = "link", se.fit = TRUE)
pred_DecNAO <- cbind(pred_DecNAO, new_data)
pred_DecNAO <- transform(pred_DecNAO, lwr_ci = ilink(fit - (2 * se.fit)),
                         upr_ci = ilink(fit + (2 * se.fit)),
                         fitted = ilink(fit))
pred_DecNAO <- pred_DecNAO %>%
  select(NAO_index_Dec, lwr_ci:fitted) %>%
  rename(lwr_ci_DecNAO = lwr_ci,
         upr_ci_DecNAO = upr_ci,
         LengthOfIceCover_days = fitted)

IceDuration_DecNAO<-ggplot(pred_DecNAO, aes(x = NAO_index_Dec, y = LengthOfIceCover_days)) +
  geom_ribbon(aes(ymin = lwr_ci_DecNAO, ymax = upr_ci_DecNAO), alpha = 0.2) +
  geom_line() +
  geom_point(data=MohonkIceWeather, aes(x=NAO_index_Dec,
                                        y=LengthOfIceCover_days))+
  labs(x="Dec NAO index",
       y="Length of ice cover (days)")+
  scale_y_continuous(breaks = seq(30, 150, by = 30) )+
  coord_cartesian(ylim = c(30, 150), expand = TRUE) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0.5,0,0.5,0), "lines"),
        axis.ticks.length.y = unit(0, "pt"))+
  geom_text(data=panelLetter.normal,
            aes(x=xpos,
                y=ypos,
                hjust=hjustvar,
                vjust=vjustvar,
                label="b",
                fontface="bold"))


### Panel C-- Ice Duration vs. NAO_index_Nov
modIceDuration1_summary

new_data <-
  with(MohonkIceWeather,
       expand.grid(
         NAO_index_Nov = seq(
           min(NAO_index_Nov, na.rm = TRUE),
           max(NAO_index_Nov, na.rm =
                 TRUE),
           length = 200
         ),
         GlobalTempanomaly_C = median(GlobalTempanomaly_C, na.rm =
                                        TRUE),
         NAO_index_Dec = median(NAO_index_Dec, na.rm =
                                  TRUE)
       ))

ilink <- family(modIceDuration1)$linkinv
pred_NovNAO <- predict(modIceDuration1, new_data, type = "link", se.fit = TRUE)
pred_NovNAO <- cbind(pred_NovNAO, new_data)
pred_NovNAO <- transform(pred_NovNAO, lwr_ci = ilink(fit - (2 * se.fit)),
                         upr_ci = ilink(fit + (2 * se.fit)),
                         fitted = ilink(fit))
pred_NovNAO <- pred_NovNAO %>%
  select(NAO_index_Nov, lwr_ci:fitted) %>%
  rename(lwr_ci_NovNAO = lwr_ci,
         upr_ci_NovNAO = upr_ci,
         LengthOfIceCover_days = fitted)

IceDuration_NovNAO<-ggplot(pred_NovNAO, aes(x = NAO_index_Nov, y = LengthOfIceCover_days)) +
  geom_ribbon(aes(ymin = lwr_ci_NovNAO, ymax = upr_ci_NovNAO), alpha = 0.2) +
  geom_line() +
  geom_point(data=MohonkIceWeather, aes(x=NAO_index_Nov,
                                        y=LengthOfIceCover_days))+
  labs(x="Nov NAO index",
       y="Length of ice cover (days)")+
  scale_y_continuous(breaks = seq(30, 150, by = 30) )+
  coord_cartesian(ylim = c(30, 150), expand = TRUE) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0.5,0.5,0.5,0), "lines"),
        axis.ticks.length.y = unit(0, "pt"))+
  geom_text(data=panelLetter.normal,
            aes(x=xpos,
                y=ypos,
                hjust=hjustvar,
                vjust=vjustvar,
                label="c",
                fontface="bold"))




composite_noLegend<-(IceDuration_GlobalT+IceDuration_DecNAO+IceDuration_NovNAO)
composite_noLegend



ggsave("figures/Figure4.GamPredictions_IceDuration.png", width=7, height=3,units="in", dpi=300)







# 10 year rolling window of C.V.? -----------------------------------------
#Try using zoo package
library(zoo)
#Code snagged from Gavin in this response:
#https://stackoverflow.com/questions/13195442/moving-variance-in-r



# >> Ice phenology cv -----------------------------------------------------



##Ice Duration
iceDuration_days_withNAs<-MohonkIce %>%
  select(LengthOfIceCover_days,Year) 

iceDuration_days<-MohonkIce %>%
  select(LengthOfIceCover_days) %>%
  drop_na()


iceDuration_cv<-data.frame(rollapply(iceDuration_days, width =7, FUN = cv)) %>%
  rownames_to_column(., var = "rollingwindow_timepoint")  #convert column name to row name 

iceDuration_cv %>%
  ggplot(aes(x=as.numeric(rollingwindow_timepoint),
             y=LengthOfIceCover_days))+geom_point()+
  xlab("7-year rolling window timepoint")+
  ylab("Ice duration (c.v.)")+
  geom_smooth(method="gam", color="black", size=0.5)


#Fitting a GAM to ice duration c.v. and the rolling average timepoint. 





##Ice On
iceOnDOY_fed<-MohonkIce %>%
  select(IceInDayofYear_fed) %>%
  drop_na()

iceOnDOY_fed_cv<-data.frame(rollapply(iceOnDOY_fed, width = 10, FUN = cv)) %>%
  rownames_to_column(., var = "rollingwindow_timepoint")  #convert column name to row name 

iceOnDOY_fed_cv %>%
  ggplot(aes(x=as.numeric(rollingwindow_timepoint),
             y=IceInDayofYear_fed))+geom_point()+
  xlab("10-year rolling window timepoint")+
  ylab("Ice on (c.v.)")



##Ice Off
iceOffDOY<-MohonkIce %>%
  select(IceOutDayofYear) %>%
  drop_na()

iceOffDOY_cv<-data.frame(rollapply(iceOffDOY, width = 10, FUN = cv)) %>%
  rownames_to_column(., var = "rollingwindow_timepoint")  #convert column name to row name 

iceOffDOY_cv %>%
  ggplot(aes(x=as.numeric(rollingwindow_timepoint),
             y=IceOutDayofYear))+geom_point()+
  xlab("10-year rolling window timepoint")+
  ylab("Ice off (c.v.)")


#Shorter rolling window timepoint since there doesn't appear to be any periodicity. Just want to answer the question: is ice-out day getting more variable?

##Ice Off - again but shorter duration

iceOffDOY_cv<-data.frame(rollapply(iceOffDOY, width = 2, FUN = cv)) %>%
  rownames_to_column(., var = "rollingwindow_timepoint")  #convert column name to row name 

iceOffDOY_cv %>%
  ggplot(aes(x=as.numeric(rollingwindow_timepoint),
             y=IceOutDayofYear))+geom_point()+
  xlab("2-year rolling window timepoint")+
  ylab("Ice off (c.v.)")


lm_iceOffDOY_cv <- lm(IceOutDayofYear~as.numeric(rollingwindow_timepoint),
                      iceOffDOY_cv)
summary(lm_iceOffDOY_cv)
#No strong evidence of increasing variability in ice-out day.

# >> rolling avgs of weather patterns -------------------------------------


#ENSO 7-year rolling average... 
ENSO_withNAs<-MohonkIceWeather %>%
  select(ENSO_index_fall,Year) 
ENSO<-MohonkIceWeather %>%
  select(ENSO_index_fall) %>%
  drop_na()

ENSO_mean<-data.frame(rollapply(ENSO, width =7, FUN = mean)) %>%
  rownames_to_column(., var = "rollingwindow_timepoint")  #convert column name to row name 

ENSO_mean %>%
  ggplot(aes(x=as.numeric(rollingwindow_timepoint),
             y=ENSO_index_fall))+geom_point()+
  xlab("7-year rolling window timepoint (1979-2020")+
  ylab("ENSO (mean)")+
  geom_smooth(method="lm", color="black", size=0.5)

#Is the 7-year rolling average of winter ENSO related to the c.v. of ice duration?
#I guses we can only examine 1978-2020 because of limited ENSO record. Let's try.
iceDuration_days_short<-MohonkIceWeather %>%
  select(LengthOfIceCover_days, Year) %>%
  left_join(.,ENSO_withNAs, by="Year")%>% ##This step ensures we have all the same years.
  drop_na()%>%                                        ##we have some missing obs. of ice duration early on.
  select(-Year,-ENSO_index_fall)

iceDuration_short_cv<-data.frame(rollapply(iceDuration_days_short, width =7, FUN = cv)) %>%
  rownames_to_column(., var = "rollingwindow_timepoint")  #convert column name to row name 

iceDuration_short_cv %>%
  ggplot(aes(x=as.numeric(rollingwindow_timepoint),
             y=LengthOfIceCover_days))+geom_point()+
  xlab("7-year rolling window timepoint")+
  ylab("Ice duration (c.v.) - 1979-2020")+
  geom_smooth(method="gam", color="black", size=0.5)

#is there a correlation btwn the two?
#Are they correlated?
left_join(ENSO_mean,iceDuration_short_cv)%>%
  ggplot(aes(x=ENSO_index_fall,y=LengthOfIceCover_days))+
  geom_point()
corr_temp<-left_join(ENSO_mean,iceDuration_short_cv)%>%
  mutate(rollingwindow_timepoint=as.numeric(rollingwindow_timepoint))%>%
  arrange(rollingwindow_timepoint) %>%
  drop_na()
cor.test(corr_temp$ENSO_index_fall, corr_temp$LengthOfIceCover_days)
#No

#But do they look related *at all*???
left_join(ENSO_mean,iceDuration_short_cv)%>%
  pivot_longer(-rollingwindow_timepoint) %>%
  ggplot(aes(x=as.numeric(rollingwindow_timepoint),y=value))+
  geom_point()+facet_wrap(.~name, scales="free_y",nrow=2)+
  xlab("7-year rolling winter ")
##I wouldn't expect much of a relationship here because most likely the cv of ice over length
##is more directly related to temp or precipitation. The periodicity is brought on by the ENSO cycles.


##Is the 7-year rolling average of cumulative fall temperature related to the c.v. of ice duration?
cumfall_T<-MohonkIceWeather %>%
  select(cumMeanDailyT_OctNovDec, Year) %>%
  left_join(.,iceDuration_days_withNAs, by="Year")%>% ##This step ensures we have all the same years.
  drop_na()%>%                                        ##we have some missing obs. of ice duration early on.
  select(-Year,-LengthOfIceCover_days)

cumfall_T_mean<-data.frame(rollapply(cumfall_T, width =7, FUN = mean)) %>%
  rownames_to_column(., var = "rollingwindow_timepoint")  #convert column name to row name 

cumfall_T_mean %>%
  ggplot(aes(x=as.numeric(rollingwindow_timepoint),
             y=cumMeanDailyT_OctNovDec))+geom_point()+
  xlab("7-year rolling window timepoint")+
  ylab("Cum. mean daily T of fall (mean)")+
  geom_smooth(method="lm", color="black", size=0.5)

#Are they correlated?
left_join(cumfall_T_mean,iceDuration_cv)%>%
  ggplot(aes(x=cumMeanDailyT_OctNovDec,y=LengthOfIceCover_days))+
  geom_point()+
  xlab("7-year rolling average of cumulative mean daily fall temps (Oct+Nov+Dec)")+
  ylab("7-year rolling average of the c.v. in ice over duration")

corr_temp<-left_join(cumfall_T_mean,iceDuration_cv)
print(cor.test(corr_temp$cumMeanDailyT_OctNovDec,corr_temp$LengthOfIceCover_days))
#Does't look like much, but there *is* a significant correlation between the two. 

##Is the 7-year rolling average of cumulative fall precip related to the c.v. of ice duration?
cumfall_rain<-MohonkIceWeather %>%
  select(cumRain_OctNovDec, Year) %>%
  left_join(.,iceDuration_days_withNAs, by="Year")%>% ##This step ensures we have all the same years.
  drop_na()%>%                                        ##we have some missing obs. of ice duration early on.
  select(-Year,-LengthOfIceCover_days)

cumfall_rain_mean<-data.frame(rollapply(cumfall_rain, width =7, FUN = mean)) %>%
  rownames_to_column(., var = "rollingwindow_timepoint")  #convert column name to row name 

cumfall_rain_mean %>%
  ggplot(aes(x=as.numeric(rollingwindow_timepoint),
             y=cumRain_OctNovDec))+geom_point()+
  xlab("7-year rolling window timepoint")+
  ylab("Cum. fall rain (7-year rolling mean)")+
  geom_smooth(method="lm", color="black", size=0.5)
#It does look cyclicall.. 


#Are they correlated?
left_join(cumfall_rain_mean,iceDuration_cv)%>%
  ggplot(aes(x=cumRain_OctNovDec,y=LengthOfIceCover_days))+
  geom_point()
corr_temp<-left_join(cumfall_rain_mean,iceDuration_cv)
print(cor.test(corr_temp$cumRain_OctNovDec,corr_temp$LengthOfIceCover_days))
#No...

##Is the 7-year rolling average of prop. fall precip as RAIN related to the c.v. of ice duration?
propfall_rain<-MohonkIceWeather %>%
  select(cumRain_OctNovDec, cumSnow_OctNovDec, Year) %>%
  mutate(prop_rain=cumRain_OctNovDec/(cumRain_OctNovDec+cumSnow_OctNovDec)) %>%
  left_join(.,iceDuration_days_withNAs, by="Year")%>% ##This step ensures we have all the same years.
  drop_na()%>%                                        ##we have some missing obs. of ice duration early on.
  select(-Year,-LengthOfIceCover_days,
         -cumRain_OctNovDec,-cumSnow_OctNovDec)

propfall_rain_mean<-data.frame(rollapply(propfall_rain, width =7, FUN = mean)) %>%
  rownames_to_column(., var = "rollingwindow_timepoint")  #convert column name to row name 

propfall_rain_mean %>%
  ggplot(aes(x=as.numeric(rollingwindow_timepoint),
             y=prop_rain))+geom_point()+
  xlab("7-year rolling window timepoint")+
  ylab("Cum. fall rain (7-year rolling mean)")+
  geom_smooth(method="lm", color="black", size=0.5)



# Table S1.  Sens slope climate trends ------------------------------------
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
# quick_docx(Climate.SensSlopeSummary_hux, file = 'figures/TableS1.SensSlopesClimateVariables.docx')




# ~~~~~~~ Visualize trends ~~~~~~~~ --------------------------------------------------------



#Visualize the years where we have good precip. as snow data
MohonkDailyWeatherFull %>% 
  filter(season=="winter")%>%
  select(Date, Snow_mm, water_year) %>%
  drop_na()%>%
  group_by(water_year) %>%
  summarize(snow_total_mm=sum(Snow_mm))%>%
  ggplot(aes(x=water_year, y=snow_total_mm))+
  geom_point()+
  ggtitle("Winter snow totals")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))  
## The caveat here is that prior to 1950 the snow data is kind of patchy


#Visualize the years where we have good snow depth data
MohonkDailyWeatherFull %>% 
  filter(season=="winter")%>%
  select(Date, SnowDepth_mm, water_year) %>%
  drop_na()%>%
  group_by(water_year) %>%
  summarize(mean_snowdepth=mean(SnowDepth_mm))%>%
  ggplot(aes(x=water_year, y=mean_snowdepth))+
  geom_point()+
  ggtitle("Winter snow depth average")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))  

#Mean snow depth in march
MohonkDailyWeatherFull %>% 
  filter(Month=="3")%>%
  select(Date, SnowDepth_mm, Year) %>%
  drop_na()%>%
  group_by(Year) %>%
  summarize(mean_snowdepth=mean(SnowDepth_mm))%>%
  ggplot(aes(x=Year, y=mean_snowdepth))+
  geom_point()+
  ggtitle("March snow depth average")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))  

#Maximum winter snow depth (kind of like max SWE? good predictor of ice off in Rocky lakes)
MohonkDailyWeatherFull %>% 
  filter(season=="winter")%>%
  select(Date, SnowDepth_mm, water_year) %>%
  drop_na()%>%
  group_by(water_year) %>%
  summarize(max_snowdepth=max(SnowDepth_mm))%>%
  ggplot(aes(x=water_year, y=max_snowdepth))+
  geom_point()+
  ggtitle("Winter snow depth maximum")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))  


MohonkDailyWeatherFull %>% 
  filter(season=="winter")%>%
  select(Date, SnowDepth_mm, water_year) %>%
  drop_na()%>%
  group_by(water_year) %>%
  summarize(max_snowdepth=max(SnowDepth_mm))%>%
  ggplot(aes(x=water_year, y=max_snowdepth))+
  geom_point()+
  ggtitle("March snow depth maximum")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))  


#Trends in March-April-May temperatures
MohonkDailyWeatherFull %>%
  select(Year, Month,TempMax_degC, TempMin_degC, TempMean_degC) %>%
  group_by(Year, Month) %>%
  summarize_at(vars(TempMax_degC:TempMean_degC), mean, na.rm=TRUE) %>%
  pivot_longer(-(1:2)) %>%
  filter(Month %in% c("3","4","5"))%>%
  ggplot(aes(x=Year,y=value,color=name))+
  geom_point()+
  facet_wrap(.~Month, nrow=3, scales="free_y")+
  geom_line(size=0.5)+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))  

#Trends in seasonal temperatures
MohonkDailyWeatherFull %>%
  select(water_year, season,TempMax_degC, TempMin_degC, TempMean_degC) %>%
  group_by(water_year,season) %>%
  summarize_at(vars(TempMax_degC:TempMean_degC), mean, na.rm=TRUE) %>%
  pivot_longer(-(1:2)) %>%
  # filter(month %in% c("3","4","5"))%>%
  ggplot(aes(x=water_year,y=value,color=name))+
  geom_point()+
  facet_wrap(.~season, nrow=3, scales="free_y")+
  geom_line(size=0.5)+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))  +
  ylab("Temperature (C)")+
  theme(legend.position="bottom")+
  geom_smooth(size=0.5)


MohonkIceWeather %>%
  select(Year, contains(c("cumMean"))) %>%
  pivot_longer(-1) %>%
  ggplot(aes(x=Year, y=value))+
  geom_point()+
  # ggtitle("Cumulative Mar+Apr temperatures")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))+
  geom_smooth()+
  facet_wrap(~name, scales="free_y")


# >> corr. with ice-off date ----------------------------------------------

# ggpairs(MohonkIceWeather, columns = 3:ncol(MohonkIceWeather)) 

#Strong correlations between Ice out DOY and spring snow (+) and temperatures (-)
#Moderate correlations between Ice on DOY and Ice duration and MEI of previous winter

MohonkIceWeather %>%
  select(IceOutDayofYear, LengthOfIceCover_days, cumMeanDailyT_Mar, nDaysMeanBelowZero_Mar,
         nDaysMinBelowZero_Mar, cumMeanDailyT_FebMar, cumSnow_FebMar, nDaysMeanAboveZero_FebMar,
         maxSnowDepth_mm, NAO_index_spring) %>%
  ggpairs()

# >> corr. with ice-on date ----------------------------------------------

MohonkIceWeather %>%
  mutate(prop_rain=cumRain_OctNovDec/(cumRain_OctNovDec+cumSnow_OctNovDec)) %>%
  select(IceInDayofYear_fed, prop_rain, ENSO_index_fall, LengthOfIceCover_days, cumMeanDailyT_Dec,
         nDaysMeanBelowZero_Dec, cumMeanDailyT_OctNov, cumMeanDailyT_OctNovDec, nDaysMinBelowZero_OctNovDec) %>%
  ggpairs()









# Fitting GAMs for mean winter temperature -------------------------------------------
WinterWx<-MohonkDailyWeatherFull %>%
  filter(season=="winter")%>%
  select(water_year, TempMax_degC, TempMin_degC, TempMean_degC) %>%
  group_by(water_year) %>%
  summarize_at(vars(TempMax_degC:TempMean_degC), mean, na.rm=TRUE) 


### Model
modWinterTempMean <- gamm(TempMean_degC ~ s(water_year),
                          data = WinterWx,
                          correlation = corCAR1(form = ~ water_year),
                          method = "REML")

## summary object
modWinterTempMean_S <- summary(modWinterTempMean$gam)
modWinterTempMean_S #Gives you the P values, degrees of freedom...

###Since we're concerned with the response, include "response" in type of predict()
WinterTempMeanPred <- with(WinterWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                                 max(water_year, na.rm=TRUE),
                                                                 length.out = 200)))
WinterTempMeanPred <- cbind(WinterTempMeanPred, data.frame(predict(modWinterTempMean$gam, WinterTempMeanPred,
                                                                   type="response",
                                                                   se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
WinterTempMeanPred <- transform(WinterTempMeanPred, upper = fit + (2 * se.fit),
                                lower = fit - (2 * se.fit))

# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modWinterTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(WinterTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

ylim <- with(WinterTempMeanPred, range(upper, lower, fit))
ylab <- 'Mean winter temp (water year)'

plot(fit ~ water_year, data = WinterTempMeanPred, type = "n", ylab = ylab, ylim = ylim)
lines(fit ~ water_year, data = WinterTempMeanPred)
lines(upper ~ water_year, data = WinterTempMeanPred, lty = "dashed")
lines(lower ~ water_year, data = WinterTempMeanPred, lty = "dashed")
lines(unlist(m1.dsig$incr) ~ water_year, data = WinterTempMeanPred, col = "blue", lwd = 3)
lines(unlist(m1.dsig$decr) ~ water_year, data = WinterTempMeanPred, col = "red", lwd = 3)
#To me this indicates that there is no acceleration, and the rate of change is fairly constant. 

#But another way to visualize it is there would be a significant period of change if the error bar around
#the first derivative didn't overlap the horizontal black line.
plot.Deriv(m1.d)

#Plot Ice on DOY vs. year pretty
ggplot(WinterTempMeanPred,aes(x=water_year,y=fit))+
  geom_point(data=WinterWx,
             mapping=aes(x=water_year, y=TempMean_degC), size=2.5, alpha=0.7) +
  geom_line(size=1)+
  geom_ribbon(aes(ymin = (lower), ymax = (upper), x = water_year), alpha = 0.5, inherit.aes = FALSE) +
  labs(x="Water year",y="Mean winter temperature (C))")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))

# Fitting GAMs for max winter temperature -------------------------------------------
WinterWx<-MohonkDailyWeatherFull %>%
  filter(season=="winter")%>%
  select(water_year, TempMax_degC, TempMin_degC, TempMean_degC) %>%
  group_by(water_year) %>%
  summarize_at(vars(TempMax_degC:TempMean_degC), mean, na.rm=TRUE) 


### Model
modWinterTempMax <- gamm(TempMax_degC ~ s(water_year),
                         data = WinterWx,
                         correlation = corCAR1(form = ~ water_year),
                         method = "REML")

## summary object
modWinterTempMax_S <- summary(modWinterTempMax$gam)
modWinterTempMax_S #Gives you the P values, degrees of freedom...

###Since we're concerned with the response, include "response" in type of predict()
WinterTempMaxPred <- with(WinterWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                                max(water_year, na.rm=TRUE),
                                                                length.out = 200)))
WinterTempMaxPred <- cbind(WinterTempMaxPred, data.frame(predict(modWinterTempMax$gam, WinterTempMaxPred,
                                                                 type="response",
                                                                 se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
WinterTempMaxPred <- transform(WinterTempMaxPred, upper = fit + (2 * se.fit),
                               lower = fit - (2 * se.fit))

# ?transform
# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modWinterTempMax)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(WinterTempMaxPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

ylim <- with(WinterTempMaxPred, range(upper, lower, fit))
ylab <- 'Max winter temp (water year)'

plot(fit ~ water_year, data = WinterTempMaxPred, type = "n", ylab = ylab, ylim = ylim)
lines(fit ~ water_year, data = WinterTempMaxPred)
lines(upper ~ water_year, data = WinterTempMaxPred, lty = "dashed")
lines(lower ~ water_year, data = WinterTempMaxPred, lty = "dashed")
lines(unlist(m1.dsig$incr) ~ water_year, data = WinterTempMaxPred, col = "blue", lwd = 3)
lines(unlist(m1.dsig$decr) ~ water_year, data = WinterTempMaxPred, col = "red", lwd = 3)
#An acceleration in the trend 1990s-2000s

#But another way to visualize it is there would be a significant period of change if the error bar around
#the first derivative didn't overlap the horizontal black line.
plot.Deriv(m1.d)

#Plot Ice on DOY vs. year pretty
ggplot(WinterTempMaxPred,aes(x=water_year,y=fit))+
  geom_point(data=WinterWx,
             mapping=aes(x=water_year, y=TempMax_degC), size=2.5, alpha=0.7) +
  geom_line(size=1)+
  geom_ribbon(aes(ymin = (lower), ymax = (upper), x = water_year), alpha = 0.5, inherit.aes = FALSE) +
  labs(x="Water year",y="Max winter temperature (C))")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))

# Fitting GAMs for min winter temperature -------------------------------------------
WinterWx<-MohonkDailyWeatherFull %>%
  filter(season=="winter")%>%
  select(water_year, TempMax_degC, TempMin_degC, TempMean_degC) %>%
  group_by(water_year) %>%
  summarize_at(vars(TempMax_degC:TempMean_degC), mean, na.rm=TRUE) 


### I added Family Gamma here for how errors should respond
modWinterTempMin <- gamm(TempMin_degC ~ s(water_year),
                         data = WinterWx,
                         correlation = corCAR1(form = ~ water_year),
                         method = "REML")

## summary object
modWinterTempMin_S <- summary(modWinterTempMin$gam)
modWinterTempMin_S #Gives you the P values, degrees of freedom...

###Since we're concerned with the response, include "response" in type of predict()
WinterTempMinPred <- with(WinterWx, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                                max(water_year, na.rm=TRUE),
                                                                length.out = 200)))
WinterTempMinPred <- cbind(WinterTempMinPred, data.frame(predict(modWinterTempMin$gam, WinterTempMinPred,
                                                                 type="response",
                                                                 se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
WinterTempMinPred <- transform(WinterTempMinPred, upper = fit + (2 * se.fit),
                               lower = fit - (2 * se.fit))


# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modWinterTempMin)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(WinterTempMinPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

ylim <- with(WinterTempMinPred, range(upper, lower, fit))
ylab <- 'Min winter temp (water year)'

plot(fit ~ water_year, data = WinterTempMinPred, type = "n", ylab = ylab, ylim = ylim)
lines(fit ~ water_year, data = WinterTempMinPred)
lines(upper ~ water_year, data = WinterTempMinPred, lty = "dashed")
lines(lower ~ water_year, data = WinterTempMinPred, lty = "dashed")
lines(unlist(m1.dsig$incr) ~ water_year, data = WinterTempMinPred, col = "blue", lwd = 3)
lines(unlist(m1.dsig$decr) ~ water_year, data = WinterTempMinPred, col = "red", lwd = 3)
#To me this indicates that there is no acceleration, and the rate of change is fairly constant. 

#But another way to visualize it is there would be a significant period of change if the error bar around
#the first derivative didn't overlap the horizontal black line.
plot.Deriv(m1.d)

#Plot Ice on DOY vs. year pretty
ggplot(WinterTempMinPred,aes(x=water_year,y=fit))+
  geom_point(data=WinterWx,
             mapping=aes(x=water_year, y=TempMin_degC), size=2.5, alpha=0.7) +
  geom_line(size=1)+
  geom_ribbon(aes(ymin = (lower), ymax = (upper), x = water_year), alpha = 0.5, inherit.aes = FALSE) +
  labs(x="Water year",y="Min winter temperature (C))")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))



# Fitting GAMs for mean sept temperature -------------------------------------------
FallWx<-MohonkDailyWeatherFull %>%
  filter(Month==9)%>%
  select(Year, TempMax_degC, TempMin_degC, TempMean_degC) %>%
  group_by(Year) %>%
  summarize_at(vars(TempMax_degC:TempMean_degC), mean, na.rm=TRUE) 


### I added Family Gamma here for how errors should respond
modFallTempMean <- gamm(TempMean_degC ~ s(Year),
                        data = FallWx,
                        correlation = corCAR1(form = ~ Year),
                        method = "REML")

## summary object
modFallTempMean_S <- summary(modFallTempMean$gam)
modFallTempMean_S #Gives you the P values, degrees of freedom...

###Since we're concerned with the response, include "response" in type of predict()
FallTempMeanPred <- with(FallWx, data.frame(Year = seq(min(Year, na.rm=TRUE),
                                                       max(Year, na.rm=TRUE),
                                                       length.out = 200)))
FallTempMeanPred <- cbind(FallTempMeanPred, data.frame(predict(modFallTempMean$gam, FallTempMeanPred,
                                                               type="response",
                                                               se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
FallTempMeanPred <- transform(FallTempMeanPred, upper = fit + (2 * se.fit),
                              lower = fit - (2 * se.fit))

?transform
# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "Year"
m1.d <- Deriv(modFallTempMean)

m1.dci <- confint(m1.d, term = "Year")
m1.dsig <- signifD(FallTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

ylim <- with(FallTempMeanPred, range(upper, lower, fit))
ylab <- 'Mean Sept temp (water Year)'

plot(fit ~ Year, data = FallTempMeanPred, type = "n", ylab = ylab, ylim = ylim)
lines(fit ~ Year, data = FallTempMeanPred)
lines(upper ~ Year, data = FallTempMeanPred, lty = "dashed")
lines(lower ~ Year, data = FallTempMeanPred, lty = "dashed")
lines(unlist(m1.dsig$incr) ~ Year, data = FallTempMeanPred, col = "blue", lwd = 3)
lines(unlist(m1.dsig$decr) ~ Year, data = FallTempMeanPred, col = "red", lwd = 3)
#Pretty substantial acceleration in the trend mid-1980s through 2010s

#But another way to visualize it is there would be a significant period of change if the error bar around
#the first derivative didn't overlap the horizontal black line.
plot.Deriv(m1.d)

#Plot Ice on DOY vs. Year pretty
ggplot(FallTempMeanPred,aes(x=Year,y=fit))+
  geom_point(data=FallWx,
             mapping=aes(x=Year, y=TempMean_degC), size=2.5, alpha=0.7) +
  geom_line(size=1)+
  geom_ribbon(aes(ymin = (lower), ymax = (upper), x = Year), alpha = 0.5, inherit.aes = FALSE) +
  labs(x="Water Year",y="Mean Sept temperature (C)")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))






# Fitting GAMs for cumulative mean fall temperature -------------------------------------------

### I added Family Gamma here for how errors should respond
modFallTempMean <- gamm(cumMeanDailyT_OctNovDec ~ s(water_year),
                        data = MohonkIceWeather,
                        correlation = corCAR1(form = ~ water_year),
                        method = "REML")

## summary object
modFallTempMean_S <- summary(modFallTempMean$gam)
modFallTempMean_S #Gives you the P values, degrees of freedom...

###Since we're concerned with the response, include "response" in type of predict()
FallTempMeanPred <- with(MohonkIceWeather, data.frame(water_year = seq(min(water_year, na.rm=TRUE),
                                                                       max(water_year, na.rm=TRUE),
                                                                       length.out = 200)))
FallTempMeanPred <- cbind(FallTempMeanPred, data.frame(predict(modFallTempMean$gam, FallTempMeanPred,
                                                               type="response",
                                                               se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
FallTempMeanPred <- transform(FallTempMeanPred, upper = fit + (2 * se.fit),
                              lower = fit - (2 * se.fit))

# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "water_year"
m1.d <- Deriv(modFallTempMean)

m1.dci <- confint(m1.d, term = "water_year")
m1.dsig <- signifD(FallTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

ylim <- with(FallTempMeanPred, range(upper, lower, fit))
ylab <- 'Mean fall temp (water year)'

plot(fit ~ water_year, data = FallTempMeanPred, type = "n", ylab = ylab, ylim = ylim)
lines(fit ~ water_year, data = FallTempMeanPred)
lines(upper ~ water_year, data = FallTempMeanPred, lty = "dashed")
lines(lower ~ water_year, data = FallTempMeanPred, lty = "dashed")
lines(unlist(m1.dsig$incr) ~ water_year, data = FallTempMeanPred, col = "blue", lwd = 3)
lines(unlist(m1.dsig$decr) ~ water_year, data = FallTempMeanPred, col = "red", lwd = 3)
#Similar acceleration mid-1980s to 2010s. 

#But another way to visualize it is there would be a significant period of change if the error bar around
#the first derivative didn't overlap the horizontal black line.
plot.Deriv(m1.d)

#Plot Ice on DOY vs. year pretty
ggplot(FallTempMeanPred,aes(x=water_year,y=fit))+
  geom_point(data=MohonkIceWeather,
             mapping=aes(x=water_year, y=cumMeanDailyT_OctNovDec), size=2.5, alpha=0.7) +
  geom_line(size=1)+
  geom_ribbon(aes(ymin = (lower), ymax = (upper), x = water_year), alpha = 0.5, inherit.aes = FALSE) +
  labs(x="Water year",y="Mean fall temperature (C))")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))



# Fitting GAMs for cumulative mean spring temperature -------------------------------------------

### I added Family Gamma here for how errors should respond
modspringTempMean <- gamm(cumMeanDailyT_FebMarApr ~ s(Year),
                          data = MohonkIceWeather,
                          correlation = corCAR1(form = ~ Year),
                          method = "REML")

## summary object
modspringTempMean_S <- summary(modspringTempMean$gam)
modspringTempMean_S #Gives you the P values, degrees of freedom...

###Since we're concerned with the response, include "response" in type of predict()
springTempMeanPred <- with(MohonkIceWeather, data.frame(Year = seq(min(Year, na.rm=TRUE),
                                                                   max(Year, na.rm=TRUE),
                                                                   length.out = 200)))
springTempMeanPred <- cbind(springTempMeanPred, data.frame(predict(modspringTempMean$gam, springTempMeanPred,
                                                                   type="response",
                                                                   se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
springTempMeanPred <- transform(springTempMeanPred, upper = fit + (2 * se.fit),
                                lower = fit - (2 * se.fit))

# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "Year"
m1.d <- Deriv(modspringTempMean)

m1.dci <- confint(m1.d, term = "Year")
m1.dsig <- signifD(springTempMeanPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

ylim <- with(springTempMeanPred, range(upper, lower, fit))
ylab <- 'Mean spring temp (water year)'

plot(fit ~ Year, data = springTempMeanPred, type = "n", ylab = ylab, ylim = ylim)
lines(fit ~ Year, data = springTempMeanPred)
lines(upper ~ Year, data = springTempMeanPred, lty = "dashed")
lines(lower ~ Year, data = springTempMeanPred, lty = "dashed")
lines(unlist(m1.dsig$incr) ~ Year, data = springTempMeanPred, col = "blue", lwd = 3)
lines(unlist(m1.dsig$decr) ~ Year, data = springTempMeanPred, col = "red", lwd = 3)
#Substantial spring warming since mid 70s to mid 2010s

#But another way to visualize it is there would be a significant period of change if the error bar around
#the first derivative didn't overlap the horizontal black line.
plot.Deriv(m1.d)

#Plot Ice on DOY vs. year pretty
ggplot(springTempMeanPred,aes(x=Year,y=fit))+
  geom_point(data=MohonkIceWeather,
             mapping=aes(x=Year, y=cumMeanDailyT_FebMarApr), size=2.5, alpha=0.7) +
  geom_line(size=1)+
  geom_ribbon(aes(ymin = (lower), ymax = (upper), x = Year), alpha = 0.5, inherit.aes = FALSE) +
  labs(x="Water year",y="Cumulative mean spring temperature (C))")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))





# Ice-off TS - 1 predictor models--------------------------------------------------------
#Caldwell et al. 2020 used the cumulative sum of mean daily T, snow or rain fraction, downward shortwave radiation, and wind speed
#for winter+spring (1oct-31may) or spring only (1March-31may). Let's try something similar.
#In our case, spring is defined as 1March-30Apr because the median ice-off day is ~April 7th.
#Winter+spring is 1Oct-30Apr

# ~ Ice-off , xreg="cumSnow_spring" ------------------------------------------------------
MohonkWeatherSummary_winter_naFREE<-MohonkIceWeather %>%
  drop_na(IceOutDayofYear, cumSnow_FebMarApr, cumMeanDailyT_FebMarApr)

arimaFit.SpringSnow<-auto.arima(MohonkWeatherSummary_winter_naFREE$IceOutDayofYear,
                                xreg=c(MohonkWeatherSummary_winter_naFREE$cumSnow_FebMarApr),
                                # xreg=cbind(AnnualData$GlobalTempAnomoly_C,
                                # AnnualData$Year),
                                seasonal=FALSE,allowdrift = FALSE,
                                stationary=TRUE)

arimaFit.SpringSnow #display the model that was fit

SpringSnowcoef<-as.numeric(arimaFit.SpringSnow$coef["xreg"])#extracts the xreg coefficient

IceOut_residuals<-as.numeric(arimaFit.SpringSnow$residuals) #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
SpringSnow_pdq<-paste0(as.numeric(arimaFit.SpringSnow$arma[1]),sep=",",
                       as.numeric(arimaFit.SpringSnow$arma[6]),sep=",",
                       as.numeric(arimaFit.SpringSnow$arma[2]))

#extract aicc value
SpringSnowaicc<-as.numeric(arimaFit.SpringSnow$aicc)


# ~ Ice-off , xreg="cumMeanDailyT_spring" ------------------------------------------------------


arimaFit.SpringDailyTcum<-auto.arima(MohonkWeatherSummary_winter_naFREE$IceOutDayofYear,
                                     xreg=c(MohonkWeatherSummary_winter_naFREE$cumMeanDailyT_FebMarApr),
                                     seasonal=FALSE,allowdrift = FALSE,
                                     stationary=TRUE)

arimaFit.SpringDailyTcum #display the model that was fit

SpringDailyTcumcoef<-as.numeric(arimaFit.SpringDailyTcum$coef["xreg"])#extracts the xreg coefficient

SpringDailyTcumresid<-as.numeric(arimaFit.SpringDailyTcum$residuals)

# arimaFit.Year$residuals #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
SpringDailyTcum_pdq<-paste0(as.numeric(arimaFit.SpringDailyTcum$arma[1]),sep=",",
                            as.numeric(arimaFit.SpringDailyTcum$arma[6]),sep=",",
                            as.numeric(arimaFit.SpringDailyTcum$arma[2]))

#extract aicc value
SpringDailyTcumaicc<-arimaFit.SpringDailyTcum$aicc

# Compile model output  ---------------------------------------------------------

#Column for response variable (Mixing Action in all models)
Response <- "Ice out DOY"

#Vector of predictors used in models 
Predictor <- c("Cumul. spring snow",
               "Cumul. spring temp")

#Pull all xreg interecepts
SpringSnowint<-as.numeric(arimaFit.SpringSnow$coef["intercept"])
SpringDailyTcumint<-as.numeric(arimaFit.SpringDailyTcum$coef["intercept"])

#Vector of xreg coefficients
int <- c(SpringSnowint,
         SpringDailyTcumint)

#Vector of xreg coefficients
coef <- c(SpringSnowcoef,
          SpringDailyTcumcoef)

#vector of AICc values
AICc<-c(SpringSnowaicc, SpringDailyTcumaicc)

#Vector of all p,d,q data
pdq <- c(SpringSnow_pdq,
         SpringDailyTcum_pdq)

#Combine to make a dataframe of model output 
TS_dataframe<-data.frame(Response, Predictor, AICc, int, coef, pdq)




# Ice-off TS - 2 predictor models--------------------------------------------------------

# ~ Ice-off , xreg="cumMeanDailyT_spring + cumSnow_spring" ------------------------------------------------------


arimaFit.TempSnowSpring.cum<-auto.arima(MohonkWeatherSummary_winter_naFREE$IceOutDayofYear,
                                        # xreg=c(AnnualData$Global),
                                        xreg=cbind(MohonkWeatherSummary_winter_naFREE$cumSnow_FebMarApr,
                                                   MohonkWeatherSummary_winter_naFREE$cumMeanDailyT_FebMarApr),
                                        # AnnualData$Year),
                                        seasonal=FALSE,allowdrift = FALSE,
                                        stationary=TRUE)

arimaFit.TempSnowSpring.cum #display the model that was fit



#extract aicc value
TempSnowSpring.cumaicc<-as.numeric(arimaFit.TempSnowSpring.cum$aicc)

#Combine model output into DF
#Column for response variable (Mixing Action in all models)
Response <- "Ice out DOY"

#Vector of predictors used in models 
Predictor <- "Cumul. spring snow + Cumu spring temp"

#Pull all xreg coefficients and intercepts
coef_cumSnow_spring<-as.numeric(arimaFit.TempSnowSpring.cum$coef["xreg1"])#extracts the xreg coefficient
coef_cumMeanDailyT_spring<-as.numeric(arimaFit.TempSnowSpring.cum$coef["xreg2"])#extracts the xreg coefficient
int<-as.numeric(arimaFit.TempSnowSpring.cum$coef["intercept"])#common intercept


#Pull all AICc values
AICc<-as.numeric(arimaFit.TempSnowSpring.cum$aicc)

#Pull pdq values
#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
pdq<-paste0(as.numeric(arimaFit.TempSnowSpring.cum$arma[1]),sep=",",
            as.numeric(arimaFit.TempSnowSpring.cum$arma[6]),sep=",",
            as.numeric(arimaFit.TempSnowSpring.cum$arma[2]))

#Combine to make a dataframe of model output 
TS_dataframe_2factormodels<-data.frame(Response, Predictor, AICc,
                                       int, coef_cumSnow_spring, coef_cumMeanDailyT_spring,
                                       pdq)


TS_data_iceout_table<-bind_rows(TS_dataframe,TS_dataframe_2factormodels)

TS_data_iceout_table_hux <- 
  hux(TS_data_iceout_table) %>% 
  arrange(AICc) %>%
  add_colnames() %>% 
  set_bold(row = 1, col = everywhere, value = TRUE) %>% 
  set_all_borders(TRUE) 

# theme_plain(TS_dataframe_hux) 
# quick_docx(TS_data_iceout_table_hux, file = 'figures/IceOut_TSmodel_output.docx')


MohonkWeatherSummary_winter_naFREE$fittedIceOutDOY<-as.numeric(arimaFit.TempSnowSpring.cum$fitted)

#Predicted values
MohonkWeatherSummary_winter_naFREE %>%
  select(fittedIceOutDOY,cumSnow_FebMarApr,cumMeanDailyT_FebMarApr)%>%
  rename("Cumulative daily precipitation (cm) Feb-Apr"="cumSnow_FebMarApr",
         "Cumulative mean daily spring air T (C) Feb-Apr"="cumMeanDailyT_FebMarApr")%>%
  pivot_longer(-1, names_to="predictorID", values_to="predictorValue")%>%
  ggplot(aes(y=fittedIceOutDOY, x=predictorValue))+
  geom_jitter(shape=21,size=2, color="black", fill="#81b29a", width=0.05)+
  scale_fill_viridis_c(guide = "colourbar")+
  theme_MS() + 
  ylab("Predicted Ice Out DOY ")+
  theme(strip.placement = "outside", #strip placement on outside of x-axis labels and ticks
        panel.spacing=grid::unit(0,"lines"), #squish panels together
        strip.text.x = element_text(margin = margin(0,0,0.2,0, "cm")), #increase strip box size
        axis.title.x=element_blank(),
        panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  facet_wrap(.~predictorID, scales="free_x", switch="x")
ggsave("figures/IceOut_predictors_TSmodelout.png", width=7, height=3,units="in", dpi=300)

#Raw values
MohonkWeatherSummary_winter_naFREE %>%
  select(IceOutDayofYear,cumSnow_FebMarApr,cumMeanDailyT_FebMarApr)%>%
  rename("Cumulative daily precipitation (cm) Feb-Apr"="cumSnow_FebMarApr",
         "Cumulative mean daily spring air T (C) Feb-Apr"="cumMeanDailyT_FebMarApr")%>%
  pivot_longer(-1, names_to="predictorID", values_to="predictorValue")%>%
  ggplot(aes(y=IceOutDayofYear, x=predictorValue))+
  geom_jitter(shape=21,size=2, color="black", fill="#81b29a", width=0.05)+
  scale_fill_viridis_c(guide = "colourbar")+
  theme_MS() + 
  ylab("Ice Out DOY ")+
  theme(strip.placement = "outside", #strip placement on outside of x-axis labels and ticks
        panel.spacing=grid::unit(0,"lines"), #squish panels together
        strip.text.x = element_text(margin = margin(0,0,0.2,0, "cm")), #increase strip box size
        axis.title.x=element_blank(),
        panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  facet_wrap(.~predictorID, scales="free_x", switch="x")
#Looks pretty similar! 


# Ice-off DOY mult. reg. model --------------------------------------------

#Since there is no apparaent autocorrelation in the timeseries, could we use mult. reg. to get an
#easier to interpret model output?

mr_1<-lm(IceOutDayofYear~cumSnow_FebMarApr+cumMeanDailyT_FebMarApr,data=MohonkWeatherSummary_winter_naFREE)
summary(mr_1)
plot(mr_1)

# Leaning toward no but the R2 is pretty high which suggests to me that
# cumulative spring ppt and cumulative daily T are pretty good predictors of ice-off. 


# Ice-on TS - 1 predictor models--------------------------------------------------------

##Note that the trouble with using ENSO/MEI indices is that the data do not span the entire record. 

# ~ Ice-on , xreg="ENSO_index_fall" ------------------------------------------------------
MohonkWeatherSummary_winter_naFREE<-MohonkIceWeather %>%
  drop_na(IceInDayofYear_fed, ENSO_index_fall, cumMeanDailyT_OctNovDec)



arimaFit.ENSOprevFall<-auto.arima(MohonkWeatherSummary_winter_naFREE$IceInDayofYear_fed,
                                  xreg=c(MohonkWeatherSummary_winter_naFREE$ENSO_index_fall),
                                  # xreg=cbind(AnnualData$GlobalTempAnomoly_C,
                                  # AnnualData$Year),
                                  seasonal=FALSE,allowdrift = FALSE,
                                  stationary=TRUE)

#The first argument is the y variable
#The second is xregression variables. I think you need to do xreg=cbind(x1,x2) for multiple
#seasonal=FALSE removes seasonal trends, allowdrift=FALSE disallows a drift component (we want to specify that, I think stationary=TRUE is redundant with allowdrift


arimaFit.ENSOprevFall #display the model that was fit

ENSOprevFallcoef<-as.numeric(arimaFit.ENSOprevFall$coef["xreg"])#extracts the xreg coefficient

IceIn_residuals<-as.numeric(arimaFit.ENSOprevFall$residuals) #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
ENSOprevFall_pdq<-paste0(as.numeric(arimaFit.ENSOprevFall$arma[1]),sep=",",
                         as.numeric(arimaFit.ENSOprevFall$arma[6]),sep=",",
                         as.numeric(arimaFit.ENSOprevFall$arma[2]))

#extract aicc value
ENSOprevFallaicc<-as.numeric(arimaFit.ENSOprevFall$aicc)

# ~ Ice-on , xreg="ENSO_index_fall" ------------------------------------------------------

arimaFit.cumFallPrecip<-auto.arima(MohonkWeatherSummary_winter_naFREE$IceInDayofYear_fed,
                                   xreg=c(MohonkWeatherSummary_winter_naFREE$cumMeanDailyT_OctNovDec),
                                   # xreg=cbind(AnnualData$GlobalTempAnomoly_C,
                                   # AnnualData$Year),
                                   seasonal=FALSE,allowdrift = FALSE,
                                   stationary=TRUE)

#The first argument is the y variable
#The second is xregression variables. I think you need to do xreg=cbind(x1,x2) for multiple
#seasonal=FALSE removes seasonal trends, allowdrift=FALSE disallows a drift component (we want to specify that, I think stationary=TRUE is redundant with allowdrift


arimaFit.cumFallPrecip #display the model that was fit

cumFallPrecipcoef<-as.numeric(arimaFit.cumFallPrecip$coef["xreg"])#extracts the xreg coefficient

IceIn_residuals<-as.numeric(arimaFit.cumFallPrecip$residuals) #residuals

#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
cumFallPrecip_pdq<-paste0(as.numeric(arimaFit.cumFallPrecip$arma[1]),sep=",",
                          as.numeric(arimaFit.cumFallPrecip$arma[6]),sep=",",
                          as.numeric(arimaFit.cumFallPrecip$arma[2]))

#extract aicc value
cumFallPrecipaicc<-as.numeric(arimaFit.cumFallPrecip$aicc)


# Compile model output  ---------------------------------------------------------

#Column for response variable (Mixing Action in all models)
Response <- "Ice in DOY"

#Vector of predictors used in models 
Predictor <- c("Prev. winter ENSO",
               "Cum fall temp")

#Pull all xreg interecepts
ENSOprevFallint<-as.numeric(arimaFit.ENSOprevFall$coef["intercept"])
cumFallPrecipint<-as.numeric(arimaFit.cumFallPrecip$coef["intercept"])

#Vector of xreg coefficients
int <- c(ENSOprevFallint,
         cumFallPrecipint)

#Vector of xreg coefficients
coef <- c(ENSOprevFallcoef,
          cumFallPrecipcoef)

#vector of AICc values
AICc<-c(ENSOprevFallaicc,
        cumFallPrecipaicc)

#Vector of all p,d,q data
pdq <- c(ENSOprevFall_pdq,
         cumFallPrecip_pdq)

#Combine to make a dataframe of model output 
TS_dataframe_IceIn<-data.frame(Response, Predictor, AICc, int, coef, pdq)



# Ice-on TS - 2 predictor models--------------------------------------------------------

# ~ Ice-on , xreg="ENSO_index_fall + cumMeanDailyT_OctNovDec" ------------------------------------------------------


arimaFit.ENSOcumT<-auto.arima(MohonkWeatherSummary_winter_naFREE$IceInDayofYear_fed,
                              # xreg=c(AnnualData$Global),
                              xreg=cbind(MohonkWeatherSummary_winter_naFREE$ENSO_index_fall,
                                         MohonkWeatherSummary_winter_naFREE$cumMeanDailyT_OctNovDec),
                              # AnnualData$Year),
                              seasonal=FALSE,allowdrift = FALSE,
                              stationary=TRUE)

arimaFit.ENSOcumT #display the model that was fit



#extract aicc value
ENSOcumTaicc<-as.numeric(arimaFit.ENSOcumT$aicc)

#Combine model output into DF
#Column for response variable (Mixing Action in all models)
Response <- "Ice in DOY (fed)"

#Vector of predictors used in models 
Predictor <- "Prev. fall ENSO + cum. fall air T"

#Pull all xreg coefficients and intercepts
coef_WinterENSO<-as.numeric(arimaFit.ENSOcumT$coef["xreg1"])#extracts the xreg coefficient
coef_cumMeanDailyT_OctNovDec<-as.numeric(arimaFit.ENSOcumT$coef["xreg2"])#extracts the xreg coefficient
int<-as.numeric(arimaFit.ENSOcumT$coef["intercept"])#common intercept


#Pull all AICc values
AICc<-as.numeric(arimaFit.ENSOcumT$aicc)

#Pull pdq values
#Gets the (p,d,q) model fit where p is the autoregressive terms, d is the integrative, and q is the moving average
pdq<-paste0(as.numeric(arimaFit.ENSOcumT$arma[1]),sep=",",
            as.numeric(arimaFit.ENSOcumT$arma[6]),sep=",",
            as.numeric(arimaFit.ENSOcumT$arma[2]))

#Combine to make a dataframe of model output 
TS_dataframe_2factormodels_IceIn<-data.frame(Response, Predictor, AICc,
                                             int, coef_WinterENSO, coef_cumMeanDailyT_OctNovDec,
                                             pdq)


TS_data_iceout_table_IceIn<-bind_rows(TS_dataframe_IceIn,TS_dataframe_2factormodels_IceIn)

TS_data_iceout_table_IceIn_hux <- 
  hux(TS_data_iceout_table_IceIn) %>% 
  arrange(AICc) %>%
  add_colnames() %>% 
  set_bold(row = 1, col = everywhere, value = TRUE) %>% 
  set_all_borders(TRUE) 

# theme_plain(TS_dataframe_hux) 
# quick_html(TS_data_iceout_table_IceIn_hux, file = 'figures/MohonkWinter/IceIn_TSmodel_output.html')
# quick_docx(TS_data_iceout_table_IceIn_hux, file = 'figures/MohonkWinter/IceIn_TSmodel_output.docx')



MohonkWeatherSummary_winter_naFREE$fittedIceInDOY<-as.numeric(arimaFit.ENSOcumT$fitted)

#Predicted values
MohonkWeatherSummary_winter_naFREE %>%
  select(fittedIceInDOY,ENSO_index_fall,cumMeanDailyT_OctNovDec)%>%
  rename("Fall ENSO index"="ENSO_index_fall",
         "Cumulative mean daily fall air T (C)"="cumMeanDailyT_OctNovDec")%>%
  pivot_longer(-1, names_to="predictorID", values_to="predictorValue")%>%
  ggplot(aes(y=fittedIceInDOY, x=predictorValue))+
  geom_jitter(shape=21,size=2, color="black", fill="#81b29a", width=0.05)+
  scale_fill_viridis_c(guide = "colourbar")+
  theme_MS() + 
  ylab("Predicted Ice In DOY (days since Oct 1)")+
  theme(strip.placement = "outside", #strip placement on outside of x-axis labels and ticks
        panel.spacing=grid::unit(0,"lines"), #squish panels together
        strip.text.x = element_text(margin = margin(0,0,0.2,0, "cm")), #increase strip box size
        axis.title.x=element_blank(),
        panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  facet_wrap(.~predictorID, scales="free_x", switch="x")
# ggsave("figures/MohonkWinter/IceIn_predictors_TSmodelout.png", width=5, height=3,units="in", dpi=300)

#Raw values
MohonkWeatherSummary_winter_naFREE %>%
  select(IceInDayofYear_fed,ENSO_index_fall,cumMeanDailyT_OctNovDec)%>%
  rename("Fall ENSO index"="ENSO_index_fall",
         "Cumulative mean daily fall air T (C)"="cumMeanDailyT_OctNovDec")%>%
  pivot_longer(-1, names_to="predictorID", values_to="predictorValue")%>%
  ggplot(aes(y=IceInDayofYear_fed, x=predictorValue))+
  geom_jitter(shape=21,size=2, color="black", fill="#81b29a", width=0.05)+
  scale_fill_viridis_c(guide = "colourbar")+
  theme_MS() + 
  ylab("Ice In DOY (days since Oct 1)")+
  theme(strip.placement = "outside", #strip placement on outside of x-axis labels and ticks
        panel.spacing=grid::unit(0,"lines"), #squish panels together
        strip.text.x = element_text(margin = margin(0,0,0.2,0, "cm")), #increase strip box size
        axis.title.x=element_blank(),
        panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  facet_wrap(.~predictorID, scales="free_x", switch="x")


# Ice-on DOY linear model --------------------------------------------
#I think we are violating the assumptions of normality? so better to go with the non-parametric TS models. 

mr_2a<-lm(IceInDayofYear_fed~ENSO_index_fall,data=MohonkIceWeather)
summary(mr_2a)
plot(mr_2a)

mr_2b<-lm(IceInDayofYear_fed~cumMeanDailyT_OctNovDec,data=MohonkIceWeather)
summary(mr_2b)
plot(mr_2b)

mr_2c<-lm(IceInDayofYear_fed~cumMeanDailyT_OctNovDec+ENSO_index_fall,data=MohonkIceWeather)
summary(mr_2c)
plot(mr_2c)

