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

theme_set(theme_MS())





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

##Quite a few variables highly correlated ( r > 0.90 ) 
## so I am filtering those out prior to further analyses, but you can welcome to change this!
MohonkIceWeather_trim <- MohonkIceWeather %>%
  select(-cumSnow_SepOct, -cumSnow_SepOctNov, #never any snow in Sept
         -nDaysMeanBelowZero_SepOct, -nDaysMeanBelowZero_SepOctNov, #never any days below zero Sept
         -nDaysMinBelowZero_SepOct, -nDaysMinBelowZero_SepOctNov,
         -nDaysMeanBelowZero_OctNov, #might as well just keep Nov
         -cumSnow_FebMarApr, #probablyy not much snow in April
         -nDaysMinBelowZero_OctNov, #Nov closer to ice-in DOY
         -cumSnow_OctNov, #but keep cumSnow_Nov
         -cumRain_FebMarApr, #probably little rain in Feb
         -cumRain_FebMar, #probably little rain in Feb
         -nDaysMeanAboveZero_MarApr, #Tough call, but warmer earlier will 
                                     #probably drive the trend in an earlier ice-out day
         -cumSnow_MarApr, #probably little snow in April
         -cumRain_SepOctNov, #probably little rain in November
         -cumMeanDailyT_FebMarApr, #Keep cumMeanDailyT_FebMar
         -cumMeanDailyT_SepOctNov, #Keep cumMeanDailyT_OctNov
         -nDaysMeanBelowZero_OctNovDec, #nDaysMeanBelowZero_Dec
         -cumSnow_OctNovDec, #Keep cumSnow_Dec
         -cumRain_OctNovDec, #probably little rain December
         -nDaysMinAboveZero_FebMar, #keep nDaysMinAboveZero_Mar
         -nDaysMinBelowZero_Jan, #keep nDaysMeanBelowZero_Jan
         -cumRain_Oct, #but drop cumRain_OctNov since we have
                       #cumSnow_Nov
         -cumRain_JanFebMar, #probably little rain in Jan
         -cumMeanDailyT_JanFebMar, #but keep cumMeanDailyT_FebMar
         -cumMeanDailyT_MarApr, #but keep cumMeanDailyT_Mar
         -cumSnow_JanFebMar, #but keep cumSnow_FebMar
         -nDaysMinAboveZero_MarApr, #but keep nDaysMinAboveZero_Mar
         -cumRain_MarApr, #but keep cumRain_Mar
  )

## UNSURE about the following pairs, so I am keeping those in for now:
# 	nDaysMeanBelowZero_Dec	nDaysMinBelowZero_Dec	0.86
# 	cumMeanDailyT_Mar	nDaysMinAboveZero_Mar	0.86
# 	cumMeanDailyT_MarApr	nDaysMinAboveZero_MarApr	0.86
# 	cumMeanDailyT_Mar	nDaysMeanAboveZero_Mar	0.83

#Look for potentially spurious correlations
res3 <- rcorr(as.matrix(MohonkIceWeather_trim[,3:ncol(MohonkIceWeather_trim)]))
MohonkIceWeather_trim_correlations<-flattenCorrMatrix(res3$r, res3$P)
MohonkIceWeather_trim_correlations<-MohonkIceWeather_trim_correlations%>%
  filter(row %in% c("IceOutDayofYear","IceInDayofYear_fed","LengthOfIceCover_days")) %>%
  filter(p < 0.05) 

#Take a look at what variables are highly correlated with IceOut and IceIn
MohonkIceWeather_trim_correlations %>%
  group_by(row) %>%
  filter(abs(cor) > 0.5) %>%
  arrange(row) %>%
  filter(row=="IceInDayofYear_fed")

#Look at the top 10 strongest correlations for each predictor variable
MohonkIce_top10<-MohonkIceWeather_trim_correlations %>%
  mutate(cor_abs=abs(cor)) %>%
  arrange(desc(cor_abs)) %>%
  group_by(row) %>%
  slice(1:10)

#Extract variable names 
IceInVars <- MohonkIce_top10 %>% filter(row=="IceInDayofYear_fed") %>% pull(column)
IceOutVars <- MohonkIce_top10 %>% filter(row=="IceOutDayofYear") %>% pull(column)
IceDurationVars <- MohonkIce_top10 %>% filter(row=="LengthOfIceCover_days") %>% pull(column)

#Visualize correlations with IceInDayofYear_fed
MohonkIceWeather %>%
  select(IceInDayofYear_fed, all_of(IceInVars)) %>%
  ggpairs() 
median(MohonkIceWeather$IceInDayofYear,na.rm=T)
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
MohonkIceWeather %>%
  select(IceInDayofYear_fed, cumMeanDailyT_OctNovDec, nDaysMinBelowZero_Nov) %>%
  ggpairs() 

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
MohonkIceWeather %>%
  select(LengthOfIceCover_days, IceOutDayofYear, all_of(IceDurationVars)) %>%
  ggpairs() 
median(MohonkIceWeather$LengthOfIceCover_days,na.rm=T) #100 days




#Just out of curiosity, is there any relationship between days since turnover and IceInDOY?
AnnualData %>%
  mutate(turnoverToIceIn_days=IceInDayofYear-EndOfStratification_Day) %>%
  select(Year,turnoverToIceIn_days,IceInDayofYear) %>%
  ggplot(aes(y=turnoverToIceIn_days,x=Year))+
  geom_point(size=3, shape=21)+
  geom_line(size=0.5)+
  xlab("Year")+
  geom_smooth(method="lm",color="grey50", size=0.5)






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



# ~~FIGURE 1~~ Ice in phenology timeseries ------------------------------------------------------------

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



ggplot() +
  geom_segment(
    data = MohonkIce,
    aes(
      x = Year,
      xend = Year,
      y = IceInDayofYear_fed,
      yend = IceOutDayofYear_fed,
      col = LengthOfIceCover_days
    )
  ) +
  # col="grey")+
  geom_point(
    data = MohonkIce,
    aes(x = Year, y = IceInDayofYear_fed, fill = LengthOfIceCover_days),
    shape = 21,
    color = "black",
    size = 1.5
  ) +
  geom_line(
    data = MohonkIce.Predicted,
    aes(x = Year, y = IceInDayofYear_fed_yhat),
    color = "black",
    lty = 1
  ) +
  geom_point(
    data = MohonkIce,
    aes(x = Year,
        y = IceOutDayofYear_fed,
        fill = LengthOfIceCover_days),
    shape = 21,
    color = "black",
    size = 1.5
  ) +
  scale_color_continuous(high = "green", low = "red",
                         name = "Ice cover\nduration (days)") +
  scale_fill_continuous(high = "green", low = "red",
                        name = "Ice cover\nduration (days)") +
  scale_y_continuous(lim = c(50, 200),
                     breaks = seq(50, 250, by = 25)) +
  scale_x_continuous(limit = c(1930, 2020),
                     breaks = seq(1930, 2020, by = 10)) +
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
    axis.text.y = element_text(color = "black"),
    axis.ticks = element_line(color = "black")
  ) +
  xlab("Year") +
  ylab("Days since Oct 1 (beginning of water-year)")
ggsave(
  "figures/FigX.IcePhenology.png",
  width = 6,
  height = 4,
  units = "in",
  dpi = 300
)

# Fitting GAMs for iceOnDOY_fed -------------------------------------------



### I added Family Gamma here for how errors should respond
modIceOn <- gamm(IceInDayofYear_fed ~ s(Year),
                    family=Gamma(link="log"), data = MohonkIce,
                    correlation = corCAR1(form = ~ Year),
                    method = "REML")

## summary object
modIceOn_S <- summary(modIceOn$gam)
modIceOn_S #Gives you the P values, degrees of freedom...

###Since we're concerned with the response, include "response" in type of predict()
IceOnPred <- with(MohonkIce, data.frame(Year = seq(min(Year, na.rm=TRUE),
                                                         max(Year, na.rm=TRUE),
                                                         length.out = 200)))
IceOnPred <- cbind(IceOnPred, data.frame(predict(modIceOn$gam, IceOnPred,
                                                     type="response",
                                                     se.fit = TRUE)))
### this calculates on the link scale (i.e., log)
IceOnPred <- transform(IceOnPred, upper = fit + (2 * se.fit),
                         lower = fit - (2 * se.fit))


# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "Year"
m1.d <- Deriv(modIceOn)

m1.dci <- confint(m1.d, term = "Year")
m1.dsig <- signifD(IceOnPred$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

ylim <- with(IceOnPred, range(upper, lower, fit))
ylab <- 'Ice on DOY (water year)'

plot(fit ~ Year, data = IceOnPred, type = "n", ylab = ylab, ylim = ylim)
lines(fit ~ Year, data = IceOnPred)
lines(upper ~ Year, data = IceOnPred, lty = "dashed")
lines(lower ~ Year, data = IceOnPred, lty = "dashed")
lines(unlist(m1.dsig$incr) ~ Year, data = IceOnPred, col = "blue", lwd = 3)
lines(unlist(m1.dsig$decr) ~ Year, data = IceOnPred, col = "red", lwd = 3)
#To me this indicates that there is no acceleration, and the rate of change is fairly constant. 

#But another way to visualize it is there would be a significant period of change if the error bar around
#the first derivative didn't overlap the horizontal black line.
plot.Deriv(m1.d)

#Plot Ice on DOY vs. year pretty
ggplot(IceOnPred,aes(x=Year,y=fit))+
  geom_point(data=MohonkIce,
             mapping=aes(x=Year, y=IceInDayofYear_fed), size=2.5, alpha=0.7) +
  geom_line(size=1)+
  geom_ribbon(aes(ymin = (lower), ymax = (upper), x = Year), alpha = 0.5, inherit.aes = FALSE) +
  labs(x="Year",y="Ice on (day of water-year)")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))

# Fitting GAMs for iceDuration_days -------------------------------------------


glimpse(MohonkIce)

### I added Family Gamma here for how errors should respond
modIceDuration <- gamm(LengthOfIceCover_days ~ s(Year),
                 family=Gamma(link="log"), data = MohonkIce,
                 correlation = corCAR1(form = ~ Year),
                 method = "REML")

## summary object
modIceDuration_S <- summary(modIceDuration$gam)
modIceDuration_S #Gives you the P values, degrees of freedom...

###Since we're concerned with the response, include "response" in type of predict()
IceDurationPred <- with(MohonkIce, data.frame(Year = seq(min(Year, na.rm=TRUE),
                                                 max(Year, na.rm=TRUE),
                                                 length.out = 200)))

IceDurationPred <- cbind(IceDurationPred, data.frame(predict(modIceDuration$gam, IceDurationPred,
                                                 type="response",
                                                 se.fit = TRUE)))

### this calculates on the link scale (i.e., log)
IceDurationPred <- transform(IceDurationPred,
                             upper = fit + (2 * se.fit),
                             lower = fit - (2 * se.fit))


# Plots periods of change
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
Term <- "Year"
m2.d <- Deriv(modIceDuration)

m2.dci <- confint(m2.d, term = "Year")
m2.dsig <- signifD(IceDurationPred$fit,
                   d = m2.d[[Term]]$deriv,
                   m2.dci[[Term]]$upper,
                   m2.dci[[Term]]$lower)

ylim <- with(IceDurationPred, range(upper, lower, fit))
ylab <- 'Ice duration (days)'

plot(fit ~ Year, data = IceDurationPred, type = "n", ylab = ylab, ylim = ylim)
lines(fit ~ Year, data = IceDurationPred)
lines(upper ~ Year, data = IceDurationPred, lty = "dashed")
lines(lower ~ Year, data = IceDurationPred, lty = "dashed")
lines(unlist(m2.dsig$incr) ~ Year, data = IceDurationPred, col = "blue", lwd = 3)
lines(unlist(m2.dsig$decr) ~ Year, data = IceDurationPred, col = "red", lwd = 3)
#Ice duration is getting shorter, with no periods of acceleration

#But another way to visualize it is there would be a significant period of change if the error bar around
#the first derivative didn't overlap the horizontal black line.
plot.Deriv(m2.d)

#Plot Ice duration vs. Year pretty
ggplot(IceDurationPred,aes(x=Year,y=fit))+
  geom_point(data=MohonkIce,
             mapping=aes(x=Year, y=LengthOfIceCover_days), size=2.5, alpha=0.7) +
  geom_line(size=1)+
  geom_ribbon(aes(ymin = (lower), ymax = (upper), x = Year), alpha = 0.5, inherit.aes = FALSE) +
  labs(x="Year",y="Ice duration (days)")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))


# Trends in the residuals? ------------------------------------------------
##Ice duration
lm1<-lm(LengthOfIceCover_days~Year, MohonkIce)
iceDurationResiduals<-lm1$model 
iceDurationResiduals <- iceDurationResiduals %>%
  mutate(residuals=lm1$residuals)
iceDurationResiduals %>%
  ggplot(aes(Year,residuals))+geom_point()

##Ice on
lm2<-lm(IceInDayofYear_fed~Year, MohonkIce)
iceOnResiduals<-lm2$model 
iceOnResiduals <- iceOnResiduals %>%
  mutate(residuals=lm2$residuals)
iceOnResiduals %>%
  ggplot(aes(Year,residuals))+geom_point()


##Ice off
lm3<-lm(IceOutDayofYear_fed~Year, MohonkIce)
iceOffResiduals<-lm3$model 
iceOffResiduals <- iceOffResiduals %>%
  mutate(residuals=lm3$residuals)
iceOffResiduals %>%
  ggplot(aes(Year,residuals))+geom_point()
lm3.b<-lm(residuals~Year,iceOffResiduals)
summary(lm3.b)


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

