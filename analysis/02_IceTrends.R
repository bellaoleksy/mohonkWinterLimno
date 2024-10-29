


#Run the main script to bring in all data and functions####
# source('analysis/00_main.R')
source('analysis/01_Isotherm.R') #this script already sources 00_main.R

# Set theme ---------------------------------------------------------------


theme_MS <- function () { 
  theme_base(base_size=10) %+replace% 
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill="white", colour=NA, linewidth=1.0),
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


#annotate panel letters inside plot
panelLetter.normal <- data.frame(
  xpos = c(-Inf),
  ypos =  c(Inf),
  hjustvar = c(-0.5) ,
  vjustvar = c(1.5))

# Add isotherm variables to dataframe
MohonkIceWeather <- MohonkIceWeather %>%
  left_join(., Isotherm_WaterYear_dates_IceIn %>%
              select(WaterYear,isotherm_TempMax_degC_17_days_0_degC_WaterYear_date),
            by = c("Year"="WaterYear"),
            relationship ="one-to-one") %>%
  left_join(., Isotherm_WaterYear_dates_IceOut %>%
              select(WaterYear,isotherm_TempMean_degC_29_days_4_degC_WaterYear_date),
            by = c("Year"="WaterYear")) 



# ~~ Summary stats for manuscript --------------------------------------------


# Dates of ice on, off, etc

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
#Dec 27



#Earliest ice-off
as.Date(min(MohonkIceWeather$IceOutDayofYear, na.rm=TRUE), origin="2014-01-02")
#March 11

#Latest ice-off
as.Date(max(MohonkIceWeather$IceOutDayofYear, na.rm=TRUE), origin="2014-01-02")
#May 2

#Median ice-off
as.Date(median(MohonkIceWeather$IceOutDayofYear, na.rm=TRUE), origin="2014-01-02")
#April 9


# ~~Sen slope trends in ice phenology --------------------------------------------------
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
# ~~~ panel A, ice pheno#### 
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


#Add date labels for the y-axis
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


#Prepare data for plotting multiple ice-on and ice-off dates
head(MohonkIce.upload)

MohonkIce_vis <- MohonkIce.upload %>%
  mutate(IceCover_1 = as.numeric(difftime(ICEOUT_1,ICEIN_1,units="days")),
         IceCover_2 = as.numeric(difftime(ICEOUT_2,ICEIN_2,units="days")),
         IceCover_3 = as.numeric(difftime(ICEOUT_3,ICEIN_3,units="days")),
         water_year = dataRetrieval::calcWaterYear(ICEIN_1)) %>%
  rowwise() %>%
  mutate(IceCover_sum = sum(c_across(IceCover_1:IceCover_3), na.rm = TRUE),
         IceCover_sum = case_when(IceCover_sum==0 ~ NA,
                                  TRUE ~ IceCover_sum)) %>%
  mutate(ICEIN_1_month=month(ICEIN_1),
         ICEIN_1_day=day(ICEIN_1),
         ICEIN_1_newdate=case_when(ICEIN_1_month %in% c("10","11","12") ~ ymd(paste("2014", ICEIN_1_month, ICEIN_1_day, sep = "-")),
                                   ICEIN_1_month %in% c("1","2","3","4") ~ ymd(paste("2015", ICEIN_1_month, ICEIN_1_day, sep = "-"))),
         ICEOUT_1_month=month(ICEOUT_1),
         ICEOUT_1_day=day(ICEOUT_1),
         ICEOUT_1_newdate=case_when(ICEOUT_1_month %in% c("10","11","12") ~ ymd(paste("2014", ICEOUT_1_month, ICEOUT_1_day, sep = "-")),
                                    ICEOUT_1_month %in% c("1","2","3","4") ~ ymd(paste("2015", ICEOUT_1_month, ICEOUT_1_day, sep = "-"))),
         ICEIN_2_month=month(ICEIN_2),
         ICEIN_2_day=day(ICEIN_2),
         ICEIN_2_newdate=case_when(ICEIN_2_month %in% c("10","11","12") ~ ymd(paste("2014", ICEIN_2_month, ICEIN_2_day, sep = "-")),
                                   ICEIN_2_month %in% c("1","2","3","4") ~ ymd(paste("2015", ICEIN_2_month, ICEIN_2_day, sep = "-"))),
         ICEOUT_2_month=month(ICEOUT_2),
         ICEOUT_2_day=day(ICEOUT_2),
         ICEOUT_2_newdate=case_when(ICEOUT_2_month %in% c("10","11","12") ~ ymd(paste("2014", ICEOUT_2_month, ICEOUT_2_day, sep = "-")),
                                    ICEOUT_2_month %in% c("1","2","3","4") ~ ymd(paste("2015", ICEOUT_2_month, ICEOUT_2_day, sep = "-"))),
         ICEIN_3_month=month(ICEIN_3),
         ICEIN_3_day=day(ICEIN_3),
         ICEIN_3_newdate=case_when(ICEIN_3_month %in% c("10","11","12") ~ ymd(paste("2014", ICEIN_3_month, ICEIN_3_day, sep = "-")),
                                   ICEIN_3_month %in% c("1","2","3","4") ~ ymd(paste("2015", ICEIN_3_month, ICEIN_3_day, sep = "-"))),
         ICEOUT_3_month=month(ICEOUT_3),
         ICEOUT_3_day=day(ICEOUT_3),
         ICEOUT_3_newdate=case_when(ICEOUT_3_month %in% c("10","11","12") ~ ymd(paste("2014", ICEOUT_3_month, ICEOUT_3_day, sep = "-")),
                                    ICEOUT_3_month %in% c("1","2","3","4") ~ ymd(paste("2015", ICEOUT_3_month, ICEOUT_3_day, sep = "-")))) %>%
  group_by(Year) %>%
  mutate(duration_complete = case_when(is.na(IceCover_1)  ~ "no",
                                 TRUE ~ "yes"))

str(MohonkIce_vis)

gg.MohonkIceTrends<-ggplot() +
  geom_segment(
    data = MohonkIce_vis,
    aes(
      x = Year,
      xend = Year,
      y = ICEIN_1_newdate,
      yend = ICEOUT_1_newdate,
      col = IceCover_sum
    )
  ) +
  geom_point(
    data = MohonkIce_vis,
    aes(x = Year, y = ICEIN_1_newdate, fill = IceCover_sum,
        shape = duration_complete),
    color = "black",
    size = 0.75
  ) +
  geom_smooth(
    data = MohonkIce.Predicted.test,
    aes(x = Year, y = IceInDayofYear_yhat_newdate),
    color = "black",
    lty = 1,
    size= 0.5
  ) +
  geom_point(
    data = MohonkIce_vis,
    aes(x = Year,
        y = ICEOUT_1_newdate,
        fill = IceCover_sum,
        shape = duration_complete),
    color = "black",
    size = 0.75
  ) +
  #Add in the intermittant years
  geom_segment(
    data = MohonkIce_vis,
    aes(
      x = Year,
      xend = Year,
      y = ICEIN_2_newdate,
      yend = ICEOUT_2_newdate,
      col = IceCover_sum
    )
  ) +
  geom_point(
    data = MohonkIce_vis,
    aes(x = Year, y = ICEIN_2_newdate, fill = IceCover_sum,
        shape = duration_complete),
    color = "black",
    size = 0.75
  ) +
  geom_point(
    data = MohonkIce_vis,
    aes(x = Year,
        y = ICEOUT_2_newdate,
        fill = IceCover_sum,
        shape = duration_complete),
    color = "black",
    size = 0.75
  ) +
  geom_segment(
    data = MohonkIce_vis,
    aes(
      x = Year,
      xend = Year,
      y = ICEIN_3_newdate,
      yend = ICEOUT_3_newdate,
      col = IceCover_sum
    )
  ) +
  geom_point(
    data = MohonkIce_vis,
    aes(x = Year, y = ICEIN_3_newdate,
        fill = IceCover_sum,
        shape = duration_complete),
    color = "black",
    size = 0.75
  ) +
  geom_point(
    data = MohonkIce_vis,
    aes(x = Year,
        y = ICEOUT_3_newdate,
        fill = IceCover_sum,
        shape = duration_complete),
    color = "black",
    size = 0.75
  ) +
  scale_color_continuous(high = "#1A85FF", low = "#FFC20A",
                         name = "Ice duration\n(days)") +
  scale_fill_continuous(high = "#1A85FF", low = "#FFC20A",
                        name = "Ice duration\n(days)") +
  scale_shape_manual(values=c(25,21))+
  scale_x_continuous(limit = c(1932, 2023),
                     breaks = seq(1940, 2020, by = 20)) +
  scale_y_date(date_breaks = "28 days", date_minor_breaks = "14 days",
               date_labels = "%d-%b")+
  theme_MS() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(
      fill = NA,
      colour = "black",
      linewidth = 1
    ),
    # plot.margin=unit(c(1,0,0,0), "lines"),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black", angle=90, hjust=0.5),
    axis.ticks = element_line(color = "black")
  ) +
  xlab("Year") +
  ylab("Date of ice formation or clearance")+
  guides(shape="none")
gg.MohonkIceTrends
# ggsave(gg.MohonkIceTrends,
#   file="figures/MS/Fig1.IcePhenology_withDates_intermittant.jpg",
#   width = 5,
#   height = 3,
#   units = "in",
#   dpi = 600
# )

#Save the mohonk trends as a object
# save(gg.MohonkIceTrends, file = "output/gg.MohonkIceTrends.rdata")

# ~~~ panel B, map#### 

source("analysis/08_MapMohonk.R")

#Combine the plots and make the top one a little bigger####
temp <-
  plot_grid(
    gg.composite.map,
    NULL,
    gg.MohonkIceTrends,
    align = "v",
    ncol = 1,
    rel_heights = c(2.0 / 6.4, -0.01, 2.5 / 6.4),
    labels = c("a", "", "b")
  )
temp
#Map: 4" wide x 3.4" tall to minimize margins and maintain aspect
#The ice figure has to be 4x2.4 to maintain aspect ratio (down from 5"x3")
ggsave(temp,file="figures/MS/Fig1.MapANDIcePhenology_withDates_intermittant.jpg",  width = 4,
       height = 3.0+2.5,
       units = "in",
       dpi = 600)



# ~~ FIGURE 3a.  Ice On Predictors -----------------------------------------

# ...... Fitting GAMs for iceOnDOY_fed -------------------------------------------

hist(MohonkIceWeather$IceInDayofYear_fed)
IceInVars

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


### IceInDayofYear_fed~Nov + Dec
modIceOn3 <- gam(IceInDayofYear_fed ~  s(cumMeanDailyT_Nov) + s(cumMeanDailyT_Dec),
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
modIceOn4 <- gam(IceInDayofYear_fed ~  s(cumMeanDailyT_Nov) + s(cumMeanDailyT_Dec) + s(NAO_index_Nov),
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

# ...... Final Ice On model ----------------------------------------------------

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
summary(modIceOn4)$dev.expl #CumNov, CumDec, NAONov
summary(modIceOn5)$dev.expl #Fall isotherm
summary(modIceOn6)$dev.expl #Fall Isotherm, CumNov





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
  geom_ribbon(aes(ymin = lwr_ci_isotherm, ymax = upr_ci_isotherm),
              fill="lightgrey", color="darkgrey") +
  geom_line() +
  geom_point(data=MohonkIceWeather,
             aes(x=isotherm_TempMax_degC_17_days_0_degC_WaterYear_date,
                                        y=IceInDayofYear_fed),
             shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
  labs(x="Fall isotherm date",
    # x=expression(Iso["max,"]["17day,"]["0°C"]),
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
  geom_ribbon(aes(ymin = lwr_ci_Nov, ymax = upr_ci_Nov),
              fill="lightgrey", color="darkgrey") +
  geom_line() +
  geom_point(data=MohonkIceWeather,
             aes(x=cumMeanDailyT_Nov,
                 y=IceInDayofYear_fed),
             shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
  labs(x="Nov. cumulative\nmean daily temp. (°C)",
       y="Ice-on date")+
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




Row1a<-(IceOn_isotherm+IceOn_CumuNov)
Row1a


# ggsave("figures/Figure2.GamPredictions_IceOn.png", plot=Row1a, width=8, height=4,units="in", dpi=300)
# 
# ggsave("figures/Figure2.GamPredictions_IceOn.jpg", plot=Row1a, width=180, height=120,units="mm", dpi=300)

# ~~ FIGURE 3b  Ice Off Predictors --------------------------------------

# ...... Fitting GAMs for IceOutDayofYear -------------------------------------------

#Distribution of y
hist(MohonkIceWeather$IceOutDayofYear)
IceOutVars

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
ylab <- 'Ice off DOY (water year)'

plot(fit ~ Year, data = IceOffPred, type = "n", ylab = ylab, ylim = ylim)
lines(fit ~ Year, data = IceOffPred)
lines(upper ~ Year, data = IceOffPred, lty = "dashed")
lines(lower ~ Year, data = IceOffPred, lty = "dashed")
lines(unlist(m1.dsig$incr) ~ Year, data = IceOffPred, col = "blue", lwd = 3)
lines(unlist(m1.dsig$decr) ~ Year, data = IceOffPred, col = "red", lwd = 3)
#Now with some additional data we do a modest negative trends in Ice-off

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


draw(modIceOut11, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals

gam.check(modIceOut11)
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
compareML(modIceOut7, modIceOut10) #ModIceOut7 has lower AIC.



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


jpeg(filename = 'figures/ASLO/FigX.IntxnIsothermSpringSnow.jpg',
     width = 5, height = 5, units = 'in', res = 600)
# graphics.off()
par(bg = "black")
mgcv::vis.gam(modIceOut9, view=c("cumSnow_FebMarApr","isotherm_TempMean_degC_29_days_4_degC_WaterYear_date"),
              plot.type="contour", color="cm", type="response",
              xlab="Cumulative snowfall Feb-Mar (cm)",
              col.lab="white",
              col.axis="white",
              ylab="Spring isotherm (days since Oct 1)") 
axis(1,  col = "white", col.axis = "white")
axis(2,  col = "white", col.axis = "white")
dev.off()

vis.gam(modIceOut9, view=c("cumSnow_FebMarApr","isotherm_TempMean_degC_29_days_4_degC_WaterYear_date"),
        theta= 24, type="response",
        ticktype="detailed",
        color="cm",
        zlab="\nIce Out DOY (Julian day)",
        xlab="\nCumulative snowfall Feb-Mar",
        ylab="\n29-day isotherm > 4 deg") #Adjust theta to get a different view


#  ...... Final Ice Out model ---------------------------------------------------



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
summary(modIceOut7)$dev.expl #CumTempFeb, CumTempMar, CumSnowFebApr, IceInDOY
summary(modIceOut8)$dev.expl #CumTempFebMar, CumSnowFebApr, IceInDOY
summary(modIceOut9)$dev.expl #CumTempFeb, Isotherm, CumSnowFebApr, IceInDOY


modIceOut9_summary<-summary(modIceOut9)
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
  geom_ribbon(aes(ymin = lwr_ci_FebT, ymax = upr_ci_FebT),
              fill="lightgrey", color="darkgrey") +
  geom_line() +
  geom_point(data=MohonkIceWeather, aes(x=cumMeanDailyT_Feb,
                                        y=IceOutDayofYear),
             shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
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
# modIceOut9_summary

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


breaks_IsoAvg_fed<-c(170,190,210) 
as.Date(170-91, origin="2014-01-02")
as.Date(190-91, origin="2014-01-02")
as.Date(210-91, origin="2014-01-02")


IceOut_isotherm<-ggplot(pred_isotherm, aes(x = isotherm_TempMean_degC_29_days_4_degC_WaterYear_date, y = fitted_isotherm)) +
  geom_ribbon(aes(ymin = lwr_ci_isotherm, ymax = upr_ci_isotherm), 
              fill="lightgrey", color="darkgrey") +
  geom_line() +
  geom_point(data=MohonkIceWeather, aes(x=isotherm_TempMean_degC_29_days_4_degC_WaterYear_date,
                                        y=IceOutDayofYear),
             shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
  labs(x="Spring isotherm date",
    # x=expression(Iso["avg,"]["29day,"]["4°C"]),
    # x="Isotherm Formula: TempAvg in degC, 29 day window, 4 degC threshold",
       y="Ice-off date")+
  # scale_y_continuous(breaks = seq(70, 120, by = 10) )+
  # coord_cartesian(ylim = c(65, 120), expand = TRUE)+
  scale_y_continuous(breaks=breaks_IceOffDayofYear_fed,labels=c("13-Mar","23-Mar","02-Apr","12-Apr","22-Apr","02-May"),limits=c(70,120))+
  scale_x_continuous(breaks=breaks_IsoAvg_fed,labels=c("22-Mar","11-Apr","01-May"),limits=c(170,220))+
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
  geom_ribbon(aes(ymin = lwr_ci_FebMarAprSnow, ymax = upr_ci_FebMarAprSnow),
              fill="lightgrey", color="darkgrey") +
  geom_line() +
  geom_point(data=MohonkIceWeather, aes(x=cumSnow_FebMarApr,
                                        y=IceOutDayofYear),
             shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
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
  geom_ribbon(aes(ymin = lwr_ci_IceIn, ymax = upr_ci_IceIn),
              fill="lightgrey", color="darkgrey") +
  geom_line() +
  geom_point(data=MohonkIceWeather, aes(x=IceInDayofYear_fed,
                                        y=IceOutDayofYear),
             shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
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





Row2a<-(IceOut_isotherm/IceOut_FebT/IceOut_FebMarAprSnow)
Row2a


# ggsave("figures/Figure3.GamPredictions_IceOff.png", plot=Row2a, width=8, height=5,units="in", dpi=600)
## For ASLO
# Row2a <- (IceOut_isotherm/IceOut_FebT/IceOut_FebMarAprSnow) & 
#   ggdark::dark_theme_bw(base_size=10)
# Row2a
# ggsave(
#   "figures/ASLO/Fig3B.IceOffPredictors.jpg",
#   width = 3,
#   height = 5,
#   units = "in",
#   dpi = 600
# )



# ~~>> FIGURE 3 -- final export --------------------------------------------------


Combined23 <-(IceOn_isotherm+IceOn_CumuNov+plot_spacer())/(IceOut_isotherm+IceOut_FebT+
                                                             IceOut_FebMarAprSnow+  theme(axis.text.y=element_blank(),
                                                                                          axis.ticks.y=element_blank(),
                                                                                          axis.title.y=element_blank(),
                                                                                          plot.margin=unit(c(0.5,0.5,0,0), "lines"),
                                                                                          axis.ticks.length.y = unit(0, "pt")))
Combined23

# ggsave("figures/Figure2-3.IceOn_IceOff_combined.png", plot=Combined23, width=8, height=8,units="in", dpi=600)

# ggsave("figures/MS/Fig3.IceOn_IceOff_combined.jpg", plot=Combined23, width=180, height=160,units="mm", dpi=300)

ggsave("figures/MS/Fig3.IceOn_IceOff_combined.jpg", plot=Combined23, width=7.2, height=6.4,units="in", dpi=600)

# 7.2 inches = 2 panels wide

# ~~FIGURE 4 ~ IceCoverDuration vs.  ABC ---------------------------------


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
  geom_line(linewidth=1)+
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


## Mod 4
#Try the final model but with accurate duration of ice cover 
#(e.g., not including the intermittant open water periods)

MohonkIce_duration <- MohonkIce.upload %>%
  mutate(IceCover_1 = as.numeric(difftime(ICEOUT_1,ICEIN_1,units="days")),
         IceCover_2 = as.numeric(difftime(ICEOUT_2,ICEIN_2,units="days")),
         IceCover_3 = as.numeric(difftime(ICEOUT_3,ICEIN_3,units="days")),
         water_year = dataRetrieval::calcWaterYear(ICEIN_1)) %>%
  rowwise() %>%
  mutate(IceCover_sum = sum(c_across(IceCover_1:IceCover_3), na.rm = TRUE),
         IceCover_sum = case_when(IceCover_sum==0 ~ NA,
                                  TRUE ~ IceCover_sum)) %>%
  select(Year, IceCover_sum) %>%
  left_join(., MohonkIceWeather)

set.seed(11)
modIceDuration4 <- gam(IceCover_sum ~  s(GlobalTempanomaly_C, k=50) +
                         s(NAO_index_Nov, k=10)+
                         s(NAO_index_Dec, k=10),
                       # family=Gamma(link="log"),
                       family=scat(link="identity"), #for heavy tail
                       data = MohonkIce_duration,
                       # correlation = corCAR1(form = ~ Year),
                       method = "REML")
summary(modIceDuration4)
#Worse fit than NAO_Nov and NAO_Dec separately. 

gam.check(modIceDuration4)
#Want to set k sufficiently high. At default, was getting low p-value for GlobalTempanomaly_C

draw(modIceDuration4, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals

plot(modIceDuration4,
     shift = coef(modIceDuration4)[1],
     pages =1)


# Compare models
compareML(modIceDuration3, modIceDuration4) # Basically indistinguishable?



## Mod 5
#Add ENSO fall
set.seed(11)
modIceDuration5 <- gam(IceCover_sum ~  s(GlobalTempanomaly_C, k=50) +
                         s(NAO_index_Nov, k=10)+
                         # s(NAO_index_Dec, k=10) +
                         s(ENSO_index_fall, k=3),
                       # family=Gamma(link="log"),
                       family=scat(link="identity"), #for heavy tail
                       data = MohonkIce_duration,
                       # correlation = corCAR1(form = ~ Year),
                       method = "REML")
summary(modIceDuration5)
#Worse fit than NAO_Nov and NAO_Dec separately. 

gam.check(modIceDuration5)
#Want to set k sufficiently high. At default, was getting low p-value for GlobalTempanomaly_C

draw(modIceDuration5, residuals = TRUE)
#Partial plots of estimated smooth functions with partial residuals

plot(modIceDuration5,
     shift = coef(modIceDuration5)[1],
     pages =1)


# Compare models
compareML(modIceDuration1, modIceDuration4) # Basically indistinguishable?
compareML(modIceDuration5, modIceDuration4) # Basically indistinguishable?


par(bg = "black")
mgcv::vis.gam(modIceDuration5, view=c("GlobalTempanomaly_C","ENSO_index_fall"),
              plot.type="contour", color="cm", type="response",
              # xlab="Cumulative snowfall Feb-Mar (cm)",
              col.lab="white",
              col.axis="white")
              # ylab="Spring isotherm (days since Oct 1)") 
axis(1,  col = "white", col.axis = "white")
axis(2,  col = "white", col.axis = "white")
dev.off()

vis.gam(modIceDuration5, view=c("GlobalTempanomaly_C","ENSO_index_fall"),
        theta= 60, type="response",
        ticktype="detailed",
        color="cm")
        # zlab="\nIce Out DOY (Julian day)",
        # xlab="\nCumulative snowfall Feb-Mar",
        # ylab="\n29-day isotherm > 4 deg") #Adjust theta to get a different view



#NOW MODEL THE ICE DURATION TREND
#Excluding the days of open water


#Distribution of y
hist(MohonkIceWeather$LengthOfIceCover_days)

### I added Family Gamma here for how errors should respond
modIceDuration0 <- gam(IceCover_sum ~ s(Year),
                       # family=Gamma(link="log"),
                       data = MohonkIce_duration,
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
IceDurationPred <- with(MohonkIce_duration, data.frame(Year = seq(min(Year, na.rm=TRUE),
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
  geom_point(data=MohonkIce_duration,
             mapping=aes(x=Year, y=IceCover_sum), size=2.5, alpha=0.7) +
  geom_line(linewidth=1)+
  geom_smooth(method="lm", se=TRUE) +
  geom_ribbon(aes(ymin = (lower), ymax = (upper), x = Year), alpha = 0.5, inherit.aes = FALSE) +
  labs(x="Year",y="Ice Duration (days)")+
  coord_cartesian(xlim=c(1930,2020))+
  scale_x_continuous(breaks=seq(1930, 2020, 15))

## Does the sens slope change?
MohonkIce_duration %>%
  filter(Year>=1932) %>%
  select(Year, IceCover_sum, IceInDayofYear_fed, IceOutDayofYear) %>%
  pivot_longer(-1) %>%
  group_by(name) %>%
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
  select(-Significance,-Sens_pval)



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
  geom_ribbon(aes(ymin = lwr_ci_GlobalT, ymax = upr_ci_GlobalT),
              fill="lightgrey", color="darkgrey") +
  geom_line() +
  geom_point(data=MohonkIceWeather , aes(x=GlobalTempanomaly_C,
                                        y=LengthOfIceCover_days),
             shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
  labs(x="Global Temperature Anomaly (°C)",
       y="Ice duration (days)")+
  scale_y_continuous(breaks = seq(30, 150, by = 30) )+
  scale_x_continuous(breaks = seq(-0.2, 1.0, by = 0.4) )+
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
  geom_ribbon(aes(ymin = lwr_ci_DecNAO, ymax = upr_ci_DecNAO), 
              fill="lightgrey", color="darkgrey") +
  geom_line() +
  geom_point(data=MohonkIceWeather, aes(x=NAO_index_Dec,
                                        y=LengthOfIceCover_days),
             shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
  labs(x="Dec NAO index",
       y="Length of ice cover (days)")+
  scale_y_continuous(breaks = seq(30, 150, by = 30) )+
  scale_x_continuous(breaks = seq(-250, 150, by = 100) )+
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
  geom_ribbon(aes(ymin = lwr_ci_NovNAO, ymax = upr_ci_NovNAO),
              fill="lightgrey", color="darkgrey") +
  geom_line() +
  geom_point(data=MohonkIceWeather, aes(x=NAO_index_Nov,
                                        y=LengthOfIceCover_days),
             shape=21,fill=rgb(96,98,99,max=255,alpha=150),size=1)+
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



ggsave("figures/MS/Figure4.GamPredictions_IceDuration.png", width=7.2, height=3,units="in", dpi=600)



#ASLO Dark theme
# composite_noLegend<-(IceDuration_GlobalT+IceDuration_DecNAO+IceDuration_NovNAO) &
#   ggdark::dark_theme_bw(base_size=9)
# composite_noLegend
# ggsave(
#   "figures/ASLO/FigX.IceDurationPredictors.jpg",
#   width = 7.2,
#   height = 3,
#   units = "in",
#   dpi = 600
# )






