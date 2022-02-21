
########## Mohonk project ###########


# random forest 

# install.packages("randomForest")
library(tidyverse)
library(randomForest)
# install.packages("tree")
library(tree)
library(partykit)
library(party)


hist(MohonkIceWeather$IceOutDayofYear)
hist(MohonkIceWeather$IceInDayofYear_fed)
hist(MohonkIceWeather$LengthOfIceCover_days)
ncol(MohonkIceWeather)
glimpse(MohonkIceWeather)

#Need to trim the data and remove missing values.
#If you don't pair down the list of variables then all the 
#colinear predictors appear at the top

data.mo.IceOut <- MohonkIceWeather %>%
  select(
    IceOutDayofYear,
    cumMeanDailyT_FebMar,
    nDaysMeanAboveZero_FebMar,
    nDaysMeanAboveZero_Mar,
    percPrecipRain_Mar,
    cumMeanDailyT_Apr,
  ) %>%
  drop_na()

mo.rf <-
  randomForest(
    IceOutDayofYear ~ .,
    data = data.mo.IceOut,
    mtry = 2,
    importance = TRUE,
    na.action = na.omit
  )
print(mo.rf) #54% explained
out <- data.frame(round(importance(mo.rf), 2))
out %>%
  arrange(desc(X.IncMSE))
mo.rf.tree <- getTree(mo.rf, k = 1, labelVar = TRUE)

#Condition RF
cf <-
  cforest(IceOutDayofYear ~ .,
          data = data.mo.IceOut,
          controls = cforest_control(mtry = 10, mincriterion = 0))

pt <-
  party:::prettytree(cf@ensemble[[1]], names(cf@data@get("input")))
pt
nt <- new("BinaryTree")
nt@tree <- pt
nt@data <- cf@data
nt@responses <- cf@responses
nt
plot(nt)


data.mo.IceIn<- MohonkIceWeather %>%
  select(IceInDayofYear_fed,
         cumMeanDailyT_OctNovDec,
         cumMeanDailyT_Dec,
         percPrecipRain_Nov,
         nDaysMinBelowZero_Dec,
         cumSnow_Nov)



mo.rf2 <- randomForest(IceInDayofYear_fed ~ ., data = data.mo.IceIn, importance=TRUE, na.action = na.omit)
print(mo.rf2) #41% explained
out2 <- round(importance(mo.rf2), 2) 


# write.csv(out, 'out.csv')
# write.csv(out2, 'out2.csv')

m1 <- lm(cumMeanDailyT_Mar ~ Year, data = MohonkIceWeather) # sign increasing
m2 <- lm(percPrecipRain_Mar ~ Year, data = MohonkIceWeather)
m3 <- lm(nDaysMeanBelowZero_Feb ~ Year, data = MohonkIceWeather) #sig decreasing
m4 <- lm(maxSnowDepth_mm ~ Year, data = MohonkIceWeather)
m5 <- lm(cumSnow_FebMar ~ Year, data = MohonkIceWeather)
m6 <- lm(cumMeanDailyT_OctNov ~ Year, data = MohonkIceWeather)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)








