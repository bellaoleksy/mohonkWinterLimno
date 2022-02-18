
########## Mohonk project ###########


# random forest 

install.packages("randomForest")
library(tidyverse)
library(randomForest)

set.seed(71)
iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        proximity=TRUE)
print(iris.rf)

## Look at variable importance:
round(importance(iris.rf), 2)
## Do MDS on 1 - proximity:
iris.mds <- cmdscale(1 - iris.rf$proximity, eig=TRUE)
op <- par(pty="s")
pairs(cbind(iris[,1:4], iris.mds$points), cex=0.6, gap=0,
      col=c("red", "green", "blue")[as.numeric(iris$Species)],
      main="Iris Data: Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(iris.mds$GOF) 

set.seed(131)
ozone.rf <- randomForest(Ozone ~ ., data=airquality, mtry=3,
                         importance=TRUE, na.action=na.omit)
print(ozone.rf)
## Show "importance" of variables: higher value mean more important:
round(importance(ozone.rf), 2) 

### Mohonk below
install.packages("tree")
library(tree)
# data.mo <- read.csv('data/exported/MohonkSummary_winter.csv')
data.mo <- read.csv('data/exported/MohonkDailyWeather_monthly&seasonal_trim.csv')
# data.mo <- read.csv('Mohonk_021021.csv')
hist(data.mo$IceOutDayofYear)
hist(data.mo$IceInDayofYear_fed)
hist(data.mo$LengthOfIceCover_days)
ncol(data.mo)
glimpse(data.mo)
# data.mo.IceOut<- data.mo %>%
#   select(-X, -water_year, -Year, -IceInDayofYear_fed, -IceInDayofYear) %>%
#   drop_na()
data.mo.IceOut <- data.mo %>%
  select(IceOutDayofYear, cumMeanDailyT_Mar, cumMeanDailyT_FebMar, cumSnow_FebMar,maxSnowDepth_mm, LengthOfIceCover_days, nDaysMeanBelowZero_Jan ) %>%
  drop_na()

mo.rf <- randomForest(IceOutDayofYear ~ ., data = data.mo.IceOut, mtry=2, importance=TRUE, na.action = na.omit)
print(mo.rf) #54% explained
out <- data.frame(round(importance(mo.rf), 2)) 
out %>%
  arrange(desc(X.IncMSE))
mo.rf.tree<-getTree(mo.rf, k=1, labelVar=TRUE)

cf<-cforest(IceOutDayofYear ~ ., data=data.mo.IceOut, controls=cforest_control(mtry=10, mincriterion=0))
# cf<-cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))
# cf <- cforest(Species ~ ., data = iris)
pt <- party:::prettytree(cf@ensemble[[1]], names(cf@data@get("input")))
pt
nt <- new("BinaryTree")
nt@tree <- pt
nt@data <- cf@data
nt@responses <- cf@responses
nt
plot(nt)


data.mo.IceIn<- data.mo %>%
  select(-X, -water_year, -Year, -IceInDayofYear, -LengthOfIceCover_days,
         -NAO_index_spring, -MEI_sum_previouswinter, -cumMeanDailyT_spring)



mo.rf2 <- randomForest(IceInDayofYear_fed ~ ., data = data.mo.IceIn, mtry=10, importance=TRUE, na.action = na.omit)
print(mo.rf2) #10% explained
out2 <- round(importance(mo.rf2), 2) 

mo.rf3 <- randomForest(LengthOfIceCover_days ~ ., data = data.mo, mtry=10, importance=TRUE, na.action = na.omit)
print(mo.rf3) #54% explained
out3 <- round(importance(mo.rf3), 2) 
getTree(mo.rf3)

write.csv(out, 'out.csv')
write.csv(out2, 'out2.csv')
write.csv(out3, 'out3.csv')

m1 <- lm(cumMeanDailyT_Mar ~ Year, data = data.mo) # sign increasing
m2 <- lm(percPrecipRain_Mar ~ Year, data = data.mo)
m3 <- lm(nDaysMeanBelowZero_Feb ~ Year, data = data.mo) #sig decreasing
m4 <- lm(maxSnowDepth_mm ~ Year, data = data.mo)
m5 <- lm(cumSnow_FebMar ~ Year, data = data.mo)
m6 <- lm(cumMeanDailyT_OctNov ~ Year, data = data.mo)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)








