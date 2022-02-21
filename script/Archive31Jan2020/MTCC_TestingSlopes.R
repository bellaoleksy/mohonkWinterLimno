#Test out sens slopes with NA data


library(trend)
library(zyp)
library(tidyverse)

#Sample data set 
testDF<-data.frame(Year=seq(1985,2017,1))

intercept=45-1985
slope=1

testDF<-testDF%>%mutate(Response=slope*Year+intercept+rnorm(n(), mean=0,sd=5))

plot(Response~Year,data=testDF)

#Linear regression
lm.slope<-summary(lm(Response~Year,data=testDF))$coefficients[2,"Estimate"]
#Sens slope using trend
sens.slope(testDF$Response)
sens.slope<-sens.slope(testDF$Response)$estimates
MannKendall(testDF$Response)

#zyp.sen using zyp
zyp.sen(Response~Year,data=testDF)
senszyp.slope<-zyp.sen(Response~Year,data=testDF)$coefficients[2]

#Same stuff but with missing values
testDF.na<-testDF
  testDF.na$Response[c(4,17)]<-NA

  #Linear regression
  lm.slope<-summary(lm(Response~Year,data=testDF.na))$coefficients[2,"Estimate"]
  #Sens slope using trend
  trend.sens.slope<-sens.slope(testDF.na$Response)$estimates
  #zyp.sen using zyp
  zyp.sens.slope<-zyp.sen(Response~Year,data=testDF.na)$coefficients[2]
  
  ?zyp.sen
  getAnywhere(zyp.sen)

  x<-testDF.na$Year
  y<-testDF.na$Response
  
  #Function for the slope differences
  slope.differences <- function(i, xx, yy, n) (yy[1:(n - i)] - yy[(i + 1):n])/(xx[1:(n - i)] - xx[(i + 1):n])
  
  #Function to calculate the Theil Sen's slope (non-parametric slope)
  #Includes output of p-value, slope and intercept
  #based on a combination of zyp:zyp.sen and trend:sens.slope
  #Implentation:
    #MTCC.sensSlope(testDF.na$Year,testDF.na$Response)
  
  MTCC.sensSlope<-function (x,y) 
  {
    #Figure out the length of the data
    n <- length(x)
    #Get all the pairwise slopes
    slopes <- unlist(lapply(1:(n - 1), slope.differences, x, y, n))
    #Figure out which ones are finite
    sni <- which(is.finite(slopes))
    #Get the median slope of only the finite ones
    slope <- median(slopes[sni])
    #Calculate all the possible intercepts
    intercepts <- y - slope * x
    #Get the median intercept - this is different from zyp.sen because it has na.rm=T
    intercept <- median(intercepts,na.rm=TRUE)
    #Make the y variables into a table
    table <- table(y)
    names(table) <- NULL
    #Adjust table values?
    tadjs <- sum(table * (table - 1) * (2 * table + 5))
    #Not sure what varS signifies
    varS <- (n * (n - 1) * (2 * n + 5) - tadjs)/18
    #Find some boundaries for significance?
    C <- qnorm(1 - (1 -0.95)/2) * sqrt(varS)
    rank.up <- round(((n-1) + C)/2 + 1)
    rank.lo <- round(((n-1) - C)/2)
    rank.d <- sort(slopes[sni])
    lo <- rank.d[rank.lo]
    up <- rank.d[rank.up]
    #mkScore
    S <- 0
    for (j in 1:n) {
       S <- S + sum(sign(y[j] - y[1:j]),na.rm=TRUE)
       }
    
    sg <- sign(S)
    z <- sg * (abs(S) - 1)/sqrt(varS)
    pval <- 2 * min(0.5, pnorm(abs(z), lower.tail = FALSE))
    
    #Compile a list for export
    res <- list(coefficients = c(intercept, slope), 
                residuals = (y - slope * x + intercept), 
                pval=pval,z_stat=z,n=n)
    names(res$coefficients) = c("Intercept", "Year")
    return(res)
  }  

#Test case with no NA   
MTCC.sensSlope(x,y)    
#Test case with NA
MTCC.sensSlope(testDF.na$Year,testDF.na$Response)



getAnywhere(.mkScore)
#Code for .mkScore function
# function (x) 
# {
#   n <- length(x)
#   S <- 0
#   for (j in 1:n) {
#     S <- S + sum(sign(x[j] - x[1:j]))
#   }
#   return(S)
# }

  #Code for zyp.sen
  # function (formula, dataframe) 
  # {
  #   zyp.slopediff <- function(i, xx, yy, n) (yy[1:(n - i)] - 
  #                                              yy[(i + 1):n])/(xx[1:(n - i)] - xx[(i + 1):n])
  #   if (missing(dataframe)) 
  #     dataframe <- environment(formula)
  #   term <- as.character(attr(terms(formula), "variables")[-1])
  #   x <- dataframe[[term[2]]]
  #   y <- dataframe[[term[1]]]
  #   n <- length(x)
  #   if (length(term) > 2) {
  #     stop("Only linear models are accepted")
  #   }
  #   slopes <- unlist(lapply(1:(n - 1), zyp.slopediff, x, y, n))
  #   sni <- which(is.finite(slopes))
  #   slope <- median(slopes[sni])
  #   intercepts <- y - slope * x
  #   intercept <- median(intercepts)
  #   res <- list(coefficients = c(intercept, slope), slopes = slopes, 
  #               intercepts = intercepts, rank = 2, residuals = (y - slope * 
  #                                                                 x + intercept), x = x, y = y)
  #   names(res$coefficients) = c("Intercept", term[2])
  #   class(res) = c("zyp", "lm")
  #   return(res)
  # }

  
  #Source code for trend::sens.slope
  # function (x, conf.level = 0.95) 
  # {
  #   if (!is.numeric(x)) {
  #     stop("'x' must be a numeric vector")
  #   }
  #   na.fail(x)
  #   n <- length(x)
  #   t <- table(x)
  #   names(t) <- NULL
  #   varS <- .varmk(t, n)
  #   k <- 0
  #   d <- rep(NA, n * (n - 1)/2)
  #   for (i in 1:(n - 1)) {
  #     for (j in (i + 1):n) {
  #       k <- k + 1
  #       d[k] <- (x[j] - x[i])/(j - i)
  #     }
  #   }
  #   b.sen <- median(d, na.rm = TRUE)
  #   C <- qnorm(1 - (1 - conf.level)/2) * sqrt(varS)
  #   rank.up <- round((k + C)/2 + 1)
  #   rank.lo <- round((k - C)/2)
  #   rank.d <- sort(d)
  #   lo <- rank.d[rank.lo]
  #   up <- rank.d[rank.up]
  #   S <- .mkScore(x)
  #   sg <- sign(S)
  #   z <- sg * (abs(S) - 1)/sqrt(varS)
  #   pval <- 2 * min(0.5, pnorm(abs(z), lower.tail = FALSE))
  #   cint <- c(lo, up)
  #   attr(cint, "conf.level") <- conf.level
  #   ans <- list(estimates = c(`Sen's slope` = b.sen), statistic = c(z = z), 
  #               p.value = pval, null.value = c(z = 0), alternative = "two.sided", 
  #               data.name = deparse(substitute(x)), method = "Sen's slope", 
  #               parameter = c(n = n), conf.int = cint)
  #   class(ans) <- "htest"
  #   return(ans)
  # }
  # 
  
  methods(sens.slope)
  getAnywhere(sens.slope)
  
  getAnywhere(.varmk)
  #Code for the .varmk function within sens.slope   
  # function (t, n) 
  # {
  #   tadjs <- sum(t * (t - 1) * (2 * t + 5))
  #   varS <- (n * (n - 1) * (2 * n + 5) - tadjs)/18
  #   return(varS)
  # }