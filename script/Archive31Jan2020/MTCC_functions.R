##FUNCTIONS-Mohonk Lake Climate change analysis####
##Updated 09Apr2019 - include spring analysis
##Functions include
    ##NENA.HeatMapOne
    ##ThermoclineDepth
    ##thermocline.Depth.max
    ##buoyancy.freq.profile.max
    ##stability.calc
    ##stderr

#Libraries
if(!require(rLakeAnalyzer)){install.packages("rLakeAnalyzer")}

library(rLakeAnalyzer)


#Functions####

##################Function: NENA.HeatMapOne####
#This function will take our profiles and print a heat map
#This is more generic and will just print out all the profiles
#Includes a string for the title

## create heat map of annual profiles - needs a bit of reformatting (below) before it will work for rLakeAnalyzer
## remove first column of years and last column of "days away from max stray" (not needed for rLakeAnalyzer function)
NENA.HeatMapOne <- function(reformat.annual.profiles,depth.vector,main.title){
  
  #Find rows with less than 2 temperature measurements and delete
  reformat.annual.profiles<-reformat.annual.profiles[rowSums(!is.na(reformat.annual.profiles[,2:length(names(reformat.annual.profiles))])) > 1, ]
  
  ## reformat column headers to be in "wtr_0.0" format
  ## first replace "Temp" with "wtr"
  colnames(reformat.annual.profiles)=sub(pattern="Temp",replacement="wtr",
                                         x=colnames(reformat.annual.profiles))
  
  ## then remove final "m" in column headers
  colnames(reformat.annual.profiles)=c("datetime",unlist(strsplit(colnames(reformat.annual.profiles)[2:ncol(reformat.annual.profiles)],"m")))
  
  #Make dateTime an arbitrary days for the time series so we can look at each profile individually
  reformat.annual.profiles$datetime<-seq(c(reformat.annual.profiles$datetime[1]),
                                         by = "day",
                                         length.out = length(reformat.annual.profiles$datetime))
  
  ## create temporary .txt file needed for wtr.heat.map function
  write.table(reformat.annual.profiles,"TempHeatMapFile.txt",sep="\t")
  
  ## load temporary file
  heat.map.data=load.ts("TempHeatMapFile.txt")
  
  ## delete temporary file from folder
  unlink("TempHeatMapFile.txt")
  
  
  
  
  for(m in 1:nrow(heat.map.data)){
    est.vals=na.approx(unlist(heat.map.data[m,2:ncol(heat.map.data)]),x=depth.vector,na.rm=FALSE)
    heat.map.data[m,2:ncol(heat.map.data)]=est.vals
  }
  
  wtr.heat.map(heat.map.data,main=main.title,zlim=c(3,35))
  
}
##############End: NENA.HeatMapOne#

#####Function: ThermoclineDepth#################
#THERMOCLINE DEPTH
#Calculates the thermocline depth based on a threshold for density (usually 0.3), temp profile (C) and depth profile (m)
#returns thermocline depth in m
thermocline.Depth<-function(depth.array,temp.array,thresh){
  
  #Calculate density array 
  Density<-1000*(1 - (temp.array+288.9414)/(508929.2*(temp.array+68.12963))*(temp.array-3.9863)^2)
  
  #drho_dz is the change in density over depth
  numDepths=length(depth.array)
  dDensdz=rep(NA,numDepths-1)
  for (i in 1:(numDepths - 1)) {
    dDensdz[i] = (Density[i + 1] - Density[i])/(depth.array[i + 1] - depth.array[i])
    #end of for loop to calculating dDensitydz
  }
  
  dDensdz.unlist<-unlist(dDensdz)
  #if the maximum is NA, then return NA (all the values are NA)
  #else if the maximum is less than the threshold, then it is not stratified and return NA
  #Else find the maximum density difference to establish the thermocline depth
  if(max(dDensdz.unlist,na.rm=TRUE)==-Inf){
    metaDepth<-NA
  }else if(max(dDensdz.unlist,na.rm=TRUE)<thresh){
    metaDepth<-NA
    #Old code returns max depth; this might be helpful to know you had reading but not stratified
    #metaDepth<-max(depth.array,na.rm=TRUE)
  }else{
    maxBigChange<-which.max(dDensdz.unlist)
    metaDepth <- mean(c(depth.array[maxBigChange],depth.array[(maxBigChange+1)]))
  }
  #return the max depth
  return(metaDepth)
}
####End of Thermocline function#

#Thermocline depth function - max density####
#Calculates the thermocline depth based on max density rate of change, temp profile (C) and depth profile (m)
thermocline.Depth.max<-function(depth.array,temp.array){
  #Calculate density array 
  Density<-1000*(1 - (temp.array+288.9414)/(508929.2*(temp.array+68.12963))*(temp.array-3.9863)^2)
  
  #Calculate the differences of depth
  dz<-diff(depth.array)
  #Calculate the differences of density
  dDens<-as.numeric(Density[(2):length(Density)] - Density[1:length(Density)-1])
  #First derivative of the density profile
  dDensdz<-dDens/dz
  
  #Find the max rate of change
  maxBigChange<-which.max(dDensdz)
  metaDepth <- mean(c(depth.array[maxBigChange],depth.array[(maxBigChange+1)]))  
  
  return(metaDepth)
  #End of thermocline depth max density  
}
####End of Thermocline function#

##################Function: buoyancy.freq.profile.max################
#FUNCTION Created 02Feb2015 DCR
#Function to calculate the maximum buoyancy frequency for a dataframe of profiles
#Exports a vector of maximum buoyancy frequencies for each profile
#Based on lake analyzer profile calculations and discussion with Jordan Read (02Feb2015)
#about the selection of the maximum
#Units are (1/seconds^2) and represents the local stability of the water column, based on the density gradient drho/dz
buoyancy.freq.profile.max <- function(wtr, depths){
  rhoVar = water.density(wtr)
  n2.vector<-rep(NA,nrow(wtr))
  #Calculate the first derivative of density
  #j<-1
  for(j in 1:nrow(wtr)){
    t1.profile<-as.numeric(rhoVar[j,])
    t.depths<-depths[!is.na(t1.profile)]
    t2.profile <- t1.profile[!is.na(t1.profile)]
    
    #REMOVE NAs here from both the profile and depth
    #Create new depth vector
    n2.vector[j]<-max((9.81/head(t2.profile,-1))*(diff(t2.profile)/diff(t.depths)),na.rm=TRUE)
    
  }
  return(n2.vector)
}
##################End: buoyancy.freq.profile.max################



#####Function: Schmidt Stability######
#SCHMIDT STABILITY
#Calculates the Schmidt stability based bathymetry data, bathymetry depths, depths of thermisters, and a single thermister profile
#returns stability in units of Joules/m2
stability.calc<-function(wtr,depths,bthA,bthD){
  #Create new bathymetry file adding up the previous layers
  #picture a layer cake, all the layers have 0 to 1 depth (surface area)
  
  #Calculate the total surface area
  totalSurfaceArea<-bthA[1]
  
  #Gravity and the dz that we are going to interpolate to
  g = 9.81
  dz = 0.1
  
  #Formula
  #Stability=g/At * Integral from 0 to max depth ((z-z0)*(rho.z-rho.bar)*Az)*dz 
  
  numD = length(wtr)
  
  #expand out the water data to the maximum of the bathymetry data
  #Pick the lowest non NA value for the bottom value
  low.temp=NA
  for(kk in length(wtr):1){if(!is.na(wtr[kk])){low.temp=wtr[kk]
  break}}
  wtr[numD + 1] = low.temp
  depths[numD + 1] = max(bthD)
  
  #Select the minimum depths
  Zo = min(depths)
  Io = which.min(depths)
  
  #Calculate the density of water in the absence of salinity
  Density<-1000*(1 - (wtr+288.9414)/(508929.2*(wtr+68.12963))*(wtr-3.9863)^2)
  
  #Interpolate the depths at dz interval
  layerD = seq(min(depths), max(depths), by = dz)
  #Interpolate the rho's (densities at the layers)
  layerP = approx(depths, Density, layerD)$y
  #Interpolate the bathymetry data
  layerA = approx(bthD, bthA, layerD)$y
  
  #Figure out the volume averaged depth of the lake 
  #This number is useful for the methods section
  Zv = layerD * layerA * dz
  Zcv = sum(Zv)/sum(layerA)/dz
  
  #Set u the integral
  numInt = length(layerA)
  #Stability at each layer
  st = layerA * NaN
  #Weighted rho at each layer
  rho.weighted=layerA * NaN
  #Volume for each weighted rho
  volume.weighted=layerA*NaN
  
  #Figure out the average rho
  for (jj in 1:numInt) {
    A = layerA[jj]
    rho.jj=layerP[jj]
    rho.weighted[jj]=A*rho.jj*dz
    volume.weighted[jj]<-A*dz*!is.na(rho.weighted[jj])
  }
  Mean.rho<-sum(rho.weighted,na.rm=TRUE)/sum(volume.weighted,na.rm=TRUE)
  
  #Loop to add up all the stability values
  for (i in 1:numInt) {
    z = layerD[i]
    A = layerA[i]
    diffRho<-layerP[i]-Mean.rho
    st[i] = (z - Zcv) * diffRho * A * dz
  }
  St = (g/totalSurfaceArea) * sum(st,na.rm=TRUE)
  return(St)
  
  #End of stability function  
}
####End of Schmidt stability#

#####Function: Standard Error####
#Standard error
#Calculates the standard error
#returns SE
stderr <- function(x) {
  sd(x,na.rm=TRUE)/sqrt(length(na.omit(x)))
}
####End of standard error#

#Function: slope differences####
#Calculates all pairwise slope differnces
#Used in Theil sen slope function: MTCC.sensSlope
slope.differences <- function(i, xx, yy, n){ (yy[1:(n - i)] - yy[(i + 1):n])/(xx[1:(n - i)] - xx[(i + 1):n])}

#Function: Theil Sen's slope (non-parametric slope)####
#Can deal with NAs in the data frame
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
