#R file to set working directories and load in data####
#Created 20Dec2019 DCR and IAO

#DCR working directory
# setwd("C:/Users/richardd/Google Drive/MohonkLongTermAnalysis")

# IAO working directory
# setwd("~/Google Drive/Collaborations/(1) Mohonk Lake/MohonkLongTermAnalysis")

#Loads all functions####
source('script/MTCC_functions.R')
#QAQC's data####
source('script/MTCC_QAQC.R')
#Run the munging file####
  #This modifies and summarizes all the data
source('script/MTCC_munging.R')

#Run the first analysis - sometimes this is necessary for using some of the outputs like Sen's slopes####
source('script/MTCC_Analysis1_OverallTrendsAndSummaryFigures.R')

