#R file to set working directories and load in data####

#Loads in libraries/packages####
source('script/00_libraries.R')

#Loads all functions####
source('script/01_functions.R')

#QAQC's data####
source('script/01_QAQC.R')

#Run the munging file####
#This modifies and summarizes all the data
source('script/01_munging.R')

