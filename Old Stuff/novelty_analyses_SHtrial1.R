#### My first trial using Pandolfi et al.'s novelty detection framework ####

############################################################################
#### This method will detect emergence of novel fish communities in the ####
#### RivFishTIME time series database. There are X stages involved      ####

################# STEP 0: SETUP ####################

# Clear environment
#rm(list = ls())

# Set working directory
#setwd("YOUR OWN CHOICE HERE")
setwd("~/Documents/UNI/2022/Honours/BIOL6502/Data/PilotAnalyses")

# Obtain required functions from 'functions' subfolder
sapply(list.files("./Functions", pattern="\\.R", full.names=TRUE), 
       source)

# Load packages
install.packages(c("mgcv", "vegan", "lme4", "nlme", 
                   "DHARMa", "merTools", "shape",
                   "multcomp", "maptools", "sp", 
                   "divDyn", "plotrix", "raster",
                   "rgeos", "fun", "analogue",
                   "brms", "data.table", "tidyverse"))

# RivFishTIME data
time_series_data <-read.csv("inputs/1873_2_RivFishTIME_TimeseriesTable.csv")
survey_data <- read.csv("inputs/1873_2_RivFishTIME_SurveyTable.csv")

# Create a list to hold these, serves as input for the matrix creator
ID_list <- split(time_series_data, time_series_data$BioRealm)

##### STAGE 1: QUALIFIED TS #####
# I want to find time series that are 10 years or more <- will likely turn into a function later

#Tim Stuff
#timesseriesmatlist <- lapply(split(survey_data, survey_data$TimeSeriesID), longtowide_timeseries, bin_width=2)

#### FEW SINGLE TIME SERIES ####
#survey_data <- survey_data[,c(1,3,5,6)]
TSG111 <- survey_data[survey_data$TimeSeriesID=="G111",]
TSG10189 <- survey_data[survey_data$TimeSeriesID=="G10189",]

#Convert to matrix and insert 0 for NA values
TSG111.mat <- tapply(TSG111$Abundance, list(TSG111$Year, TSG111$Species), mean, na.rm=TRUE)
TSG111.mat[is.na(TSG111.mat)] <- 0
TSG10189.mat <- tapply(TSG10189$Abundance, list(TSG10189$Year, TSG10189$Species), mean, na.rm=TRUE)
TSG10189.mat[is.na(TSG10189.mat)] <- 0

# Use identify.novel.GAM function
G111output <- identify.novel.gam(TSG111.mat, alpha = 0.05, metric = "bray", site = "G111", plot = TRUE, plot.data = TRUE,
                   gam.max.k = -1)


#See table of timeseries that qualify for analysis based on no. year criteria
selected_TS <- select_timeseries(survey_data)





