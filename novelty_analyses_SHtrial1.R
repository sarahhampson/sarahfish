#### My first trial using Pandolfi et al.'s novelty detection framework ####

############################################################################
#### This method will detect emergence of novel fish communities in the ####
#### RivFishTIME time series database. There are X stages involved      ####

################# STEP 0: SETUP AND DATA TRANSFORMATION ####################

# Clear environment
rm(list = ls())

# Set working directory
setwd("YOUR OWN CHOICE HERE")
#setwd("~/Documents/UNI/2022/Honours/BIOL6502/Data/RCode/Fish")

# Obtain required functions from 'functions' subfolder
sapply(list.files("./TimStuff/functions", pattern="\\.R", full.names=TRUE), 
       source)

# Load packages
install.packages(c("mgcv", "vegan", "lme4", "nlme", 
                   "DHARMa", "merTools", "shape",
                   "multcomp", "maptools", "sp", 
                   "divDyn", "plotrix", "raster",
                   "rgeos", "fun", "analogue",
                   "brms", "data.table"))

# RivFishTIME data
timeseries_data <-read.csv("./RCode/Fish/inputs/1873_2_RivFishTIME_TimeseriesTable.csv")
survey_data <- read.csv("./RCode/Fish/inputs/1873_2_RivFishTIME_SurveyTable.csv")

# Simplify some column names
colnames(timeseries_data)[colnames(timeseries_data) == "Age..Ma..Gradstein.et.al..2012"] = "age"
colnames(timeseries_data)[colnames(timeseries_data) == "Site"] = "site"
colnames(timeseries_data)[colnames(timeseries_data) == "Latitude"] = "lat"
colnames(timeseries_data)[colnames(timeseries_data) == "Longitude"] = "long"



