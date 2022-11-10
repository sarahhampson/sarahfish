#### My first trial using Pandolfi et al.'s novelty detection framework ####

############################################################################
#### This method will detect emergence of novel fish communities in the   ####
#### RivFishTIME time series database. So far there are 3 steps involved  ####

################# STEP 1: SETUP ####################

# Clear environment
rm(list = ls())

# Set working directory
#setwd("YOUR OWN CHOICE HERE")
setwd("~/Documents/UNI/2022/Honours/BIOL6502/Data/sarahfish")

# Obtain required functions from 'functions' subfolder
sapply(list.files("./Functions", pattern="\\.R", full.names=TRUE), 
       source)

# Load packages
install.packages(c("mgcv", "vegan", "lme4", "nlme", 
                   "DHARMa", "merTools", "shape",
                   "multcomp", "maptools", "sp", 
                   "divDyn", "plotrix", "raster",
                   "rgeos", "fun", "analogue",
                   "brms", "data.table", "tidyverse,
                   dyplr, stringr"))

# RivFishTIME data
time_series_data <-read.csv("inputs/1873_2_RivFishTIME_TimeseriesTable.csv")
survey_data <- read.csv("inputs/1873_2_RivFishTIME_SurveyTable.csv")

# Create a list to hold these, serves as input for the matrix creator
#ID_list <- split(time_series_data, time_series_data$BioRealm)

##### STEP 1: FEW TIMESERIES #####
# Run analysis on a few time series

#Select a few nice and easy time series
TSG111 <- survey_data[survey_data$TimeSeriesID=="G111",]
TSG10189 <- survey_data[survey_data$TimeSeriesID=="G10189",]
TSG10568 <-survey_data[survey_data$TimeSeriesID=="G10568",]

# Convert to matrix and insert 0 for NA values
TSG111.mat <- tapply(TSG111$Abundance, list(TSG111$Year, TSG111$Species), mean, na.rm=TRUE)
TSG111.mat[is.na(TSG111.mat)] <- 0
TSG10189.mat <- tapply(TSG10189$Abundance, list(TSG10189$Year, TSG10189$Species), mean, na.rm=TRUE)
TSG10189.mat[is.na(TSG10189.mat)] <- 0
TSG10568.mat <- tapply(TSG10568$Abundance, list(TSG10568$Year, TSG10568$Species), mean, na.rm=TRUE)
TSG10568.mat[is.na(TSG10568.mat)] <- 0

# Use identify.novel.GAM function
G111output <- identify.novel.gam(TSG111.mat, alpha = 0.05, metric = "bray", site = "G111", plot = TRUE, plot.data = TRUE,
                   gam.max.k = -1)
G10189output <- identify.novel.gam(TSG10189.mat, alpha = 0.05, metric = "bray", site = "G10189", plot = TRUE, plot.data = TRUE,
                                 gam.max.k = -1)
G10568output <- identify.novel.gam(TSG10568.mat, alpha = 0.05, metric = "bray", site = "G10189", plot = TRUE, plot.data = TRUE,
                                   gam.max.k = -1)

# Save in output folder
write.csv(G111output, "./outputs/G111output.csv", row.names = FALSE)

##### STEP 3: MORE TIMESERIES ####
# First I need to find the different time series which can feasibly be used to analyse

# Create table of timeseries with consecutive yearly surveys
consecutive_year_table <- survey_data %>% group_by(TimeSeriesID) %>%
  summarise(number_of_years = length(unique(Year)), first_year = min(Year), last_year = max(Year),
            first_to_last = last_year - first_year, full_data = (number_of_years == first_to_last), 
            species_count = length(unique(Species))) %>%
  filter(full_data == TRUE, number_of_years >= 10, species_count > 1)

# Alternatively, create table of timeseries with at least ten years of data
overten_year_table <- survey_data %>% group_by(TimeSeriesID) %>%
  summarise(number_of_years = length(unique(Year)), first_year = min(Year), last_year = max(Year),
            full_data = (number_of_years >= 10), 
            species_count = length(unique(Species))) %>%
  filter(full_data == TRUE, species_count > 1)

# Modify survey data to only include valid time series data (10+ consecutive, and 10+ total)
consecutive_yr_surveydata <- survey_data[survey_data$TimeSeriesID %in% consecutive_year_table$TimeSeriesID,]
overten_yr_surveydata <- survey_data[survey_data$TimeSeriesID %in% overten_year_table$TimeSeriesID,]

# Convert culled time series datasets into lists of matrices
consecutive_yr_matrix_list <- 


# Create empty list
multi_timeseries_results <- list()

# Create for loop to go through qualified time series IDs
start_time <- Sys.time()
for(i in 1:length(consecutive_year_table$TimeSeriesID)){
  # Make easier variable names
  TS_data <- survey_data_culled[survey_data_culled$TimeSeriesID==consecutive_year_table$TimeSeriesID[i],]
  
  # Create species abundance matrix for 'i' time series
  TS_data.mat <- tapply(TS_data$Abundance, list(TS_data$Year, TS_data$Species), mean, na.rm=TRUE)
  
  # Replace NA with 0
  TS_data.mat[is.na(TS_data.mat)] <- 0
  
  # Apply framework 
  TS_output <- identify.novel.gam(TS_data.mat, alpha = 0.05, metric = "bray", 
                                  site = consecutive_year_table$TimeSeriesID[i], plot = TRUE, 
                                  plot.data = TRUE, gam.max.k = -1)
  TS_output <- list(TS_output)
  multi_timeseries_results <- append(multi_timeseries_results, TS_output)
  print(consecutive_year_table$TimeSeriesID[i])
}
end_time <- Sys.time()
run_time <- end_time - start_time
print(run_time)


######## STEP 4: CONSIDERING QUARTERS ########
# Remove spaces from the quarter column
survey_data <- survey_data %>%
  mutate(Quarter = str_replace_all(Quarter, " ", ""))

# Create table of timeseries based on quarter sampled (1, 2, 3 or 4)
quarter_list <- split(survey_data, survey_data$Quarter)
quarter_table <- survey_data %>% group_by(TimeSeriesID) %>%
  summarise(number_of_quarters = length(unique(Quarter)), 
            multi_quarter = (number_of_quarters > 1))



