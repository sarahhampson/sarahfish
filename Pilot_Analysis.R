############################# PILOT ANALYSIS ###############################
###################### Using perfect time series data ######################

# This analysis will detect emergence of novel fish communities using the 
# ideal time series set. Both taxonomic and functional approach are used.

############################# STEP 0: SETUP ################################

# Clear environment
rm(list = ls())                                                     
# Set working directory
#setwd("YOUR OWN CHOICE HERE")
setwd("~/Documents/UNI/2022/Honours/BIOL6502/Data/sarahfish")
# Obtain required functions from 'functions' subfolder
sapply(list.files("./Functions", pattern="\\.R", full.names=TRUE), source)
# Load packages
install.packages(c("mgcv", "vegan", "lme4", "nlme",
                   "DHARMa", "merTools", "shape",
                   "multcomp", "maptools", "sp", 
                   "divDyn", "plotrix", "raster",
                   "rgeos", "fun", "analogue",
                   "brms", "data.table", "dplyr",
                   "tidyverse", "funrar"))

# RivFishTIME data
time_series_data <-read.csv("inputs/1873_2_RivFishTIME_TimeseriesTable.csv")
survey_data <- read.csv("inputs/1873_2_RivFishTIME_SurveyTable.csv")

# Remove spaces from the quarter column
survey_data <- survey_data %>%
  mutate(Quarter = str_replace_all(Quarter, " ", ""))

####################### STEP 1: SELECT TIMESERIES ##########################

# Table of timeseries with min 10 consecutive yearly surveys and 2 species
pilot_ts_table <- survey_data %>% group_by(TimeSeriesID) %>%
  summarise(year_count = length(unique(Year)), first_year = min(Year), 
            last_year = max(Year), first_to_last = last_year - first_year, 
            full_data = (year_count == first_to_last), 
            species_count = length(unique(Species)), 
            quarter_count = length(unique(Quarter))) %>%
  filter(full_data == TRUE, year_count >=10, species_count > 1)
# Many TS are sampled in more than 1 quarter so I will ignore this for now

# Create a list of ideal timeseries IDs
pilot_ts_list <- pilot_ts_table %>%
  dplyr::select(TimeSeriesID)

# Modify survey data to only include timeseries from pilot_ts_table
pilot_survey_data <- survey_data[survey_data$TimeSeriesID %in% 
                                   pilot_ts_list$TimeSeriesID,]
# Remove unused datasets
rm(survey_data, pilot_ts_table)

####################### STEP 3: TAXONOMIC ANALYSIS #########################

# Create a list of abundance matrices using matrix maker function
tax_matrix_list <- taxonomic_matrix_maker(pilot_survey_data, 
                                          pilot_ts_list)

# Apply framework using taxonomic novelty detection function
pilot_taxonomic_results <- tax_novelty_detection(tax_matrix_list)

# Extract all novel columns from list of lists
Tax_novelty_data <- pilot_taxonomic_results %>%
  group_by(pilot_taxonomic_results[[1]])
  
  
# Save output as a table
 








