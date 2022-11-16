############################# PILOT ANALYSIS ###############################
##################### Using 'perfect' time series data #####################

# This analysis will detect emergence of novel fish communities using the 
# ideal time series set. Both taxonomic and functional approach are used.

############################# STEP 0: SETUP ################################

# Clear environment
rm(list = ls())
graphics.off()

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
# Many TS are sampled in more than 1 quarter so I will ignore this for now,
# though this may be a filter in future

# Modify survey data to only include timeseries from pilot_ts_table
pilot_survey_data <- survey_data[survey_data$TimeSeriesID %in% 
                                   pilot_ts_table$TimeSeriesID,]

# Modify timeseries to only include time series from pilot_ts_table
pilot_ts_data <- time_series_data[time_series_data$TimeSeriesID %in%
                                    pilot_ts_table$TimeSeriesID,]

# Remove unused datasets
rm(survey_data, time_series_data, pilot_ts_table)

################### STEP 3: TAXONOMIC NOVELTY DETECTION ####################

# Create a list of relative abundance matrices using matrix maker function
tax_matrix_list <- taxonomic_matrix_maker(pilot_survey_data, 
                                          pilot_ts_data)

# Apply framework using taxonomic novelty detection function
pilot_tax_results <- tax_novelty_detection(tax_matrix_list)

# Empty plots folder if wanted
unlink("./plots/Pilot Taxonomic Plots/*")

#################### STEP 4: TAXONOMIC SUMMARY STATS #######################

# Convert results list to a dataframe
pilot_tax_results.df <- do.call("rbind", pilot_tax_results)

# I need to summarise data:
# 1. Count of T/F novelty detection across all time series
# 2. Probability of novelty detection in this pilot dataset
# 3. Probability of instantaneous AND cumulative novelty

# Novelty count result and store as dataframe
tax_novelty_table <- as.data.frame.matrix(table(pilot_tax_results.df$site, pilot_tax_results.df$cat))

# Loop over to get success failure columns and site ID
tax_comm_state <- lapply(1:ncol(tax_novelty_table), function(x) {
  data.frame(site = rownames(tax_novelty_table), 
             success = tax_novelty_table[,x], 
             failure = rowSums(tax_novelty_table[,-x]))
})
# Change names in list to community state
names(tax_comm_state) <- colnames(tax_novelty_table)

# Probability model (log odds)
tax_prob_results <- lapply(tax_comm_state, function(x){
  glm(cbind(success, failure) ~ 1,
         data=x, family=binomial)
})

# Shows log odds results
lapply(tax_prob_results, summary)

# Convert to normal probabilities (or try to)
state.prob <- lapply(tax_prob_results,function(x){
  plogis(tax_prob_results[x,[1]])
})

# Rudimentary probability conversion
back.prob <- plogis(tax_prob_results[["back"]][["coefficients"]])
instant.prob <- plogis(tax_prob_results[["instant"]][["coefficients"]])
cumul.prob <- plogis(tax_prob_results[["cumul"]][["coefficients"]])
novel.prob <- plogis(tax_prob_results[["novel"]][["coefficients"]])

# How to determine confidence intervals prior to plogis function
-4.229+0.105*1.96

# Unweighted probabilities
sum(tax_novelty_table[,4])/sum(tax_novelty_table)

# Save output as a table like so
            # Prob      # Upper CI    # Lower CI
# Back
# Instant
# Cumul
# Novel








