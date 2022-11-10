# Function to apply the novelty detection framework over a list of time series

multi_timeseries_novelty <- function(TS_matrix_list){
  
  # Create empty list
  multi_timeseries_results <- list()
  # Prepare simple variables
  TS_ID <- surveydata$TimeSeriesID
  TS_List <- yr_table$TimeSeriesID
  
  # Create for loop to go through time series IDs
  for(i in 1:length(TS_List)){
    # Make easier variable names
    TS_data <- surveydata[TS_ID==TS_List[i],]
    
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
  
  # Select data row for a given timeseries
  TS_data <- surveydata[surveydata$TimeSeriesID==timeseries_ID,]
 
  # Create species abundance matrix for a given time series
  TS_data.mat <- tapply(TS_data$Abundance, list(TS_data$Year, TS_data$Species), mean, na.rm=TRUE)
  
  # Replace NA with 0
  TS_data.mat[is.na(TS_data.mat)] <- 0
  
  # Remove matrices with 1 species
  if(n_distinct(TS_data$Species) > 1){
    # Apply framework 
    TS_output <- identify.novel.gam(TS_data.mat, alpha = 0.05, metric = "bray", site = timeseries_ID, plot = TRUE, plot.data = TRUE,
                                     gam.max.k = -1)
    return(TS_output)
  }
  else{
    return(print("There is not enough species in this timeseries"))
  }
}
  