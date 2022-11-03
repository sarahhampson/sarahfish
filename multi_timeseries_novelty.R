# Function to apply the novelty detection framework to one time series

multi_timeseries_novelty <- function(surveydata, timeseries_ID){
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
  