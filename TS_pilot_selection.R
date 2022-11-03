# Function to create a list of time series that qualify for novelty analysis
# Requirements are that there must be at least 10 unique, consecutive years in the dataset, and they must have greater than 2 species

TS_pilot_selection <- function(timeseries_ID, bin_width){
  # Want to make sure each time series is individual and unique
  timeseries_ID = TSG111
  
  #Check if there are at least 10 years
  if(unique(timeseries_ID$Years)>=bin_width){
    return("V") #Viable
  }
  else{
    return("DNQ") #Does not qualify
  }
  
  # Check if no. species > = 2 in each time series
  #for (i in TS_list){
   # if Species > = 2
    #ts_list <- 
}
  
  
  
  


}
