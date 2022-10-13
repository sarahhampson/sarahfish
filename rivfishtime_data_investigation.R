### This code is to help understand the breadth and depth of the rivfishtime database ###

# Characterising qualitative features

# Data
data.c <- read.csv("inputs/1873_2_RivFishTIME_TimeseriesTable.csv")

# Packages
install.packages(c("dplyr"))

library(dplyr)

# No. of sites
sitecount <- as.list(unique(data.c$SiteID))
#11072 unique sampling sites

# List of hydrobasins
hydrobasinlist <- as.list(unique(data.c$HydroBasin)) 
#402 hydrobasins

# List of countries, regions, and waterbodies
count(data.c, data.c$Country) #19
count(data.c, data.c$TimeSeriesID) #Which sites have double ups?

# Add dates to output file
date.wrap <- function(string, ext){
  paste0(string, " ", Sys.Date(), ext)
}
