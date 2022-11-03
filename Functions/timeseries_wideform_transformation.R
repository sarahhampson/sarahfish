# Transform long time series data into wide form

longtowide_timeseries <- function(timeseries, bin_width){
  
  bin_cut <- cut(timeseries$Year, breaks=seq(1900, 2100, bin_width))
 
  ts.mat  <- tapply(timeseries$Abundance, list(bin_cut, timeseries$Species), mean, na.rm=TRUE)
  
  #Remove factors which have no values (i.e. years which are epmty/not part of the time series)
  ts.mat <- ts.mat[rowSums(is.na(ts.mat))!=ncol(ts.mat),]
  #Make NA species entries 0
  ts.mat[is.na(ts.mat)] <- 0
  
  if (length(nrows(ts.mat))>0) {
    rownames(ts.mat) <- rowMeans(cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", rownames(ts.mat) )),
        upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", rownames(ts.mat) ))))
  
  #ts.mat <- ts.mat[-which(sapply(ts.mat[Type], is.null))]
  
  return(ts.mat)
    }
  }
