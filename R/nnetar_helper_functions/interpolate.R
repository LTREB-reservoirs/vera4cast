#Interpolate target data
#Author: Mary Lofton
#Date: 26SEP23

#Purpose: interpolate EXO chl-a data to daily

#'Function to fit day of year model for chla
#'@param vec vector of chl-a values

#load packages
library(tidyverse)
library(lubridate)
library(zoo)

interpolate <- function(vec){
  
  #replace missing values at beginning of timeseries
  vec[cumall(is.na(vec))] <- as.double(vec[min(which(!is.na(vec)))])
  
  #create interpolated timeseries
  interp <- na.approx(vec)
  
  #fill in missing values at end of timeseries
  if(length(interp) < length(vec)){
    num_NA = length(vec) - length(interp)
    nas <- rep(NA, times = num_NA)
    interp2 = c(interp, nas)
    interp3 <- na.locf(interp2)
    out <- interp3
  } else {
    out <- interp
  }
  
  return(out)
  
}