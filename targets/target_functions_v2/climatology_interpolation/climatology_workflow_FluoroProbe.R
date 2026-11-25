# Title: Climatology model workflow for FluoroProbe
# Author: Mary Lofton
# Date: 19SEP23

# Purpose: workflow for fitting climatology models for FP targets

# load packages ----
library(tidyverse)
library(lubridate)

# source functions ----
source("./R/target_generation_FluoroProbe.R")
source("./R/climatology_interpolation/fit_climatology_FluoroProbe.R")

# define current and historic files ----
historic_data <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.272.7&entityid=001cb516ad3e8cbabe1fdcf6826a0a45"
current_file <- "/Users/MaryLofton/RProjects/Reservoirs/Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/fluoroprobe_L1.csv"

# Austin when I try to load directly from GitHub, keep getting this error
# Reading in unpublished data
# Error: The size of the connection buffer (131072) was not large enough
# to fit a complete line:
#   * Increase it by setting `Sys.setenv("VROOM_CONNECTION_SIZE")`

# generate targets ----
targets <- target_generation_FluoroProbe(current_file = current_file, historic_file = historic_data)

# define arguments for DOY model function ----
data <- targets
cal_dates <- c(date(data$datetime[1]),date(last(data$datetime)))
plot_file <- "/Users/MaryLofton/Desktop/interp_plots/"

# fit DOY models with and without interpolated data ----
fit_DOY_FP(data = data, cal_dates = cal_dates, plot_file = plot_file)
