# Title: Climatology model workflow for Chem
# Author: Mary Lofton and Austin Delany
# Date: 19SEP23

# Purpose: workflow for fitting climatology models for FP targets

# load packages ----
library(tidyverse)
library(lubridate)

## set EDI API Key
edi_access_key = Sys.getenv('EDI_ACCESS_KEY')

# source functions ----
source("./R/target_generation_chemistry_daily.R")
source("./R/climatology_interpolation/fit_climatology_chem.R")

# define current and historic files ----
#historic_data <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.272.7&entityid=001cb516ad3e8cbabe1fdcf6826a0a45"
#current_file <- "/Users/MaryLofton/RProjects/Reservoirs/Data/DataNotYetUploadedToEDI/Raw_fluoroprobe/fluoroprobe_L1.csv"
current_file <- NULL # not ready yet

# Austin when I try to load directly from GitHub, keep getting this error
# Reading in unpublished data
# Error: The size of the connection buffer (131072) was not large enough
# to fit a complete line:
#   * Increase it by setting `Sys.setenv("VROOM_CONNECTION_SIZE")`

# generate targets ----
targets <- target_generation_chemistry_daily(
 current_data_file=NULL,
 historic_data_file=paste0("https://pasta.lternet.edu/package/data/eml/edi/199/11/509f39850b6f95628d10889d66885b76?key=",edi_access_key))

# define arguments for DOY model function ----
data <- targets
cal_dates <- c(date(data$datetime[1]),date(last(data$datetime)))
plot_file <- "R/climatology_interpolation/chem_interp_plots/"

# fit DOY models with and without interpolated data ----
fit_DOY_chem(data = data, cal_dates = cal_dates, plot_file = plot_file)
