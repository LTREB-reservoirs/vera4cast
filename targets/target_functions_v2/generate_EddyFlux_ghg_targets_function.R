# Function for generating the targets file for mean daily fluxes from EddyFlux
# Author: Adrienne Breef-Pilz
# Created: 8 Sep 2023
# Edited: 29 March 2024 - fix column name issues and timzone issues

install.packages('pacman')
pacman::p_load("tidyverse","lubridate")


generate_EddyFlux_ghg_targets_function <- function(flux_current_data_file,
                                                   flux_edi_data_file,
                                                   met_current_data_file,
                                                   met_edi_data_file){

  # Things to figure out is how many fluxes are needed for a good daily flux.
  # Right now it is just a daily average no matter if it is one observation or 48

  # functions we need for despike
  source("https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/EddyFlux_Processing/despike.R")

  ## Read in the data files

  ## read in EddyFlux summary files from the current data file which is found on GitHub

  dt1 <-read_csv(flux_current_data_file)

  # read in historical data file
  # EDI

  # read in the data file downloaded from EDI
  dt2 <-read_csv(flux_edi_data_file)

  # combine the historic and the current data file
  ec <- dt2%>%
    bind_rows(.,dt1)%>%
    distinct()

  # Format time
  # make a datetime column and read in with original timezone
  ec$datetime <- paste0(ec$date, " ",ec$time)

  # Set timezone as America/New_York because that's what it is in and then convert to EST
  ec$datetime <- force_tz(ymd_hms(ec$datetime), tzone = "America/New_York")

  # convert from Eastern/US with daylight savings observed to EST which does not.
  ec$datetime <- with_tz(ec$datetime, tzone = "EST")



  #### Reading in data from the Met Station for QAQCing when raining
  # Load data Meteorological data from EDI


  # Read in Met file from EDI
  met_all <- read_csv(met_edi_data_file,
                      col_select=c("DateTime","Rain_Total_mm"))%>%
    mutate(DateTime = force_tz(DateTime, tzone="EST"))%>%
    # Start timeseries on the 00:15:00 to facilitate 30-min averages
    filter(DateTime >= ymd_hms("2020-04-04 00:15:00", tz="EST"))

  # Bind files together if need to use current file

  met_curr <- read_csv(met_current_data_file,
                       col_select=c("DateTime","Rain_Total_mm"))%>%
    mutate(DateTime = force_tz(DateTime, tzone="EST"))

  met_all <- dplyr::bind_rows(met_curr, met_all) # bind everything together


  # Start timeseries on the 00:15:00 to facilitate 30-min averages

  # Select data every 30 minutes from Jan 2020 to end of met data
  met_all$Breaks <- cut(met_all$DateTime,breaks = "30 mins",right=FALSE)
  met_all$Breaks <- parse_date_time(met_all$Breaks, orders=c("ymd", "ymd HMS"), tz="EST")


  # Sum met data to the 30 min mark (for Total Rain and Total PAR)
  met_2 <- met_all %>%
    select(DateTime,Rain_Total_mm,Breaks) %>%
    group_by(Breaks) %>%
    summarise_if(is.numeric,sum,na.rm=TRUE) %>%
    ungroup()%>%
    mutate(DateTime=Breaks - 900)%>%
    rename(datetime = DateTime,
           Rain_sum = Rain_Total_mm)

  ec2 <- left_join(ec, met_2, by='datetime')

  # convert time to UTC
  ec2 <- ec2 |>
    dplyr::mutate(datetime_utc = with_tz(datetime, tz = 'UTC'),
                  date = as.Date(datetime_utc))



  # Filter out wind directions that are BEHIND the catwalk
  # I.e., only keep data that is IN FRONT of the catwalk for both EC and Met data
  ec_filt <- ec2 %>% dplyr::filter(wind_dir < 80 | wind_dir > 250)


  # Remove values that are greater than abs(100)
  # NOTE: Updated from Brenda's code to use abs(100); instead of -70 to 100 filtering
  # Waldo et al. 2021 used: values greater than abs(15000)
  ec_filt$co2_flux_umolm2s <- ifelse(ec_filt$co2_flux_umolm2s > 100 | ec_filt$co2_flux_umolm2s < -100, NA, ec_filt$co2_flux_umolm2s)

  # Remove CO2 data if QC >= 2 (aka: data that has been flagged by Eddy Pro)
  ec_filt$co2_flux_umolm2s <- ifelse(ec_filt$qc_co2_flux >= 2, NA, ec_filt$co2_flux_umolm2s)

  # Additionally remove CO2 data when H and LE > 2 (following CH4 filtering)
  ec_filt$co2_flux_umolm2s <- ifelse(ec_filt$qc_co2_flux==1 & ec_filt$qc_LE>=2, NA, ec_filt$co2_flux_umolm2s)
  ec_filt$co2_flux_umolm2s <- ifelse(ec_filt$qc_co2_flux==1 & ec_filt$qc_H>=2, NA, ec_filt$co2_flux_umolm2s)

  # Remove large CH4 values

  # Remove values that are greater than abs(0.25)
  # NOTE: Updated from Brenda's code to use abs(0.25)
  # Waldo et al. 2021 used: values greater than abs(500)
  ec_filt$ch4_flux_umolm2s <- ifelse(ec_filt$ch4_flux_umolm2s >= 0.25 | ec_filt$ch4_flux_umolm2s <= -0.25, NA, ec_filt$ch4_flux_umolm2s)

  # Remove ch4 values when signal strength < 20
  ec_filt$ch4_flux_umolm2s <- ifelse(ec_filt$rssi_77_mean < 20, NA, ec_filt$ch4_flux_umolm2s)

  # Remove CH4 data if QC >= 2
  ec_filt$ch4_flux_umolm2s <- ifelse(ec_filt$qc_ch4_flux >=2, NA, ec_filt$ch4_flux_umolm2s)

  # Additionally, remove CH4 when other parameters are QA/QC'd
  # Following Waldo et al. 2021: Remove additional ch4 flux data
  # (aka: anytime ch4_qc flag = 1 & another qc_flag =2, remove)
  ec_filt$ch4_flux_umolm2s <- ifelse(ec_filt$qc_ch4_flux==1 & ec_filt$qc_co2_flux>=2, NA, ec_filt$ch4_flux_umolm2s)
  ec_filt$ch4_flux_umolm2s <- ifelse(ec_filt$qc_ch4_flux==1 & ec_filt$qc_LE>=2, NA, ec_filt$ch4_flux_umolm2s)
  ec_filt$ch4_flux_umolm2s <- ifelse(ec_filt$qc_ch4_flux==1 & ec_filt$qc_H>=2, NA, ec_filt$ch4_flux_umolm2s)

  # Check QC for H and LE
  # Removing qc >= 2 for H and LE
  ec_filt$H_wm2 <- ifelse(ec_filt$qc_H >= 2, NA, ec_filt$H_wm2)
  ec_filt$LE_wm2 <- ifelse(ec_filt$qc_LE >= 2, NA, ec_filt$LE_wm2)

  # Remove high H values: greater than abs(200)
  # NOTE: Updated to have same upper and lower magnitude bound
  # Waldo et al. 2021 used abs of 200 for H

  ec_filt$H_wm2 <- ifelse(ec_filt$H_wm2 >= 200 | ec_filt$H_wm2 <= -200, NA, ec_filt$H_wm2)

  # Remove high LE values: greater than abs(500)
  # NOTE: Updated to have same upper and lower magnitude bounds
  # Waldo et al. 2021 used abs of 1000 for LE

  ec_filt$LE_wm2 <- ifelse(ec_filt$LE_wm2 >= 500 | ec_filt$LE_wm2 <= -500, NA, ec_filt$LE_wm2)


  # Remove CH4 when it rains
  ec_filt$ch4_flux_umolm2s <- ifelse(ec_filt$Rain_sum > 0, NA, ec_filt$ch4_flux_umolm2s)

  # Remove CH4 data when thermocouple was not working (apr 05 - apr 25) # ABP find for 2023
  ec_filt$ch4_flux_umolm2s <- ifelse(ec_filt$datetime >= '2021-04-05' & ec_filt$datetime <= '2021-04-25',
                                     NA, ec_filt$ch4_flux_umolm2s)

  eddy_fcr <- ec_filt

  # Despike NEE (CO2 flux) and CH4. Use the function sourced at the beginning of the script


  # Calculate low, medium, and high data flags
  flag <- spike_flag(eddy_fcr$co2_flux_umolm2s,z = 7)
  NEE_low <- ifelse(flag == 1, NA, eddy_fcr$co2_flux_umolm2s)
  flag <- spike_flag(eddy_fcr$co2_flux_umolm2s,z = 5.5)
  NEE_medium <- ifelse(flag == 1, NA, eddy_fcr$co2_flux_umolm2s)
  flag <- spike_flag(eddy_fcr$co2_flux_umolm2s,z = 4)
  NEE_high <- ifelse(flag == 1, NA, eddy_fcr$co2_flux_umolm2s)


  # Combine all flagged data into the data frame but only keep medium one

  eddy_fcr$CO2_med_flux <- NEE_medium


  #Despike CH4 flux
  flag <- spike_flag(eddy_fcr$ch4_flux_umolm2s,z = 7)
  CH4_low <- ifelse(flag == 1, NA, eddy_fcr$ch4_flux_umolm2s)
  flag <- spike_flag(eddy_fcr$ch4_flux_umolm2s,z = 5.5)
  CH4_medium <- ifelse(flag == 1, NA, eddy_fcr$ch4_flux_umolm2s)
  flag <- spike_flag(eddy_fcr$ch4_flux_umolm2s,z = 4)
  CH4_high <- ifelse(flag == 1, NA, eddy_fcr$ch4_flux_umolm2s)


  # Combine all flagged data into the data frame but only keep the medium one
  eddy_fcr$ch4_med_flux <- CH4_medium


  # Filter out all the values (x_peak) that are out of the reservoir
  eddy_fcr$footprint_flag <- ifelse(eddy_fcr$wind_dir >= 15 & eddy_fcr$wind_dir <= 90 & eddy_fcr$x_peak_m >= 40, 1,
                                    ifelse(eddy_fcr$wind_dir < 15 & eddy_fcr$wind_dir > 327 & eddy_fcr$x_peak_m > 120, 1,
                                           ifelse(eddy_fcr$wind_dir < 302 & eddy_fcr$wind_dir >= 250 & eddy_fcr$x_peak_m > 50, 1, 0)))

  # Remove flagged data
  targets_df <- eddy_fcr %>%
    filter(footprint_flag == 0)%>% # filter out so it is the smallest footprint
    select(date, CO2_med_flux, ch4_med_flux)%>%
    dplyr::rename(CO2flux_umolm2s_mean = CO2_med_flux,
                  CH4flux_umolm2s_mean = ch4_med_flux)%>%  # rename columns

    group_by(date)%>% # average if there are more than one sample taken during that day
    summarise_if(is.numeric, mean, na.rm = TRUE)%>%
    ungroup()%>%
    drop_na(date)%>% # drop when we have timezone issues with daylight savings
    mutate(datetime=(paste0(date," ","00:00:00")))%>%
    #drop_na(datetime) %>%
    mutate(Reservoir='fcre')%>% # change the name to the the reservoir code for FLARE
    mutate(Depth_m = NA)%>%
    select(-date)%>%
    rename(site_id=Reservoir, # rename the columns for standard notation
           depth_m=Depth_m)%>%
    pivot_longer(cols=c(CO2flux_umolm2s_mean, CH4flux_umolm2s_mean), # make the wide data frame into a long one so each observation has a depth
                 names_to='variable',
                 values_to='observation')%>%
    mutate(duration = 'P1D',
           z = NA,
           z_ref = 'depth_m') |>
    select(c('datetime', 'site_id', 'z', 'z_ref', "observation", 'variable', 'duration')) # rearrange order of columns



  ## return dataframe formatted to match FLARE targets
  return(targets_df)
}

# Using the function with the EDI address for data
# generate_EddyFlux_ghg_targets_function(

# flux_current_data_file <- "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/EddyFlux_Processing/EddyPro_Cleaned_L1.csv",
# flux_edi_data_file <- "https://pasta-s.lternet.edu/package/data/eml/edi/692/11/e0976e7a6543fada4cbf5a1bb168713b",
# met_current_data_file <- "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCRmet_L1.csv",
# met_edi_data_file <- "https://pasta.lternet.edu/package/data/eml/edi/389/8/d4c74bbb3b86ea293e5c52136347fbb0")

