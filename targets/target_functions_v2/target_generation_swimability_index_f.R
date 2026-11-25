## function for generating the swimability targets file 
# Author: Austin Delany


target_generation_swimability_index <- function(fcre_current_temp_file, 
                                                fcre_historic_temp_file, 
                                                bvre_current_temp_file,
                                                bvre_historic_temp_file,
                                                current_met_file, 
                                                historic_met_file){
 
  ## TEMPERATURE - FCRE
  fcre_current_temp_data <- read_csv(fcre_current_temp_file)
  
  inUrl_fcre  <- fcre_historic_temp_file 
  infile1 <- tempfile()
  try(download.file(inUrl_fcre,infile1,method="curl"))
  if (is.na(file.size(infile1))) download.file(inUrl_fcre,infile1,method="auto")
  fcre_historic_temp_data <-read_csv(infile1)
  
  fcre_temp_data_full <- bind_rows(fcre_current_temp_data,fcre_historic_temp_data) |> 
    select(DateTime, Reservoir, ThermistorTemp_C_surface)
  
  
  ## TEMPERATURE - bvre
  bvre_current_temp_data <- read_csv(bvre_current_temp_file)
  
  inUrl_bvre  <- bvre_historic_temp_file 
  infile2 <- tempfile()
  try(download.file(inUrl_bvre,infile2,method="curl"))
  if (is.na(file.size(infile2))) download.file(inUrl_bvre,infile1,method="auto")
  bvre_historic_temp_data <-read_csv(infile2)
  
  bvre_temp_data_full <- bind_rows(bvre_current_temp_data,bvre_historic_temp_data) |> 
    select(DateTime, Reservoir, ThermistorTemp_C_3) |> 
    rename(ThermistorTemp_C_surface = ThermistorTemp_C_3)
  
  temp_data_full <- bind_rows(fcre_temp_data_full, bvre_temp_data_full)
  
  
  temp_data_full$date <- as.Date(temp_data_full$DateTime)
  temp_data_full$hour <- lubridate::hour(temp_data_full$DateTime)
  
  temp_data_midnight <- temp_data_full |> 
    filter(hour == 0) |> 
    drop_na(ThermistorTemp_C_surface)
  
  temp_daily <-aggregate(ThermistorTemp_C_surface ~ date + Reservoir, temp_data_midnight, mean)
  
  
  ## METEOROLOGY
  current_met_data <- read_csv(current_met_file) |> 
    select(Reservoir, DateTime, AirTemp_C_Average)
  
  inUrl2  <- historic_met_file 
  infile3 <- tempfile()
  try(download.file(inUrl2,infile3,method="curl"))
  if (is.na(file.size(infile3))) download.file(inUrl2,infile3,method="auto")
  historic_met_data <-read_csv(infile3) |> 
    select(Reservoir, DateTime, AirTemp_C_Average)
  
  met_data_full <- bind_rows(current_met_data, historic_met_data)
  
  met_data_full$date <- as.Date(met_data_full$DateTime)
  met_data_full$hour <- lubridate::hour(met_data_full$DateTime)
  
  met_data_midnight <- met_data_full |> 
    filter(hour == 0) |> 
    drop_na(AirTemp_C_Average)
  
  met_daily <-aggregate(AirTemp_C_Average ~ date, met_data_midnight, mean)
  
  
  ## combine met and temp data (only want met data that also coincides with temp data)
  swim_value_daily <- temp_daily |> 
    right_join(met_daily, by = c('date')) |> 
    mutate(swim_index_f = ((ThermistorTemp_C_surface + AirTemp_C_Average) * (9/5)) + 32) |> 
    select(date, Reservoir,swim_index_f) |> 
    drop_na(swim_index_f)
  
  
  # convert to forecast format 
  build_targets <- swim_value_daily |>
    right_join(temp_data_midnight)
  
  build_targets$datetime <- as.POSIXlt(build_targets$date,tz= 'UTC')
  build_targets$depth <- 0
  build_targets$variable = 'swim_index_f'
  
  targets_df <- build_targets |> 
    select(datetime, Reservoir, depth, swim_index_f, variable) |> 
    rename(site_id = Reservoir, observation = swim_index_f)
  
  
   return(targets_df)
}
# 
# # TEST THE FUNCTION
# target_generation_swimability_index(fcre_current_temp_file = 'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv',
#                     fcre_historic_temp_file = "https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f",
#                     bvre_current_temp_file = 'https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv', 
#                     bvre_historic_temp_file = "https://pasta.lternet.edu/package/data/eml/edi/725/3/a9a7ff6fe8dc20f7a8f89447d4dc2038",
#                     current_met_file = 'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCRmet_L1.csv',
#                     historic_met_file = 'https://pasta.lternet.edu/package/data/eml/edi/389/7/02d36541de9088f2dd99d79dc3a7a853')
