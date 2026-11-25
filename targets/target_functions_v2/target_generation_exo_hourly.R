#' 
#' @author Abigail Lewis
#' @title target_generation_exo_hourly
#' @description This function loads EXO-sonde data from FCR and BVR and generates hourly targets for the vera forecasting challenge
#' 
#' @param fcr_files vector of files with identical format that are are from FCR
#' @param bvr_current current BVR file
#' @param bvr_2020_2022 EDI BVR publication
#' @example target_generation_exo_hourly(fcr_files, bvr_current, bvr_2020_2022)
#'
#' @return dataframe of cleaned and combined targets from the EXO-sonde
#'

library(tidyverse)


# fcr_files <- c("https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f",
#               "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")
# bvr_files <- c("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv",
#               "https://pasta.lternet.edu/package/data/eml/edi/725/3/a9a7ff6fe8dc20f7a8f89447d4dc2038")

target_generation_exo_hourly <- function (fcr_files, 
                                          bvr_files) {
  
  # Load FCR data
  fcr_df <- readr::read_csv(fcr_files) |> 
    dplyr::mutate(site_id = "fcre",
                  DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                  DateTime = lubridate::with_tz(DateTime, tzone = "UTC"),
                  Date = as.Date(DateTime),
                  Hour = lubridate::hour(DateTime))
  
  # Load BVR data
  bvr_df <- readr::read_csv(bvr_files) |> 
    dplyr::mutate(site_id = "bvre",
                  DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                  DateTime = lubridate::with_tz(DateTime, tzone = "UTC"),
                  Date = as.Date(DateTime),
                  Hour = lubridate::hour(DateTime)) 
  
  # Format data to combine
  # FCR
  fcr_sum <- fcr_df |>
    dplyr::group_by(Date, Hour, site_id) |> 
    dplyr::summarise(Temp_C = mean(EXOTemp_C_1, na.rm = T),
                     Cond_uScm = mean(EXOCond_uScm_1, na.rm = T),
                     SpCond_uScm = mean(EXOSpCond_uScm_1, na.rm = T),
                     DOsat_percent = mean(EXODOsat_percent_1, na.rm = T),
                     DO_mgL = mean(EXODO_mgL_1, na.rm = T),
                     Chla_ugL = mean(EXOChla_ugL_1, na.rm = T),
                     fDOM_QSU = mean(EXOfDOM_QSU_1, na.rm = T),
                     Turbidity_FNU = mean(EXOTurbidity_FNU_1, na.rm = T),
                     Bloom_binary = as.numeric(mean(Chla_ugL, na.rm = T)>20)#,
                     #EXODepth_m = mean(EXODepth_m, na.rm = T) #could use this line to have changing depths based on the exo depth sensor
                     )
  
  # BVR
  bvr_sum <- bvr_df |> 
    dplyr::group_by(Date, Hour, site_id) |> #hourly mean
    dplyr::summarise(Temp_C = mean(EXOTemp_C_1.5, na.rm = T),
                     Cond_uScm = mean(EXOCond_uScm_1.5, na.rm = T),
                     SpCond_uScm = mean(EXOSpCond_uScm_1.5, na.rm = T),
                     DOsat_percent = mean(EXODOsat_percent_1.5, na.rm = T),
                     DO_mgL = mean(EXODO_mgL_1.5, na.rm = T),
                     Chla_ugL = mean(EXOChla_ugL_1.5, na.rm = T),
                     fDOM_QSU = mean(EXOfDOM_QSU_1.5, na.rm = T),
                     Turbidity_FNU = mean(EXOTurbidity_FNU_1.5, na.rm = T),
                     Bloom_binary = as.numeric(mean(Chla_ugL, na.rm = T)>20)#,
                     #EXODepth_m = mean(EXODepth_m, na.rm = T)
    )
  
  #depth is 1.5 at BVR and 1.6 and FCR

  #Combine and format
  comb_sum <- fcr_sum |> 
    dplyr::bind_rows(bvr_sum) |> 
    dplyr::mutate(datetime = paste0(Date," ", Hour,":00:00"))
  
  comb_sum$datetime <- lubridate::ymd_hms(comb_sum$datetime) #For some reason this was really slow within the mutate call??
  
  comb_sum <- comb_sum |> 
    tidyr::pivot_longer(cols = Temp_C:Bloom_binary, names_to = "variable", values_to = "observation") |> 
    dplyr::mutate(depth_m = ifelse(site_id == "fcre", 1.6, NA),
                  depth_m = ifelse(site_id == "bvre", 1.5, NA)) |> 
    #dplyr::rename(depth_m = EXODepth_m) |> 
    dplyr::select(datetime, site_id, depth_m, observation, variable) |> 
    dplyr::mutate(observation = ifelse(!is.finite(observation),NA,observation))
  
  return(comb_sum)
}

#a <- target_generation_exo_hourly(fcr_files = fcr_files, bvr_files = bvr_files)
