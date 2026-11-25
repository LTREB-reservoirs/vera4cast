#' 
#' @author Abigail Lewis
#' @title target_generation_waterlevel_hourly
#' @description This function loads catwalk data from FCR and BVR and generates hourly targets for the vera forecasting challenge
#' 
#' @param fcr_files vector of files with identical format that are are from FCR
#' @param bvr_current current BVR file
#' @param bvr_2020_2022 EDI BVR publication
#' @example target_generation_waterlevel_hourly(fcr_files, bvr_current, bvr_2020_2022)
#'
#' @return dataframe of cleaned and combined water level
#'

library(tidyverse)

#fcr_files <- c("https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f",
#               "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")
# bvr_files <- c("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv", 
#                "https://pasta.lternet.edu/package/data/eml/edi/725/3/a9a7ff6fe8dc20f7a8f89447d4dc2038")
#bvr_current <- c("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv")
#bvr_2020_2022 <- c("https://pasta.lternet.edu/package/data/eml/edi/725/3/a9a7ff6fe8dc20f7a8f89447d4dc2038")
#bvr_2016_2020 <- readr::read_csv("https://pasta.lternet.edu/package/data/eml/edi/725/3/38965aab7e21bf7a6157ba4d199c5e2c")

target_generation_waterlevel_hourly <- function (fcr_files, 
                                                 bvr_files) {
  
  # Load FCR data
  fcr_df <- readr::read_csv(fcr_files) |> 
    dplyr::mutate(site_id = "fcre",
                  DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                  DateTime = lubridate::with_tz(DateTime, tzone = "UTC"),
                  Date = as.Date(DateTime),
                  Hour = lubridate::hour(DateTime))
  
  # Load BVR data
  fcr_df <- readr::read_csv(bvr_files) |> 
    dplyr::mutate(site_id = "fcre",
                  DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                  DateTime = lubridate::with_tz(DateTime, tzone = "UTC"),
                  Date = as.Date(DateTime),
                  Hour = lubridate::hour(DateTime))
  
  # Format data to combine
  # FCR
  fcr_sum <- fcr_df |>
    dplyr::group_by(Date, Hour, site_id) |> #hourly mean
    dplyr::summarise(WaterLevel_m = mean(LvlDepth_m_9, na.rm = T))
  message("Note: FCR water level probably also needs an offset, which we haven't figured out yet")
  
  # BVR
  bvr_sum <- bvr_df |> 
    dplyr::group_by(Date, Hour, site_id) |> #hourly mean
    dplyr::summarise(WaterLevel_m = mean(Depth_m_13 + 0.2, na.rm = T))
  
  #Combine
  comb_sum <- fcr_sum |> 
    dplyr::bind_rows(bvr_sum) |> 
    dplyr::mutate(datetime = paste0(Date," ", Hour,":00:00"))
  
  comb_sum$datetime <- lubridate::ymd_hms(comb_sum$datetime) #For some reason this was really slow within the mutate call??
  
  comb_sum <- comb_sum |> 
    pivot_longer(cols = WaterLevel_m, names_to = "variable", values_to = "observation") |> 
    dplyr::mutate(depth_m = NA) |> 
    dplyr::select(datetime, site_id, depth_m, observation, variable) |> 
    dplyr::mutate(observation = ifelse(!is.finite(observation),NA,observation))
  
  return(comb_sum)
}
