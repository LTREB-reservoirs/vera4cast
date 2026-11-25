#' 
#' @author Abigail Lewis
#' @title target_generation_RDO_daily
#' @description This function loads RDO data from FCR and BVR and generates daily targets for the vera forecasting challenge. Note that depth changes at bvre, here we are using static depth labels from EDI
#' 
#' @param fcr_files vector of files with identical format that are are from FCR
#' @param bvr_current current BVR file
#' @param bvr_2020_2022 EDI BVR publication
#' @example target_generation_rdo_daily(fcr_files, bvr_current, bvr_2020_2022)
#'
#' @return dataframe of cleaned and combined RDO targets
#'

library(tidyverse)

#fcr_files <- c("https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f",
#               "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")
#bvr_current <- c("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv")
#bvr_2020_2022 <- c("https://pasta.lternet.edu/package/data/eml/edi/725/3/a9a7ff6fe8dc20f7a8f89447d4dc2038")
#bvr_2016_2020 <- readr::read_csv("https://pasta.lternet.edu/package/data/eml/edi/725/3/38965aab7e21bf7a6157ba4d199c5e2c")

target_generation_rdo_daily <- function (fcr_files, 
                                        bvr_files) {
  
  # Load FCR data
  fcr_df <- readr::read_csv(fcr_files) |> 
    dplyr::mutate(site_id = "fcre",
                  DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                  DateTime = lubridate::with_tz(DateTime, tzone = "UTC"),
                  Date = as.Date(DateTime))
  
  # Load BVR data
  bvr_df <- readr::read_csv(bvr_files) |> 
    dplyr::mutate(site_id = "bvre",
                  DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                  DateTime = lubridate::with_tz(DateTime, tzone = "UTC"),
                  Date = as.Date(DateTime))
  
  # Format data to combine
  # FCR
  fcr_sum <- fcr_df |>
    dplyr::group_by(Date, site_id) |> #daily mean
    dplyr::summarise(Hypoxia_binary_9 = as.numeric(mean(RDO_mgL_9, na.rm = T)<2),
                     RDO_mgL_5 = mean(RDO_mgL_5, na.rm = T),
                     RDOsat_percent_5 = mean(RDOsat_percent_5, na.rm = T),
                     RDO_mgL_9 = mean(RDO_mgL_9, na.rm = T),
                     RDOsat_percent_9 = mean(RDOsat_percent_9, na.rm = T))
  
  # BVR
  bvr_sum <- bvr_df |> 
    dplyr::group_by(Date, site_id) |> #daily mean
    dplyr::summarise(Hypoxia_binary_13 = as.numeric(mean(RDO_mgL_13, na.rm = T) < 2),
                     RDO_mgL_6 = mean(RDO_mgL_6, na.rm = T),
                     RDOsat_percent_6 = mean(RDOsat_percent_6, na.rm = T),
                     RDO_mgL_13 = mean(RDO_mgL_13, na.rm = T),
                     RDOsat_percent_13 = mean(RDOsat_percent_13, na.rm = T)
                     )
  message("Depths at BVR should potentially respond to changing water level. Keeping a message here so we remember to address this")
  
  #Combine
  comb_sum <- fcr_sum |> 
    dplyr::bind_rows(bvr_sum) |> 
    dplyr::rename(datetime = Date) |> 
    pivot_longer(cols = Hypoxia_binary_9:RDOsat_percent_13, names_to = "variable", values_to = "observation") |> 
    dplyr::mutate(depth_m = as.numeric(gsub("\\D", "", variable))) |> 
    dplyr::mutate(variable = sub("_[0-9]", "", variable)) |> 
    dplyr::select(datetime, site_id, depth_m, observation, variable) |> 
    dplyr::mutate(observation = ifelse(!is.finite(observation),NA,observation)) |> 
    dplyr::filter(!is.na(depth_m))
  
  return(comb_sum)
}
