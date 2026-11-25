#'
#' @author Abigail Lewis
#' @title target_generation_exo_daily
#' @description This function loads EXO-sonde data from FCR and BVR and generates daily targets for the vera forecasting challenge
#'
#' @param fcr_files vector of files with identical format that are are from FCR
#' @param bvr_current current BVR file
#' @param bvr_2020_2022 EDI BVR publication
#' @example target_generation_exo_daily(fcr_files, bvr_current, bvr_2020_2022)
#'
#' @return dataframe of cleaned and combined targets from the EXO-sonde
#'
library(tidyverse)

# fcr_files <- c("https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f",
#                "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")
# bvr_files <- c("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv",
#                "https://pasta.lternet.edu/package/data/eml/edi/725/3/a9a7ff6fe8dc20f7a8f89447d4dc2038")

target_generation_exo_daily <- function (fcr_files,
                                         bvr_files,
                                         waterlevel_file) {

  # Load FCR data
  fcr_df <- readr::read_csv(fcr_files, show_col_types = FALSE) |>
    dplyr::mutate(site_id = "fcre",
                  DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                  DateTime = lubridate::with_tz(DateTime, tzone = "UTC"),
                  sampledate = as.Date(DateTime))

  # Load BVR data
  bvr_current <- readr::read_csv(bvr_files[1], show_col_types = FALSE) |>
    dplyr::mutate(site_id = "bvre",
                  DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                  DateTime = lubridate::with_tz(DateTime, tzone = "UTC"),
                  sampledate = as.Date(DateTime))

  bvr_historical <- readr::read_csv(bvr_files[2], show_col_types = FALSE) |>
    dplyr::mutate(site_id = "bvre",
                  DateTime = lubridate::force_tz(DateTime, tzone = "EST"),
                  DateTime = lubridate::with_tz(DateTime, tzone = "UTC"),
                  sampledate = as.Date(DateTime)) #|>
    #dplyr::rename(LvlDepth_m_13 = Depth_m_13)

  bvr_df <- dplyr::bind_rows(bvr_current, bvr_historical)



  ## only use complete days (remove only partially sampled days) # 144 sample events per day (6*24)
  fcr_remove_days <- fcr_df |>
    dplyr::group_by(sampledate) |>
    dplyr::summarize(n_samples = n_distinct(DateTime)) |>
    dplyr::filter(n_samples < 144) |>
    dplyr::filter(sampledate == Sys.Date() | n_samples < 144/2)


  bvr_remove_days <- bvr_df |>
    dplyr::group_by(sampledate) |>
    dplyr::summarize(n_samples = n_distinct(DateTime)) |>
    dplyr::filter(n_samples < 144) |>
    dplyr::filter(sampledate == Sys.Date() | n_samples < 144/2)


  # Format data to combine
  # FCR
  fcr_sum <- fcr_df |>
    filter(!sampledate %in% fcr_remove_days$sampledate) |>  # filter for complete days
    dplyr::group_by(sampledate, site_id) |>
    dplyr::summarise(Cond_uScm_mean = mean(EXOCond_uScm_1, na.rm = T),
                     Temp_C_mean = mean(EXOTemp_C_1, na.rm = T),
                     SpCond_uScm_mean = mean(EXOSpCond_uScm_1, na.rm = T),
                     DOsat_percent_mean = mean(EXODOsat_percent_1, na.rm = T),
                     DO_mgL_mean = mean(EXODO_mgL_1, na.rm = T),
                     Chla_ugL_mean = mean(EXOChla_ugL_1, na.rm = T),
                     fDOM_QSU_mean = mean(EXOfDOM_QSU_1, na.rm = T),
                     Turbidity_FNU_mean = mean(EXOTurbidity_FNU_1, na.rm = T),
                     Bloom_binary_mean = as.numeric(mean(Chla_ugL_mean, na.rm = T)>20)#,
                     #EXODepth_m = mean(EXODepth_m, na.rm = T) #could use this line to have changing depths based on the exo depth sensor
                     )

  # BVR
  bvr_sum <- bvr_df |>
    filter(!sampledate %in% bvr_remove_days$sampledate) |> # filter for complete days
    dplyr::group_by(sampledate, site_id) |> #daily mean
    dplyr::summarise(Cond_uScm_mean = mean(EXOCond_uScm_1.5, na.rm = T),
                     Temp_C_mean = mean(EXOTemp_C_1.5, na.rm = T),
                     SpCond_uScm_mean = mean(EXOSpCond_uScm_1.5, na.rm = T),
                     DOsat_percent_mean = mean(EXODOsat_percent_1.5, na.rm = T),
                     DO_mgL_mean = mean(EXODO_mgL_1.5, na.rm = T),
                     Chla_ugL_mean = mean(EXOChla_ugL_1.5, na.rm = T),
                     fDOM_QSU_mean = mean(EXOfDOM_QSU_1.5, na.rm = T),
                     Turbidity_FNU_mean = mean(EXOTurbidity_FNU_1.5, na.rm = T),
                     Bloom_binary_mean = as.numeric(mean(Chla_ugL_mean, na.rm = T)>20)#,
                     #EXODepth_m = mean(EXODepth_m, na.rm = T)
    )


  # ## build DO for each site separately and then combine
  # fcr_DO <- fcr_df |>
  #   dplyr::filter(!sampledate %in% fcr_remove_days$sampledate) |>  # filter for complete days
  #   dplyr::select(DateTime, (starts_with('RDO') & ends_with('adjusted')) | (starts_with('EXODO'))) |>
  #   tidyr::pivot_longer(-DateTime, names_to = 'variable', values_to = 'observation') |>
  #   dplyr::mutate(sampledate = as.Date(DateTime)) |>
  #   dplyr::mutate(depth_m = as.numeric(sapply(stringr::str_split(variable, "_"), function(x) x[3]))) |>
  #   dplyr::mutate(depth_m = ifelse((grepl('EXODO_mgL', variable) | grepl('EXODOsat', variable)), 1.6, depth_m)) |>
  #   dplyr::mutate(variable = ifelse(grepl('RDO_mgL', variable), 'DO_mgL_mean', variable)) |>
  #   dplyr::mutate(variable = ifelse(grepl('RDOsat', variable), 'DOsat_percent_mean', variable)) |>
  #   dplyr::mutate(variable = ifelse(grepl('EXODO_mgL', variable), 'DO_mgL_mean', variable)) |>
  #   dplyr::mutate(variable = ifelse(grepl('EXODOsat', variable), 'DOsat_percent_mean', variable)) |>
  #   dplyr::summarise(obs_avg = mean(observation, na.rm = TRUE), .by = c('sampledate', 'variable', 'depth_m')) |>
  #   dplyr::mutate(datetime=ymd_hms(paste0(sampledate,"","00:00:00"))) |>
  #   dplyr::select(datetime, depth_m, observation = obs_avg, variable)
  #
  # fcr_DO$site_id <- 'fcre'
  #
  #
  # bvr_DO <- bvr_df |>
  #   dplyr::filter(!sampledate %in% bvr_remove_days$sampledate) |> # filter for complete days
  #   dplyr::select(DateTime, (starts_with('RDO')) | (starts_with('EXODO'))) |>
  #   tidyr::pivot_longer(-DateTime, names_to = 'variable', values_to = 'observation') |>
  #   dplyr::mutate(sampledate = as.Date(DateTime)) |>
  #   dplyr::mutate(depth_m = as.numeric(sapply(stringr::str_split(variable, "_"), function(x) x[3]))) |>
  #   dplyr::mutate(depth_m = ifelse((grepl('EXODO_mgL', variable) | grepl('EXODOsat', variable)), 1.5, depth_m)) |>
  #   dplyr::mutate(variable = ifelse(grepl('RDO_mgL', variable), 'DO_mgL_mean', variable)) |>
  #   dplyr::mutate(variable = ifelse(grepl('RDOsat', variable), 'DOsat_percent_mean', variable)) |>
  #   dplyr::mutate(variable = ifelse(grepl('EXODO_mgL', variable), 'DO_mgL_mean', variable)) |>
  #   dplyr::mutate(variable = ifelse(grepl('EXODOsat', variable), 'DOsat_percent_mean', variable)) |>
  #   dplyr::summarise(obs_avg = mean(observation, na.rm = TRUE), .by = c('sampledate', 'variable', 'depth_m')) |>
  #   dplyr::mutate(datetime=ymd_hms(paste0(sampledate,"","00:00:00"))) |>
  #   dplyr::select(datetime, depth_m, observation = obs_avg, variable, observation = obs_avg)
  #
  # bvr_DO$site_id <- 'bvre'
  #
  #
  # combined_DO <- bind_rows(fcr_DO, bvr_DO) |>
  #   dplyr::mutate(observation = ifelse(is.nan(observation), NA, observation)) |>
  #   dplyr::filter(variable %in% c('DO_mgL_mean', 'DOsat_percent_mean')) ## avoid the RDO temp variables
  #
  # do_exo_depth <- combined_DO |>
  #   filter(depth_m %in% c(1.6, 1.5))
  #
  # ## convert depths into heights
  #
  # # BVR
  # depth_offset <-  read_csv("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv")
  #
  # # Make sure dates are in the right format
  # depth <- depth_offset|>
  #   mutate(
  #     Offset_end = ymd_hms(ifelse(Offset_end == "current", as.character(Sys.time()),Offset_end)))
  #
  # do_bvr_positions <- combined_DO |>
  #   filter(!(depth_m %in% c(1.6, 1.5)),
  #          site_id == 'bvre') |>
  #   drop_na(observation) |>
  #   rename(Position = depth_m) |>
  #   mutate(Reservoir = 'BVR')
  #
  # by <- join_by(between(x$datetime, y$Offset_start, y$Offset_end), Position, Reservoir)
  #
  # do_bvr_positions  <- full_join(do_bvr_positions, depth, by)|>
  #   drop_na(datetime) |>
  #   mutate(z = Distance_above_sediments,
  #          z_ref = 'height_m',
  #          position_flag = NA) |>
  #   select(site_id, datetime, variable, z, z_ref, observation, position_flag)
  #
  #
  # do_fcr_positions <- combined_DO |>
  #   filter(!(depth_m %in% c(1.6, 1.5)),
  #          site_id == 'fcre') |>
  #   drop_na(observation) |>
  #   mutate(z = (max(depth_m) + 0.5) - depth_m, # CONVERT DEPTH TO HEIGHT
  #          z_ref = 'height_m',
  #          position_flag = NA) |>
  #   select(-depth_m)
  #
  #
  # combined_DO_positions <- bind_rows(do_bvr_positions, do_fcr_positions)
  # ## assign depth positions ##
  #
  # ## add position assignment ##
  # water_level_match_df <- waterlevel_file |>
  #   select(datetime, site_id, observation) |>
  #   rename(waterlevel_m = observation)
  #
  # targets_df_with_wl <- combined_DO_positions |>
  #   left_join(water_level_match_df, by = c('datetime','site_id')) |>
  #   mutate(obs_z_ref = waterlevel_m - z)
  #
  # ## for each date, extract the shallowest depth within 2m of the surface
  # # depth_m is measured from the surface so the shallowest sample is the smallest depth
  # # flagged depths without a sample within 2 m of the surface are left unflagged
  # targets_position_assign_df <- targets_df_with_wl |>
  #   group_by(site_id, datetime) |>
  #   mutate(
  #     position_flag = case_when( ## HEIGHT REFERENCE
  #       z <= 2 & z == min(z) ~ 'bottom_most_2m', # get min height value
  #       obs_z_ref <= 2 & z == max(z) ~ 'top_most_2m' # get max height value
  #     )
  #   ) |>
  #   ungroup() |>
  #   select(datetime, site_id, z, z_ref, observation, variable, position_flag) # rearrange order of columns


  #depth is 1.5 at BVR and 1.6 and FCR

  #Combine all and format
  comb_sum <- fcr_sum |>
    dplyr::bind_rows(bvr_sum) |>
    dplyr::ungroup() |> ## summarise() above leaves the data grouped by sampledate
    dplyr::rename(datetime = sampledate) |>
    tidyr::pivot_longer(cols = Temp_C_mean:Bloom_binary_mean, names_to = "variable", values_to = "observation") |>
    dplyr::mutate(depth_m = NA,
                  depth_m = ifelse(site_id == "fcre", 1.6, depth_m),
                  depth_m = ifelse(site_id == "bvre", 1.5, depth_m)) |>
    #dplyr::rename(depth_m = EXODepth_m) |>
    dplyr::select(datetime, site_id, depth_m, observation, variable) |>
    dplyr::mutate(observation = ifelse(!is.finite(observation),NA,observation),
                  z_ref = 'depth_m') |>
    dplyr::rename(z = depth_m) |>
    select(datetime, site_id, z, z_ref, observation, variable) #|>
    #bind_rows(targets_position_assign_df) # append DO data


  exo_dup_check <- comb_sum  %>%
    dplyr::group_by(datetime, site_id, variable, z, z_ref) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1)

  if (nrow(exo_dup_check) == 0){
    return(comb_sum)
  }else{
    print('EXO duplicates found...please fix')
    stop()
  }

  #return(comb_sum)
}

## Call healthcheck
RCurl::url.exists("https://hc-ping.com/04dde6b2-a5f1-4a33-811b-3386cf84d4f9", timeout = 5)
