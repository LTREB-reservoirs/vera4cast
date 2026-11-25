library(tidyverse)

# fcr_files <- c( "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv",
#                 "https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f")
#
# bvr_files <- c("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv",
#                "https://pasta.lternet.edu/package/data/eml/edi/725/3/a9a7ff6fe8dc20f7a8f89447d4dc2038")


target_generation_daily_physical <- function(fcr_files,
                                             bvr_files,
                                             secchi_files){

  ## EXO and RDO data
  exo_rdo_daily <- target_generation_exo_daily(fcr_files, bvr_files)

 # Thermistor String
  fcr_temp_string_daily <- target_generation_ThermistorTemp_C_daily(current_file = fcr_files[1], historic_file = fcr_files[2])
  bvr_temp_string_daily <- target_generation_ThermistorTemp_C_daily(current_file = bvr_files[1], historic_file = bvr_files[2])


  #Secchi
  source('targets/target_functions/target_generation_daily_secchi_m.R')

  secchi_daily <- target_generation_daily_secchi_m(current = secchi_files[1], edi = secchi_files[2]) |>
    filter(site_id %in% c('fcre', 'bvre'))

  secchi_daily$duration <- 'P1D'
  secchi_daily$project_id <- 'vera4cast'


 # combined targets
  combined_targets <- bind_rows(exo_rdo_daily, fcr_temp_string_daily, bvr_temp_string_daily, secchi_daily) |>
    select(all_of(column_names))

  combined_targets_deduped <- combined_targets |>
    group_by(datetime, site_id, variable, depth_m) |>
    mutate(obs_deduped = mean(observation, na.rm = TRUE)) |>
    ungroup() |>
    distinct(datetime, site_id, variable, depth_m, .keep_all = TRUE) |>
    select(project_id, site_id, datetime, duration, depth_m, variable, observation)

  combined_dup_check <- combined_targets_deduped  %>%
    dplyr::group_by(datetime, site_id, variable, depth_m) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1)


  if (nrow(combined_dup_check) == 0){
    return(combined_targets_deduped)
  }else{
    print('combined physical duplicates found...please fix')
    stop()
  }

}
