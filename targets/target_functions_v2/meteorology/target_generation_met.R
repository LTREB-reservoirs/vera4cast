library(tidyverse)

# current_met <- 'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCRmet_L1.csv'
# historic_met <- 'https://pasta.lternet.edu/package/data/eml/edi/389/7/02d36541de9088f2dd99d79dc3a7a853'

target_generation_met <- function(current_met, historic_met, time_interval){

  df_current <- read_csv(current_met) |>
    select(DateTime, "PAR_umolm2s_Average":"ShortwaveRadiationUp_Average_W_m2", "InfraredRadiationUp_Average_W_m2", 'Albedo_Average_W_m2')

  df_historic <- read_csv(historic_met) |>
    select(DateTime, "PAR_umolm2s_Average":"ShortwaveRadiationUp_Average_W_m2", "InfraredRadiationUp_Average_W_m2", 'Albedo_Average_W_m2')

  df_combined <- bind_rows(df_current, df_historic) |>
    #select(DateTime, "PAR_umolm2s_Average":"Albedo_Average_W_m2") |>
    mutate(sampledatetime_local = force_tz(as.POSIXct(DateTime), tz="EST", roll_dst = c('pre'))) |>  ## ADDED PRE TO FIX NA ISSUE
    mutate(sampledatetime_utc = with_tz(sampledatetime_local, tz = 'UTC')) |>
    #select(sampledatetime_utc, "PAR_umolm2s_Average":"Albedo_Average_W_m2") |>
    select(sampledatetime_utc, PAR_umolm2s_mean = PAR_umolm2s_Average, BP_kPa_mean = BP_Average_kPa,
           AirTemp_C_mean = AirTemp_C_Average, RH_percent_mean = RH_percent, Rain_mm_sum = Rain_Total_mm,
           WindSpeed_ms_mean = WindSpeed_Average_m_s, WindDir_degrees_mean = WindDir_degrees,
           ShortwaveRadiationUp_Wm2_mean = ShortwaveRadiationUp_Average_W_m2, InfraredRadiationUp_Wm2_mean = InfraredRadiationUp_Average_W_m2,
           Albedo_Wm2_mean = Albedo_Average_W_m2)

  rain_total_df <- df_combined |> select(sampledatetime_utc, Rain_mm_sum) # save rain total separately so it can get averaged

  df_combined <- df_combined |> select(-Rain_mm_sum) # remove rain total value so it doesn't get averaged

  # remove the large files that we don't need to use anymore
  rm(df_current, df_historic)

  if (time_interval == 'daily'){

    df_rain_long <- rain_total_df |>
      pivot_longer(!sampledatetime_utc, names_to = 'variable', values_to = 'observation') |>
      mutate(sampledate = as.Date(sampledatetime_utc)) |>
      group_by(sampledate) |>
      mutate(obs_sum = sum(observation, na.rm = TRUE)) |>
      ungroup() |>
      distinct(sampledate, .keep_all = TRUE) |>
      mutate(datetime = as.POSIXct(format(as.POSIXct(paste(as.character(sampledate), '00:00:00'), tz = "UTC"), "%Y-%m-%d %H:%M:%S"), tz = 'UTC')) |>
      select(datetime, variable, observation = obs_sum)

    df_long_conversion <- df_combined |>
      pivot_longer(!sampledatetime_utc, names_to = 'variable', values_to = 'observation') |>
      mutate(sampledate = as.Date(sampledatetime_utc)) |>
      group_by(sampledate, variable) |>
      mutate(obs_averaged = mean(observation, na.rm = TRUE)) |>
      ungroup() |>
      distinct(sampledate, variable, .keep_all = TRUE) |>
      mutate(datetime = as.POSIXct(format(as.POSIXct(paste(as.character(sampledate), '00:00:00'), tz = "UTC"), "%Y-%m-%d %H:%M:%S"), tz = 'UTC')) |>
      select(datetime, variable, observation = obs_averaged) |>
      bind_rows(df_rain_long) ## last thing is to append rain total values

    df_long_conversion$observation <- ifelse(is.nan(df_long_conversion$observation), NA, df_long_conversion$observation)


    df_long_conversion$site_id <- 'fcre'
    df_long_conversion$depth_m <- NA
    df_long_conversion$duration <- 'P1D'
    df_long_conversion$project_id <- 'vera4cast'

    ## check rounding for observations
    non_rounded_vars <- c('')
    df_long_conversion$observation <- ifelse(!(df_long_conversion$variable %in% non_rounded_vars),
                                             round(df_long_conversion$observation, digits = 2),
                                             df_long_conversion$observation)

    ## FINAL DUPLICATE CHECK
    met_dup_check <- df_long_conversion  %>%
      dplyr::group_by(datetime, site_id, depth_m, duration, project_id, variable) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n > 1)

    if (nrow(met_dup_check) == 0){
      return(df_long_conversion)
    }else{
      print('Inflow duplicates found...please fix')
    }

  } else if(time_interval == 'hourly'){

    df_rain_long <- rain_total_df |>
      pivot_longer(!sampledatetime_utc, names_to = 'variable', values_to = 'observation') |>
      mutate(sample_hour = format(as.POSIXct(sampledatetime_utc), format = "%H")) |>
      mutate(sampledate = as.Date(sampledatetime_utc)) |>
      group_by(sampledate, sample_hour) |>
      mutate(obs_sum = sum(observation, na.rm = TRUE)) |>
      ungroup() |>
      distinct(sampledate, sample_hour, .keep_all = TRUE) |>
      mutate(datetime = as.POSIXct(format(as.POSIXct(paste0(as.character(sampledate),' ', sample_hour,':00:00'), tz = "UTC"), "%Y-%m-%d %H:%M:%S"), tz = 'UTC')) |>
      select(datetime, variable, observation = obs_sum)

  df_long_conversion <- df_combined |>
    pivot_longer(!sampledatetime_utc, names_to = 'variable', values_to = 'observation') |>
    mutate(sample_hour = format(as.POSIXct(sampledatetime_utc), format = "%H")) |>
    mutate(sampledate = as.Date(sampledatetime_utc)) |>
    group_by(sampledate, sample_hour, variable) |>
    mutate(obs_averaged = mean(observation, na.rm = TRUE)) |>
    ungroup() |>
    distinct(sampledate, sample_hour, variable, .keep_all = TRUE) |>
    mutate(datetime = as.POSIXct(format(as.POSIXct(paste0(as.character(sampledate),' ', sample_hour,':00:00'), tz = "UTC"), "%Y-%m-%d %H:%M:%S"), tz = 'UTC')) |>
    select(datetime, variable, observation = obs_averaged) |>
    bind_rows(df_rain_long) ## last thing is to append rain total values

  df_long_conversion$observation <- ifelse(is.nan(df_long_conversion$observation), NA, df_long_conversion$observation)

  df_long_conversion$site_id <- 'fcre'
  df_long_conversion$depth_m <- NA
  df_long_conversion$duration <- 'PT1H'
  df_long_conversion$project_id <- 'vera4cast'

  ## check rounding for observations
  non_rounded_vars <- c('')
  df_long_conversion$observation <- ifelse(!(df_long_conversion$variable %in% non_rounded_vars),
                                           round(df_long_conversion$observation, digits = 2),
                                           df_long_conversion$observation)

  ## FINAL DUPLICATE CHECK
  met_dup_check <- df_long_conversion  %>%
    dplyr::group_by(datetime, site_id, depth_m, duration, project_id, variable) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1)

  if (nrow(met_dup_check) == 0){
    return(df_long_conversion)
  }else{
    print('Inflow duplicates found...please fix')
  }

  } else{
    print('Please specify correct time interval (daily, hourly)')
  }

}


# a <- target_generation_met(current_met = current_met, historic_met = historic_met, time_interval = 'daily')
# b <- target_generation_met(current_met = current_met, historic_met = historic_met, time_interval = 'hourly')
