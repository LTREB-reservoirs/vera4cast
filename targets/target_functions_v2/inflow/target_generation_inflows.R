##### inflow targets generation function
## author: Austin Delany
## last edited : 2023/10/11
## edited: 15 April 2025 - added the current ghg df and included CO2

library(tidyverse)
## files used in function

# current_inflow <- 'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-weir-data-qaqc/FCRWeir_L1.csv'
#
# historic_inflow <- "https://pasta.lternet.edu/package/data/eml/edi/202/10/c065ff822e73c747f378efe47f5af12b"
#
# historic_silica <- 'https://pasta.lternet.edu/package/data/eml/edi/542/1/791ec9ca0f1cb9361fa6a03fae8dfc95'
#
# historic_nutrients <- "https://pasta.lternet.edu/package/data/eml/edi/199/11/509f39850b6f95628d10889d66885b76"
#
# historic_ghg <- "https://pasta.lternet.edu/package/data/eml/edi/551/7/38d72673295864956cccd6bbba99a1a3"
#
# current_ghg <- "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Raw_GHG/L1_manual_GHG.csv"


target_generation_inflows <- function(historic_inflow, current_inflow, historic_nutrients, historic_silica, historic_ghg, current_ghg){

  # current flow
  df_current_in <- read_csv(current_inflow)

  # read in historic inflow file
  df_hist_in <- read_csv(historic_inflow)

  df_inflow <- bind_rows(df_current_in, df_hist_in) |>
    mutate(sampledate = as.Date(DateTime)) |>
    mutate(inflow = ifelse(is.na(VT_Flow_cms), WVWA_Flow_cms, VT_Flow_cms)) |>
    mutate(inflow_temp = ifelse(is.na(VT_Temp_C), WVWA_Temp_C, VT_Temp_C)) |>
    group_by(sampledate) |>
    mutate(Flow_cms_mean = mean(inflow, na.rm = TRUE)) |>
    mutate(Temp_C_mean = mean(inflow_temp, na.rm = TRUE)) |>
    ungroup() |>
    distinct(sampledate, .keep_all = TRUE) |>
    select(sampledate, Flow_cms_mean, Temp_C_mean) #|>
    #pivot_longer(!date , names_to = 'variable' , values_to = 'observation')

  df_inflow$Flow_cms_mean <- ifelse(is.nan(df_inflow$Flow_cms_mean), NA, df_inflow$Flow_cms_mean)
  df_inflow$Temp_C_mean <- ifelse(is.nan(df_inflow$Temp_C_mean), NA, df_inflow$Temp_C_mean)



  #historic nutrients

  df_hist_nutr <- read_csv(historic_nutrients) |>
    filter(Reservoir == 'FCR', Site == 100) |>
    mutate(sampledate = as.Date(DateTime)) |>
    select(3:14, sampledate, -Depth_m, -Rep, -DateTime) |>
    rename(TN_ugL_sample = TN_ugL, TP_ugL_sample = TP_ugL, NH4_ugL_sample = NH4_ugL,  NO3NO2_ugL_sample = NO3NO2_ugL, SRP_ugL_sample = SRP_ugL,
           DOC_mgL_sample = DOC_mgL, DIC_mgL_sample = DIC_mgL, DC_mgL_sample = DC_mgL, DN_mgL_sample = DN_mgL) |>
    pivot_longer(!sampledate, names_to = 'variables', values_to = 'observations') |>
    drop_na(observations) |>
    group_by(sampledate, variables) |>
    mutate(obs_averaged = mean(observations, na.rm = TRUE)) |>
    ungroup() |>
    distinct(sampledate, variables, .keep_all = TRUE) |>
    select(-observations) |>
    pivot_wider(names_from = 'variables', values_from = 'obs_averaged')

  # silica
  df_silica <- read_csv(historic_silica) |>
    filter(Reservoir == 'FCR', Site == 100) |>
    mutate(sampledate = as.Date(DateTime)) |>
    group_by(sampledate) |>
    mutate(DRSI_mgL_sample = mean(DRSI_mgL, na.rm = TRUE)) |>
    ungroup() |>
    distinct(sampledate, .keep_all = TRUE) |>
    select(sampledate, DRSI_mgL_sample)

  # GHG - ch4?
  # include CO2 a well
  df_ghg <- read_csv(c(historic_ghg, current_ghg)) |>
    bind_rows()|>
    filter(Reservoir == 'FCR', Site == 100) |>
    select(-Site,-starts_with("Flag"))|> # get rid of the columns we don't want
    mutate(sampledate = as.Date(DateTime)) |>
    drop_na() |>
    group_by(Reservoir,Depth_m,sampledate)%>%
    summarise_if(is.numeric, mean, na.rm = TRUE)%>% # average if there are reps taken at a depths
    ungroup()|>
      group_by(Reservoir,Depth_m,sampledate)%>% # average if there are more than one sample taken during that day
      summarise_if(is.numeric, mean, na.rm = TRUE)%>%
      ungroup()|>
      select(-Rep)|>
      rename(CO2_umolL_sample=CO2_umolL, # rename the columns for standard notation
             CH4_umolL_sample=CH4_umolL)|>
    distinct(sampledate, .keep_all = TRUE)|>
        select(sampledate, CH4_umolL_sample, CO2_umolL_sample)


  # build df of nutrients that match daily sensor data
  df_add_nutr_si_ghg <- df_inflow |>
    full_join(df_hist_nutr, by = c('sampledate')) |>
    full_join(df_silica, by = c('sampledate')) |>
    full_join(df_ghg, by = c('sampledate'))

  df_long <- df_add_nutr_si_ghg |>
    pivot_longer(!sampledate, names_to = 'variable', values_to = 'observation')

  # grab the missing inflow values that you want to keep (so user knows it's missing data and not just missing rows)
  df_missing_inflows_store <- df_long |>
    filter(is.nan(observation))

  df_inflow_targets_final <- df_long |>
    drop_na(observation) |>
    bind_rows(df_missing_inflows_store) |>
    mutate(datetime = as.POSIXct(format(as.POSIXct(paste(as.character(sampledate), '00:00:00'), tz = "UTC"), "%Y-%m-%d %H:%M:%S"), tz = 'UTC')) |>
    arrange(datetime) |>
    select(-sampledate)

  df_inflow_targets_final$observation <- ifelse(is.nan(df_inflow_targets_final$observation), NA, df_inflow_targets_final$observation)

  df_inflow_targets_final$site_id <- 'tubr'
  df_inflow_targets_final$depth_m <- NA
  df_inflow_targets_final$duration <- 'P1D'
  df_inflow_targets_final$project_id <- 'vera4cast'

  ## check rounding for observations
  non_rounded_vars <- c('Flow_cms_mean')
  df_inflow_targets_final$observation <- ifelse(!(df_inflow_targets_final$variable %in% non_rounded_vars),
                                           round(df_inflow_targets_final$observation, digits = 2),
                                           df_inflow_targets_final$observation)



  ## FINAL DUPLICATE CHECK
  inflow_dup_check <- df_inflow_targets_final  %>%
    dplyr::group_by(datetime, site_id, depth_m, duration, project_id, variable) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1)

  if (nrow(inflow_dup_check) == 0){
    return(df_inflow_targets_final)
  }else{
   print('Inflow duplicates found...please fix')
    stop()
  }


}

# t <- target_generation_inflows(historic_inflow = historic_inflow,
#                        current_inflow = current_inflow,
#                        historic_nutrients = historic_nutrients,
#                        historic_silica = historic_silica,
#                        historic_ghg = historic_ghg
#                        current_ghg = current_ghg)
