#Format data for NNETAR model for chl-a
#Author: Mary Lofton
#Date: 10OCT23

#Purpose: format data for ARIMA model for chla from 2018-present

#'Function to fit day of year model for chla
#'@param targets filepath to exo targets for VERA
#'@param end_date yyyy-mm-dd today's date

#load packages
library(tidyverse)
library(lubridate)
library(zoo)

format_data_NNETAR <- function(targets, target_var, end_date, depth_select){

  #read in targets
  dat <- read_csv(targets, col_types = cols()) %>%
    mutate(datetime = as.Date(datetime)) |>
    filter(variable == target_var) %>%
    arrange(site_id, datetime)

  sites <- unique(dat$site_id)

  start_dates <- dat %>%
    group_by(site_id) %>%
    filter(!is.na(observation)) %>%
    slice(1) %>%
    pull(datetime)

  #get list of dates
  end_date = as.Date(end_date)
  daily_dates_df = tibble(datetime = Date(length = 0L),
                          site_id = character(length = 0L))
  for(i in 1:length(start_dates)){
    temp_dates <- seq.Date(from = as.Date(start_dates[i]), to = as.Date(end_date), by = "day")
    temp_sites <- rep(sites[i], times = length(temp_dates))
    temp_df <- tibble(datetime = temp_dates,
                      site_id = temp_sites)
    daily_dates_df <- bind_rows(daily_dates_df, temp_df)
  }

  #join to dates and interpolate
  dat1 <- left_join(daily_dates_df, dat, by = c("datetime","site_id")) %>%
    group_by(site_id) %>%
    mutate(observation = interpolate(observation)) |>
    tidyr::fill(project_id, .direction = "downup") |>
    tidyr::fill(duration, .direction = "downup") |>
    tidyr::fill(depth_m, .direction = "downup") |>
    tidyr::fill(variable, .direction = "downup")

  if (target_var %in% c('Temp_C_mean', 'DO_mgL_mean', "fDOM_QSU_mean", "CH4_umolL_sample")){
    bvr_data <- dat1 |>
      filter(site_id == 'bvre',
             depth_m == depth_select[1])

    fcr_data <- dat1 |>
      filter(site_id == 'fcre',
             depth_m == depth_select[2])

    dat1 <- bind_rows(bvr_data, fcr_data)
  }

  ## all site members need to have gaps removed for use in tsibble (same start and end date)
  min_date_check <- dat1 |>
    mutate(datetime = as.Date(datetime)) |>
    group_by(site_id) |>
    summarise(min_date = min(datetime)) |>
    ungroup()

  min_site_date <- max(min_date_check$min_date)

  df_date_adjust <- dat1 %>%
    mutate(datetime = as.Date(datetime)) |>
    filter(datetime >= as.Date(min_site_date))

  ## remove NA rows for less frequent variables (i.e secchi)
  #dat_final <- df_date_adjust |> drop_na(variable)

  # if (target_var %in% c('Secchi_m_sample')){
  #   dat_final$datetime <- tsibble::yearweek(dat_final$datetime)
  # }

  dat_final <- df_date_adjust |> distinct(datetime, site_id, .keep_all = TRUE)

  return(dat_final)
}
