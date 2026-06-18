#fableETS chl-a predictions for 30 days ahead
#Author: Mary Lofton
#Date: 26SEP23

#Purpose: make predictions with ETS model

library(fable)
library(feasts)
library(urca)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L

fableETS <- function(targets,
                     forecast_date = Sys.Date(),
                     h,
                     site,
                     var,
                     depth = 'target',
                     model_id = 'vera_ETS'){

  reference_datetime <- forecast_date

  if (depth == 'target') {
    # only generates forecasts for target depths
    target_depths <- c(1.5, 1.6, NA)
  } else {
    target_depths <- depth
  }

  targets_ts <- targets |>
    mutate(datetime = lubridate::as_date(datetime)) |>
    filter(variable %in% var,
           site_id %in% site,
           depth_m %in% target_depths,
           datetime < forecast_date) |>
    group_by(variable, site_id, depth_m, duration, project_id, datetime) |>
    summarise(observation = mean(observation), .groups = 'drop') |>  # get rid of the repeat observations by finding the mean
    as_tsibble(key = c('variable', 'site_id', 'depth_m', 'duration', 'project_id'), index = 'datetime') |>
    # add NA values up to today (index)
    fill_gaps(.end = forecast_date) |>
    #mutate(observation = na_locf(observation, fromLast = FALSE, na.rm = FALSE))
    mutate(observation = na_locf(observation, option = 'nocb', na_remaining = 'keep')) |> # fill in nas and handle nas at ts beginning
    mutate(observation = na_locf(observation, option = 'locf', na_remaining = 'rm')) |>  ## fill in nas at end of ts
    as_tsibble(key = site_id, index = datetime)



  # # Work out when the forecast should start
  # forecast_starts <- targets %>%
  #   dplyr::filter(!is.na(observation) & site_id == site & variable == var & datetime < forecast_date) %>%
  #   # Start the day after the most recent non-NA value
  #   dplyr::summarise(start_date = as_date(max(datetime)) + lubridate::days(1)) %>% # Date
  #   dplyr::mutate(h = (forecast_date - start_date) + h) %>% # Horizon value
  #   dplyr::ungroup()
  #
  # # filter the targets data set to the site_var pair
  # targets_use <- targets_ts |>
  #   dplyr::filter(datetime < forecast_starts$start_date) |>
  #   as_tsibble(key = site_id, index = datetime)

  # #assign target and predictors
  # df <- targets_ts %>%
  #   select(-depth_m, -variable) %>%
  #   as_tsibble(key = site_id, index = datetime)

  #fit ARIMA from fable package
  if (var == 'Chla_ugL_mean'){
    my.ets <- targets_ts %>%
      model(ets = fable::ETS(log(observation + 0.001)))
  } else{
    my.ets <- targets_ts %>%
      model(ets = fable::ETS(observation))
  }



  # ### other code method ###
  # forecast <- my.ets |> fabletools::forecast(h = as.numeric(forecast_starts$h))
  # #forecast <- RW_model %>% fabletools::forecast(h = as.numeric(forecast_starts$h))
  #
  # # extract parameters
  # parameters <- distributional::parameters(forecast$observation)
  #
  # # make right format
  # forecast <- bind_cols(forecast, parameters) |>
  #   pivot_longer(mu:sigma,
  #                names_to = 'parameter',
  #                values_to = 'prediction') |>
  #   mutate(model_id = model_id,
  #          family = 'normal',
  #          reference_datetime=forecast_date) |>
  #   select(all_of(c("model_id", "datetime", "reference_datetime","site_id", "variable", "family",
  #                   "parameter", "prediction", "project_id", "duration", "depth_m" ))) |>
  #   select(-any_of('.model')) |>
  #   filter(datetime > reference_datetime) |>
  #   ungroup() |>
  #   as_tibble()

  ###

  fitted_values <- fitted(my.ets)

  #build output df
  df.out <- data.frame(site_id = site,
                       model_id = model_id,
                       datetime = targets_ts$datetime,
                       variable = var,
                       depth_m = unique(targets_ts$depth_m),
                       observation = targets_ts$observation,
                       prediction = fitted_values$.fitted)

  #get process error
  sd_resid <- sd(df.out$prediction - df.out$observation)

  #create "new data" dataframe
  fc_dates <- seq.Date(from = reference_datetime, to = reference_datetime + h, by = "day")
  new_data <- tibble(datetime = fc_dates,
                     #site_id = rep(unique(df.out$site_id), each = length(fc_dates)),
                     site_id = site,
                     observation = NA,
                     variable = var,
                     depth_m = unique(targets_ts$depth_m),
                     duration = 'P1D',
                     project = 'vera4cast') |>
    as_tsibble(key = site_id, index = datetime)

  #make forecast
  fc <- forecast(my.ets, new_data = new_data, bootstrap = TRUE, times = 500)

  ensemble <- matrix(data = NA, nrow = length(fc$observation), ncol = 500)
  for(i in 1:length(fc$observation)){
    ensemble[i,] <- unlist(fc$observation[i])
  }

  ensemble_df <- data.frame(ensemble) %>%
    add_column(site_id = site,
               #datetime = rep(fc_dates,times = 2),
               datetime = fc_dates,
               reference_datetime = reference_datetime,
               family = "ensemble",
               variable = var,
               model_id = model_id,
               duration = "P1D",
               project_id = "vera4cast",
               depth_m = unique(targets_ts$depth_m)) |>
    pivot_longer(X1:X500, names_to = "parameter", values_to = "prediction") %>%
    mutate(across(parameter, substr, 2, nchar(parameter)))

  if (var %in% c('Secchi_m_sample', "CO2flux_umolm2s_mean", "CH4flux_umolm2s_mean")){
    ensemble_df$depth_m = NA
  }

  ensemble_df <- as.data.frame(ensemble_df)

  #return ensemble output in EFI format
  return(ensemble_df)
}
