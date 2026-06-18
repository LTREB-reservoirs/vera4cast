#fableNNETAR predictions for 30 days ahead
#Author: Mary Lofton (modified by Austin Delany)
#Date: 10OCT23 (last modified 24Jun24)

#Purpose: make predictions with ETS model

library(fable)
library(feasts)
library(urca)

#'Function to fit day of year model for chla
#'@param data data frame with columns Date (yyyy-mm-dd) and
#'median daily EXO_chla_ugL_1 with chl-a measurements in ug/L

fableNNETAR <- function(data, target_var, reference_datetime, forecast_horizon, depth_select){

  #assign target and predictors
  df <- data %>%
    select(-depth_m, -variable) %>%
    as_tsibble(key = site_id, index = datetime) |>
    tsibble::fill_gaps(.full = TRUE) |>
    group_by(site_id) |>
    mutate(observation = na.locf(observation, fromLast = FALSE, na.rm = FALSE)) |>
    ungroup()

  #df <- tsibble::fill_gaps(df, .full = TRUE)

  #fit NNETAR from fable package
  if (target_var %in% c('Chla_ugL_mean')){
    my.nnar <- df %>%
      model(nnar = fable::NNETAR(log(observation + 0.001)))
  } else {
    my.nnar <- df %>%
      model(nnar = fable::NNETAR(observation))
  }

  fitted_values <- fitted(my.nnar)

  #build output df
  df.out <- data.frame(site_id = df$site_id,
                       model_id = "fableNNETAR_focal",
                       datetime = df$datetime,
                       variable = target_var,
                       depth_m = depth_select[2],
                       observation = df$observation,
                       prediction = fitted_values$.fitted)

  #get process error
  sd_resid <- sd(df.out$prediction - df.out$observation)

  #create "new data" dataframe
  fc_dates <- seq.Date(from = reference_datetime, to = reference_datetime + forecast_horizon, by = "day")
  new_data <- tibble(datetime = rep(fc_dates,times = 2),
                     site_id = rep(unique(df.out$site_id), each = length(fc_dates)),
                     observation = NA) %>%
    as_tsibble(key = site_id, index = datetime)

  #make forecast
  fc <- forecast(my.nnar, new_data = new_data, bootstrap = TRUE, times = 500)

  ensemble <- matrix(data = NA, nrow = length(fc$observation), ncol = 500)
  for(i in 1:length(fc$observation)){
    ensemble[i,] <- unlist(fc$observation[i])
  }

  ensemble_df <- data.frame(ensemble) %>%
    add_column(site_id = rep(unique(df.out$site_id), each = length(fc_dates)),
               datetime = rep(fc_dates,times = 2),
               reference_datetime = reference_datetime,
               family = "ensemble",
               variable = target_var,
               model_id = "fableNNETAR_focal",
               duration = "P1D",
               project_id = "vera4cast",
               depth_m = ifelse(site_id == "fcre", depth_select[2], depth_select[1])) %>%
    pivot_longer(X1:X500, names_to = "parameter", values_to = "prediction") %>%
    mutate(across(parameter, substr, 2, nchar(parameter)))

  if (target_var %in% c('Secchi_m_sample', "CO2flux_umolm2s_mean", "CH4flux_umolm2s_mean")){
    ensemble_df$depth_m = NA
  }

  #return ensemble output in EFI format
  return(ensemble_df)
}
