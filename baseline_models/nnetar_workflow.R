#Format data for each model and fit models from 2018-2021
#Author: Austin Delany
#Date: 15Apr2024

#Purpose:create function to create nnetar model predictions for water quality variables

library(tidyverse)
library(lubridate)
library(vera4castHelpers)
library(zoo)

if(exists("curr_reference_datetime") == FALSE){

  curr_reference_datetime <- Sys.Date()

}else{

  print('Running Reforecast')

}

#Load data formatting functions
data.format.functions <- list.files("./R/nnetar_helper_functions/")
sapply(paste0("./R/nnetar_helper_functions/", data.format.functions),source,.GlobalEnv)

#Define targets filepath
targets <- "https://amnh1.osn.mghpcc.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"

#target_variable <- 'Chla_ugL_mean'
#target_variable <- 'Temp_C_mean'
#target_variable <- 'DO_mgL_mean'
#target_variable <- 'Secchi_m_sample'

print('predicting shallow variables...')
target_variables <- c('Temp_C_mean', "DO_mgL_mean", "fDOM_QSU_mean", "CH4_umolL_sample")

prediction_df_shallow <- data.frame()

for (t in target_variables){

  print(t)

  #Define start and end dates (needed for interpolation)
  end_date = curr_reference_datetime

  #Set prediction window and forecast horizon
  reference_datetime <- curr_reference_datetime
  forecast_horizon = 35

  if (t == 'CH4_umolL_sample'){
    dat_NNETAR <- format_data_NNETAR(targets = targets,
                                     target_var = t,
                                     end_date = end_date,
                                     depth_select = c(0.1, 1.6))
    #Predict variable
    pred <- fableNNETAR(data = dat_NNETAR,
                        target_var = t,
                        reference_datetime = reference_datetime,
                        forecast_horizon = forecast_horizon,
                        depth_select = c(0.1, 1.6))

  } else{
    #Format data
    dat_NNETAR <- format_data_NNETAR(targets = targets,
                                     target_var = t,
                                     end_date = end_date,
                                     depth_select = c(1.5, 1.6))
    #Predict variable
    pred <- fableNNETAR(data = dat_NNETAR,
                        target_var = t,
                        reference_datetime = reference_datetime,
                        forecast_horizon = forecast_horizon,
                        depth_select = c(1.5, 1.6))
    }



  # calculate probability of bloom -- if target variables include chla
  if (t %in% c('Chla_ugL_mean')){
    mod <- pred %>%
      mutate(bloom = ifelse(prediction >= 20, 1, 0)) %>%
      group_by(site_id, datetime, reference_datetime, family, variable, model_id, duration, project_id, depth_m) %>%
      summarize(prediction = sum(bloom)/1000) %>%
      mutate(family = "bernoulli",
             variable = "Bloom_binary_mean") %>%
      add_column(parameter = "prob")

    fc <- bind_rows(pred, mod)

    pred <- fc

    print('Bloom_binary_mean')
  }

  prediction_df_shallow <- bind_rows(prediction_df_shallow, pred)

} # close variable iteration loop



print('predicting deep variables...')

target_variables_deep <- c('Temp_C_mean', "DO_mgL_mean","CH4_umolL_sample")

prediction_df_deep <- data.frame()

for (t in target_variables_deep){

  print(t)

  #Define start and end dates (needed for interpolation)
  end_date = curr_reference_datetime

  #Format data
  dat_NNETAR <- format_data_NNETAR(targets = targets,
                                   target_var = t,
                                   end_date = end_date,
                                   depth_select = c(9,9))

  #Set prediction window and forecast horizon
  reference_datetime <- curr_reference_datetime
  forecast_horizon = 35

  #Predict variable
  pred <- fableNNETAR(data = dat_NNETAR,
                      target_var = t,
                      reference_datetime = reference_datetime,
                      forecast_horizon = forecast_horizon,
                      depth_select = c(9,9))

  # calculate probability of bloom -- if target variables include chla
  if (t %in% c('Chla_ugL_mean')){
    mod <- pred %>%
      mutate(bloom = ifelse(prediction >= 20, 1, 0)) %>%
      group_by(site_id, datetime, reference_datetime, family, variable, model_id, duration, project_id, depth_m) %>%
      summarize(prediction = sum(bloom)/1000) %>%
      mutate(family = "bernoulli",
             variable = "Bloom_binary_mean") %>%
      add_column(parameter = "prob")

    fc <- bind_rows(pred, mod)

    pred <- fc

    print('Bloom_binary_mean')
  }

  prediction_df_deep <- bind_rows(prediction_df_deep, pred)

} # close variable iteration loop


## non-depth variables
target_variables_no_depth <- c("Secchi_m_sample", "CO2flux_umolm2s_mean", "CH4flux_umolm2s_mean")


prediction_df_no_depth <- data.frame()

for (t in target_variables_no_depth){

  print(t)

  #Define start and end dates (needed for interpolation)
  end_date = curr_reference_datetime

  #Set prediction window and forecast horizon
  reference_datetime <- curr_reference_datetime
  forecast_horizon = 35


  #Format data
  dat_NNETAR <- format_data_NNETAR(targets = targets,
                                   target_var = t,
                                   end_date = end_date,
                                   depth_select = c(NA, NA))
  #Predict variable
  pred <- fableNNETAR(data = dat_NNETAR,
                      target_var = t,
                      reference_datetime = reference_datetime,
                      forecast_horizon = forecast_horizon,
                      depth_select = c(NA, NA))

  prediction_df_no_depth <- bind_rows(prediction_df_no_depth, pred)

} # close variable iteration loop




prediction_df <- bind_rows(prediction_df_shallow, prediction_df_deep, prediction_df_no_depth)

# Submit forecasts
theme <- 'daily'
date <- curr_reference_datetime

forecast_models <- c("fableNNETAR_focal")

forecast_name <- c(paste0(forecast_models, ".csv"))

# Write the file locally
forecast_file <- paste(theme, date, forecast_name, sep = '-')

forecast_file_abs_path <- paste0("./model_output/fable_NNETAR_focal/",forecast_file)

# write to file
print('Writing File...')

if (!file.exists("./model_output/fable_NNETAR_focal")){
  dir.create("./model_output/fable_NNETAR_focal")
}
write.csv(prediction_df, forecast_file_abs_path, row.names = FALSE)

# validate
print('Validating File...')
vera4castHelpers::forecast_output_validator(forecast_file_abs_path)
vera4castHelpers::submit(forecast_file_abs_path, s3_region = "submit", s3_endpoint = "ltreb-reservoirs.org", first_submission = FALSE)
