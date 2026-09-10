## create drivers for xgboost baseline models ##

## install pacakge for converting salinity to conductivity
#install.packages("gsw")
library(gsw)

config <- yaml::read_yaml("challenge_configuration.yaml")

## Met drivers ##

## pull in future NOAA data
forecast_start_date = Sys.Date()
#forecast_start_date = as.Date('2026-07-30')

noaa_date = forecast_start_date - days (1)
site_id = 'fcre'


## ACCESS FUTURE NOAA MET FORECASTS ##

met_s3_future <- arrow::s3_bucket(file.path("bio230121-bucket01/flare/drivers/met/gefs-v12/stage2",paste0("reference_datetime=",noaa_date),paste0("site_id=",site_id)),
                                  endpoint_override = config$endpoint,
                                  anonymous = TRUE)

df_met_future <- arrow::open_dataset(met_s3_future) |>
  dplyr::filter(variable %in% c("air_pressure",
                                "air_temperature",
                                "northward_wind",
                                "eastward_wind",
                                "surface_downwelling_shortwave_flux_in_air")) |>
  collect() |>
  rename(ensemble = parameter) |>
  mutate(variable = ifelse(variable == "air_pressure", "BP_kPa_mean", variable),
         variable = ifelse(variable == "air_temperature", "AirTemp_C_mean", variable),
         prediction = ifelse(variable == "temperature_2m", prediction - 273.15, prediction),
         date = as.Date(datetime)) |>
    summarise(prediction = mean(prediction, na.rm = TRUE),
            .by = c(any_of(c("site_id", "family")), ensemble, date, variable)) |>
  pivot_wider(names_from = 'variable', values_from = 'prediction') |>
  mutate(WindSpeed_ms_mean = sqrt(northward_wind^2 + eastward_wind^2),
         ensemble = ensemble + 1) |>
  select(datetime = date, parameter = ensemble, BP_kPa_mean, AirTemp_C_mean, surface_downwelling_shortwave_flux_in_air, WindSpeed_ms_mean)



### ACCESS FUTURE FLARE FORECASTS ##

df_wq_future <- arrow::open_dataset(arrow::s3_bucket(paste0("bio230121-bucket01/flare/forecasts/parquet/site_id=",site_id,
                                                            '/model_id=glm_aed_flare_v3/reference_date=',noaa_date,'/'),
                                                     endpoint_override = config$endpoint,
                                                     anonymous = TRUE)) |>
  # filter(forecast == 1,
  #        depth %in% c(1.6, 9, NA)) |>
  filter(variable %in% c('Temp_C_mean','DO_mgL_mean','fDOM_QSU_mean','Chla_ugL_mean', 'depth', 'NIT_amm','secchi',
                         'NIT_nit', 'salt')) |>
  select(-pub_datetime, -forecast, -variable_type, -log_weight, -family, -reference_datetime) |>
  collect()


flare_var_convert <- df_wq_future |>
  # Combine into a vera data frame
  dplyr::rename(depth_m = depth) |>
  dplyr::mutate(#variable = ifelse(variable == "DO_mgL_mean", "DO_mgL_mean_all_depth", variable),
    datetime = ifelse(variable == "DO_mgL_mean", datetime - lubridate::days(1), datetime),
    prediction = ifelse(variable == "DO_mgL_mean", prediction/1000*(32),prediction),
    datetime = ifelse(variable == "Temp_C_mean", datetime - lubridate::days(1), datetime),
    prediction = ifelse(variable == "fDOM_QSU_mean", (151.3407 + prediction)/29.62654,prediction),
    prediction = ifelse(variable == "NIT_amm", prediction/1000/0.001/(1/18.04),prediction),
    variable = ifelse(variable == "NIT_amm", "NH4_ugL_sample", variable),
    prediction = ifelse(variable == "NIT_nit", prediction/1000/0.001/(1/62.00),prediction),
    variable = ifelse(variable == "NIT_nit", "Nitrate", variable),
    variable = ifelse(variable == "secchi", "Secchi_m_sample", variable),
    depth_m = ifelse(variable == 'salt' & depth_m == 1, 1.6, depth_m),
    datetime = lubridate::as_datetime(datetime))

flare_var_selection <- flare_var_convert |>
  filter(depth_m %in% c(1.6, NA) |
           (depth_m == 9 & variable %in% c("Temp_C_mean", "DO_mgL_mean"))) |>
  distinct(datetime, variable, .keep_all = T) |>
  mutate(variable = ifelse(variable == 'Temp_C_mean' & depth_m == 9, 'Temp_C_mean_9m', variable),
         variable = ifelse(variable == 'DO_mgL_mean' & depth_m == 9, 'DO_mgL_mean_9m', variable)) |>
  select(-depth_m) |>
  pivot_wider(names_from = variable, values_from = prediction)


## JOIN FLARE, INFLOW, AND MET PREDICTIONS TOGETHER AND MATCH UP THE ENSEMBLE MEMBERS (parameter column)

## join met drivers onto the FLARE water quality forecast by datetime and parameter
## df_met_future only has n_met_parameters ensemble members while df_wq_future has many
## more, so the met parameters are recycled (wrapped via modulo) to cover every
## flare_var_selection parameter
n_met_parameters <- dplyr::n_distinct(df_met_future$parameter)

df_wq_met_join <- flare_var_selection |>
  mutate(met_parameter = ((parameter - 1) %% n_met_parameters) + 1) |>
  left_join(df_met_future, by = c('datetime' = 'datetime', 'met_parameter' = 'parameter')) |>
  select(-met_parameter) |>
  mutate(SpCond_uScm_mean = gsw::gsw_C_from_SP(SP = salt, t = AirTemp_C_mean, p = 0)*1000) ## CHECK PRESSURE UNIT


## grab future inflow forecasts (not done yet, but join this object onto "df_wq_met_join" by datetime and parameter)
df_inflow_future <- arrow::open_dataset(arrow::s3_bucket(paste0("bio230121-bucket01/vera4cast/forecasts/archive-parquet/project_id=vera4cast/duration=P1D/"),
                                                         endpoint_override = config$endpoint,
                                                         anonymous = TRUE)) |>
  filter(variable %in% c('Temp_C_mean', 'Flow_cms_mean'),
         model_id == 'inflow_gefsClimAED',
         reference_date == noaa_date) |>
  #select(-pub_datetime, -forecast, -variable_type, -log_weight, -family, -reference_datetime) |>
  collect()

# create a full dataset of future predictions that are needed
full_drivers <- df_wq_met_join # this but also join on the df_inflow_future by datetime and parameter




## GENERATE TRAINING DATASET (MIGHT NEED ADD MORE 9M DEPTH VARS (LINE_128) FOR ADDITIONAL CHEM VARS THAT ARE NEEDED)
## THESE DEPTH SPECIFC VARS WILL BE RUN AS SEPARATE VARIABLES
source('./R/xgboost_functions/create_xgboost_training_dataset.R')
## ALL PREVIOUS WATER QUALITY, INFLOW, AND MET DATA NEEDED FOR MODEL TRAINING
df_training <- read_csv('R/xgboost_functions/fcr_xgboost_training_dataset.csv') |>
  pivot_longer(cols = -c(site_id, datetime, depth_m), names_to = 'variable', values_to = 'observation') |>
  filter(depth_m %in% c(1.6, NA) |
           (depth_m == 9 & variable %in% c("Temp_C_mean", "DO_mgL_mean", "CO2_umolL_sample"))) |>
  filter(!grepl('inflow', variable) | variable %in% c('Flow_cms_mean_inflow', 'Temp_C_mean_inflow')) |>
  drop_na(observation) |>
  #mutate(variable = paste0(variable, '_', depth_m)) |>
  distinct(datetime, variable, .keep_all = T) |>
  mutate(variable = ifelse(variable == 'Temp_C_mean' & depth_m == 9, 'Temp_C_mean_9m', variable),
         variable = ifelse(variable == 'DO_mgL_mean' & depth_m == 9, 'DO_mgL_mean_9m', variable),
         variable = ifelse(variable == 'CO2_umolL_sample' & depth_m == 9, 'CO2_umolL_sample_9m', variable)) |>
  select(-depth_m) |>
  pivot_wider(names_from = variable, values_from = observation) |>
  filter(datetime <= noaa_date) ## this should match up with future forecasts...if not you may need to adjust to the true forecast date (noaa_date + 1)


## RUN PREDICTIONS
## EVERYTHING ABOVE ONLY NEEDS TO BUILT ONCE -- BELOW IS WHERE VARIABLE SPECIFIC CODE WILL LIKELY BEGIN

# USING WATER TEMP AS AN EXAMPLE HERE

# CREATE A NEW TRAINING DATASET THAT IS SPECIFIC TO YOUR VARIABLE/FEATURE COMBINATION
var_training_df <- df_training |> select(Temp_C_mean, AirTemp_C_mean, sin_doy)

## MODIFY THIS RECEIPE FOR EACH VARIABLE/FEATURE COMBINATION
model_rec <- recipe(Temp_C_mean ~ AirTemp_C_mean + sin_doy,
                   data = var_training_df)

## BUILD DRIVER DATASET HERE (SHOULD CONTAIN PREDICTIONS OF THE FEATURES NEEDED TO PREDICT THE VARIABLE)
## DRIVER DATA COLUMNS SHOULD MATCH THE COLUMNS IN THE TRAINING DATA -- JUST USE NAs FOR FUTURE VARIABLE
## IN THE CASE OF WATER TEMP PREDICTIONS, YOU MAINLY NEED AIRTEMP FROM NOAA FORECASTS

driver_data <- full_drivers |> select(...) ## select what you need from the drivers for this specific variable

var_predictions <- xg_run_model(train_data = flow_training_df,
                                 model_recipe = flow_rec,
                                 met_combined = df_combined,
                                 targets_df = flow_targets,
                                 drivers_df = flow_drivers,
                                 var_name = 'Temp_C_mean')





## PULL IN PAST WQ AND MET DATA ## (YOU PROBABLY WON'T NEED THIS BECAUSE IT'S ALREADY IN THE TRAINING DATA)
## JUST PUTTING THIS HERE FOR REFERENCE IF NEEDED ##

## pull in past NOAA data
min_datetime <- min(df_met_future$datetime)

met_s3_past <- arrow::s3_bucket(paste0("bio230121-bucket01/flare/drivers/met/gefs-v12/stage3/site_id=",site_id),
                                endpoint_override = config$endpoint,
                                anonymous = TRUE)

years_prior <- forecast_start_date - lubridate::days(1825) # 5 years

df_met_past <- arrow::open_dataset(met_s3_past) |>
    dplyr::filter(variable %in% c("air_pressure",
                                "air_temperature",
                                "northward_wind",
                                "eastward_wind",
                                "surface_downwelling_shortwave_flux_in_air"),
                  datetime < min_datetime,
                  datetime > years_prior) |>
  collect() |>
  rename(ensemble = parameter) |>
  mutate(variable = ifelse(variable == "air_pressure", "BP_kPa_mean", variable),
         variable = ifelse(variable == "air_temperature", "AirTemp_C_mean", variable),
         prediction = ifelse(variable == "temperature_2m", prediction - 273.15, prediction),
         date = as.Date(datetime)) |>
  summarise(prediction = mean(prediction, na.rm = TRUE),
            .by = c(any_of(c("site_id", "family")), ensemble, date, variable)) |>
  pivot_wider(names_from = 'variable', values_from = 'prediction') |>
  mutate(WindSpeed_ms_mean = sqrt(northward_wind^2 + eastward_wind^2),
         ensemble = ensemble + 1) |>
  select(datetime = date, parameter = ensemble, BP_kPa_mean, AirTemp_C_mean, surface_downwelling_shortwave_flux_in_air, WindSpeed_ms_mean)


# combine past and future noaa data
df_combined <- bind_rows(df_met_future, df_met_past) |>
  arrange(date, ensemble)




## PREVIOUS WATER QUALITY DATA
df_wq_past <- read_csv('R/xgboost_functions/fcr_xgboost_training_dataset.csv') |>
  pivot_longer(cols = -c(site_id, datetime, depth_m), names_to = 'variable', values_to = 'observation') |>
  filter(depth_m %in% c(1.6, NA) |
         (depth_m == 9 & variable %in% c("Temp_C_mean", "DO_mgL_mean", "CO2_umolL_sample"))) |>
  filter(!grepl('inflow', variable) | variable %in% c('Flow_cms_mean_inflow', 'Temp_C_mean_inflow')) |>
  drop_na(observation) |>
  #mutate(variable = paste0(variable, '_', depth_m)) |>
  distinct(datetime, variable, .keep_all = T) |>
  mutate(variable = ifelse(variable == 'Temp_C_mean' & depth_m == 9, 'Temp_C_mean_9m', variable),
         variable = ifelse(variable == 'DO_mgL_mean' & depth_m == 9, 'DO_mgL_mean_9m', variable),
         variable = ifelse(variable == 'CO2_umolL_sample' & depth_m == 9, 'CO2_umolL_sample_9m', variable)) |>
  select(-depth_m) |>
  pivot_wider(names_from = variable, values_from = observation)


## join met drivers onto the FLARE water quality forecast by datetime and parameter
## df_met_future only has n_met_parameters ensemble members while df_wq_future has many
## more, so the met parameters are recycled (wrapped via modulo) to cover every
## flare_var_selection parameter
n_met_parameters <- dplyr::n_distinct(df_met_future$parameter)

df_wq_met_join <- flare_var_selection |>
  mutate(met_parameter = ((parameter - 1) %% n_met_parameters) + 1) |>
  left_join(df_met_future, by = c('datetime' = 'datetime', 'met_parameter' = 'parameter')) |>
  select(-met_parameter) |>
  mutate(SpCond_uScm_mean = gsw::gsw_C_from_SP(SP = salt, t = AirTemp_C_mean, p = 0)*1000) ## CHECK PRESSURE UNIT



df_inflow_future <- arrow::open_dataset(arrow::s3_bucket(paste0("bio230121-bucket01/vera4cast/forecasts/archive-parquet/project_id=vera4cast/duration=P1D/"),
                                                         endpoint_override = config$endpoint,
                                                         anonymous = TRUE)) |>
  filter(variable %in% c('Temp_C_mean', 'Flow_cms_mean'),
         model_id == 'inflow_gefsClimAED',
         reference_date == noaa_date) |>
  #select(-pub_datetime, -forecast, -variable_type, -log_weight, -family, -reference_datetime) |>
  collect()

df_inflow_future_edited <- df_inflow_future


## pivot flare df to be wide. Make Temp/DO @9m a separate variable
## pivot inflow df wider and join onto flare predictions by datetime, ensemble
















forecast_temp <- df_combined |>
  dplyr::filter(variable == 'temperature_2m') |>
  summarise(temp_hourly = median(prediction, na.rm = TRUE), .by = c("datetime")) |> # get the median hourly temp across all EMs
  mutate(date = lubridate::as_date(datetime)) |>
  summarise(temperature = median(temp_hourly, na.rm = TRUE), .by = c("date")) # get median temp across hours of the day

forecast_met <- forecast_precip |>
  right_join(forecast_temp, by = c('date'))


print('done setting up met data')

## RUN PREDICTIONS
#sensorcode_df <- read_csv('configuration/default/sensorcode.csv', show_col_types = FALSE)
inflow_targets <- read_csv(file.path(config_obs$file_path$targets_directory, config$location$site_id,
                                     paste0(config$location$site_id,"-targets-inflow.csv")), show_col_types = FALSE)


inflow_targets <- read_csv(file.path('targets', config$location$site_id, paste0(config$location$site_id,"-targets-inflow.csv")), show_col_types = FALSE)


## RUN FLOW PREDICTIONS
print('Running Flow Inflow Forecast')

flow_targets <- inflow_targets |>
  dplyr::filter(variable == 'FLOW') |>
  rename(date = datetime)

flow_drivers <- forecast_met |>
  left_join(flow_targets, by = c('date')) |>
  drop_na(observation)

flow_training_df <- flow_drivers |>
  dplyr::filter(date < reference_datetime)

flow_rec <- recipe(observation ~ precip + sevenday_precip + doy + temperature,
                   data = flow_training_df)

flow_predictions <- xg_run_model(train_data = flow_training_df,
                                        model_recipe = flow_rec,
                                        met_combined = df_combined,
                                        targets_df = flow_targets,
                                        drivers_df = flow_drivers,
                                        var_name = 'FLOW')

# ## RUN TEMPERATURE PREDICTIONS
# print('Running Temperature Inflow Forecast')
#
# temp_targets <- inflow_targets |>
#   dplyr::filter(variable == 'TEMP') |>
#   rename(date = datetime)
#
# temp_drivers <- forecast_met |>
#   left_join(flow_targets, by = c('date')) |>
#   drop_na(observation)
#
# temp_training_df <- temp_drivers |>
#   dplyr::filter(date < reference_datetime)
#
# temp_rec <- recipe(observation ~ doy + temperature,
#                    data = temp_training_df)
#
# temp_predictions <- xg_run_inflow_model(train_data = temp_training_df,
#                                         model_recipe = temp_rec,
#                                         met_combined = df_combined,
#                                         targets_df = temp_targets,
#                                         drivers_df = temp_drivers,
#                                         var_name = 'TEMP')
