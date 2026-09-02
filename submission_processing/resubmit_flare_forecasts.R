library(tidyverse)


## can't have defined credentials during vera submit
Sys.unsetenv("AWS_ACCESS_KEY_ID")
Sys.unsetenv("AWS_SECRET_ACCESS_KEY")

# check for any missing forecasts
message("==== Checking for missed forecasts ====")
challenge_model_name <- 'glm_aed_flare_v3'

# Dates of forecasts
today <- paste(Sys.Date() - days(2), '00:00:00')
# this_year <- data.frame(date = as.character(paste0(seq.Date(as_date('2025-01-01'), to = as_date(today), by = 'day'), ' 00:00:00')),
#                         exists = NA)

missing_period <- data.frame(date = as.Date(seq.Date(as_date('2024-09-25'), to = as_date(today), by = 'day')))

# check flare forecast dates from VERA
s3 <- arrow::s3_bucket(bucket = glue::glue("bio230121-bucket01/vera4cast/forecasts/archive-parquet/project_id=vera4cast/duration=P1D/variable=Temp_C_mean/model_id=glm_aed_flare_v3"),
                       endpoint_override = "https://amnh1.osn.mghpcc.org",
                       anonymous = TRUE)

avail_dates <- gsub("reference_date=", "", s3$ls())

missing_period$exists <- ifelse(missing_period$date %in% avail_dates, T, F)

rerun_dates <- missing_period |> filter(exists == FALSE) |> pull(date)


for (i in seq.int(1:length(rerun_dates))){

  print(i)

  new_date <- rerun_dates[i]

  file_name <- paste0('./rerun_submissions/glm_aed_flare_v3',
                      "-",as.character(new_date),".csv.gz")

  s3_rerun <- arrow::s3_bucket(bucket = glue::glue("bio230121-bucket01/flare/forecasts/parquet/site_id=fcre/model_id=glm_aed_flare_v3/reference_date=",as.character(new_date),'/'),
                                  endpoint_override = "https://amnh1.osn.mghpcc.org",
                                  anonymous = TRUE)

  rerun_df <- arrow::open_dataset(s3_rerun) |>
    collect() |>
    mutate(model_id = 'glm_aed_flare_v3',
           site_id = 'fcre',
           reference_date = as.Date(reference_datetime)) |>
    rename(depth_m = depth)

  vera_variables <- c("Temp_C_mean","Chla_ugL_mean", "DO_mgL_mean", "fDOM_QSU_mean", "NH4_ugL_sample",
                      "NO3NO2_ugL_sample", "SRP_ugL_sample", "DIC_mgL_sample","Secchi_m_sample",
                      "Bloom_binary_mean","CH4_umolL_sample","IceCover_binary_max", "CO2flux_umolm2s_mean", "CH4flux_umolm2s_mean",
                      "Mixed_binary_mean")

  # Calculate the probability of bloom
  bloom_binary <- rerun_df |>
    dplyr::filter(depth_m == 1.6 & variable == "Chla_ugL_mean") |>
    dplyr::mutate(over = ifelse(prediction > 20, 1, 0)) |>
    dplyr::summarize(prediction = sum(over) / n(), .by = c(datetime, reference_datetime, model_id, site_id, depth_m, variable)) |> #pubDate
    dplyr::mutate(family = "bernoulli",
                  parameter = "prob",
                  variable = "Bloom_binary_mean",
                  datetime = lubridate::as_datetime(datetime)) |>
    dplyr::select(reference_datetime, datetime, model_id, site_id, depth_m, family, parameter, variable, prediction)

  # Calculate probability of having ice
  ice_binary <- rerun_df |>
    dplyr::filter(variable == "ice_thickness") |>
    dplyr::mutate(over = ifelse(prediction > 0, 1, 0)) |>
    dplyr::summarize(prediction = sum(over) / n(), .by = c(datetime, reference_datetime, model_id, site_id, depth_m, variable)) |> #pubDate
    dplyr::mutate(family = "bernoulli",
                  parameter = "prob",
                  variable = "IceCover_binary_max",
                  depth_m = NA,
                  datetime = lubridate::as_datetime(datetime)) |>
    dplyr::select(reference_datetime, datetime, model_id, site_id, depth_m, family, parameter, variable, prediction)

  # Calculate probablity of being mixed
  min_depth <- 1
  max_depth <- 8
  threshold <- 0.1

  temp_forecast <- rerun_df |>
    filter(variable %in% c("temp_1.0m_mean","temp_8.0m_mean")) |>
    mutate(depth_m = ifelse(variable == "temp_1.0m_mean", 1.0, 8.0),
           variable = "Temp_C_mean",
           datetime = lubridate::as_datetime(datetime - lubridate::days(1))) |>
    pivot_wider(names_from = depth_m, names_prefix = 'wtr_', values_from = prediction)

  colnames(temp_forecast)[which(colnames(temp_forecast) == paste0('wtr_', min_depth))] <- 'min_depth'
  colnames(temp_forecast)[which(colnames(temp_forecast) == paste0('wtr_', max_depth))] <- 'max_depth'

  mix_binary <- temp_forecast |>
    mutate(min_depth = rLakeAnalyzer::water.density(min_depth),
           max_depth = rLakeAnalyzer::water.density(max_depth),
           mixed = ifelse((max_depth - min_depth) < threshold, 1, 0)) |>
    summarise(prediction = (sum(mixed)/n()), .by = c(datetime, reference_datetime, model_id, site_id, variable)) |> #pubDate
    dplyr::mutate(family = "bernoulli",
                  parameter = "prob",
                  variable = "Mixed_binary_mean",
                  depth_m = NA,
                  datetime = lubridate::as_datetime(datetime)) |>
    dplyr::select(reference_datetime, datetime, model_id, site_id, depth_m, family, parameter, variable, prediction)


  # Combine into a vera data frame
  vera4cast_df <- rerun_df |>
    dplyr::mutate(#variable = ifelse(variable == "DO_mgL_mean", "DO_mgL_mean_all_depth", variable),
      variable = ifelse(variable == "oxy_mean", "DO_mgL_mean", variable),
      depth_m = ifelse(variable == "DO_mgL_mean", 1.6, depth_m),
      datetime = ifelse(variable == "DO_mgL_mean", datetime - lubridate::days(1), datetime),
      prediction = ifelse(variable == "DO_mgL_mean", prediction/1000*(32),prediction),
      variable = ifelse(variable == "Temp_C_mean", "Temp_C_mean_all_depth", variable),
      variable = ifelse(variable == "temp_1.6m_mean", "Temp_C_mean", variable),
      depth_m = ifelse(variable == "Temp_C_mean", 1.6, depth_m),
      datetime = ifelse(variable == "Temp_C_mean", datetime - lubridate::days(1), datetime),
      prediction = ifelse(variable == "fDOM_QSU_mean", (151.3407 + prediction)/29.62654,prediction),
      prediction = ifelse(variable == "NIT_amm", prediction/1000/0.001/(1/18.04),prediction),
      variable = ifelse(variable == "NIT_amm", "NH4_ugL_sample", variable),
      prediction = ifelse(variable == "NIT_nit", prediction/1000/0.001/(1/62.00),prediction),
      variable = ifelse(variable == "NIT_amm", "NO3NO2_ugL_sample", variable),
      prediction = ifelse(variable == "PHS_frp", prediction/1000/0.001/(1/94.9714),prediction),
      variable = ifelse(variable == "PHS_frp", "SRP_ugL_sample", variable),
      prediction = ifelse(variable == "CAR_dic", prediction/1000/(1/52.515), prediction),
      variable = ifelse(variable == "CAR_dic", "DIC_mgL_sample", variable),
      variable = ifelse(variable == "CAR_ch4", "CH4_umolL_sample", variable),
      variable = ifelse(variable == "secchi", "Secchi_m_sample", variable),
      prediction = ifelse(variable == "co2_flux_mean", prediction/0.001/ 86400 , prediction),
      variable = ifelse(variable == "co2_flux_mean", "CO2flux_umolm2s_mean", variable),
      prediction = ifelse(variable == "ch4_flux_mean", prediction/0.001/86400 , prediction),
      variable = ifelse(variable == "ch4_flux_mean", "CH4flux_umolm2s_mean", variable),
      depth_m = ifelse(depth_m == 0.0, 0.1, depth_m),
      datetime = lubridate::as_datetime(datetime)) |>
    dplyr::select(-forecast, -variable_type) |> #pubDate
    dplyr::mutate(parameter = as.character(parameter)) |>
    dplyr::bind_rows(bloom_binary) |>
    dplyr::bind_rows(ice_binary) |>
    dplyr::bind_rows(mix_binary) |>
    dplyr::filter(variable %in% vera_variables) |>
    mutate(project_id = "vera4cast",
           model_id = 'glm_aed_flare_v3',
           family = "ensemble",
           site_id = "fcre",
           duration = "P1D",
           datetime = lubridate::as_datetime(datetime),
           reference_datetime = lubridate::as_datetime(reference_datetime)) |>
    filter(datetime >= reference_datetime) |>
    distinct(reference_datetime, datetime, variable, depth_m, parameter, model_id,.keep_all = TRUE)


  write.csv(vera4cast_df, file_name)

  vera4castHelpers::submit(file_name, first_submission = FALSE)

}
#
# file_name <- paste0('glm_aed_flare_v3',
#                     "-",
#                     lubridate::as_date(vera4cast_df$reference_datetime[1]),".csv.gz")
#
# readr::write_csv(vera4cast_df, file = file_name)
#
# vera4castHelpers::submit(file_name, first_submission = FALSE)
