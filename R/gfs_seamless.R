
download_ensemble_forecast <- function(site){

  s3 <- arrow::s3_bucket("bio230121-bucket01/flare",
                         endpoint_override = "amnh1.osn.mghpcc.org",
                         access_key = Sys.getenv("OSN_KEY"),
                         secret_key = Sys.getenv("OSN_SECRET"))

  s3$CreateDir("drivers/met/ensemble_forecast")

  s3 <- arrow::s3_bucket("bio230121-bucket01/flare/drivers/met/ensemble_forecast",
                         endpoint_override = "amnh1.osn.mghpcc.org",
                         access_key = Sys.getenv("OSN_KEY"),
                         secret_key = Sys.getenv("OSN_SECRET"))

  site_list <- readr::read_csv("https://raw.githubusercontent.com/FLARE-forecast/aws_noaa/master/site_list_v2.csv", show_col_types = FALSE)

  for(i in 1:nrow(site_list)){

    RopenMeteo::get_ensemble_forecast(
      latitude = site_list$latitude[i],
      longitude = site_list$longitude[i],
      site_id = site_list$site_id[i],
      forecast_days = 35,
      past_days = 0,
      model = "gfs05",
      variables = RopenMeteo::glm_variables(product = "ensemble_forecast",
                                            time_step = "hourly")) |>
      RopenMeteo::add_longwave() |>
      RopenMeteo::convert_to_efi_standard() |>
      dplyr::mutate(reference_date = lubridate::as_date(reference_datetime)) |>
      arrow::write_dataset(s3, format = 'parquet',
                           partitioning = c("model_id", "reference_date", "site_id"))
  }
}
