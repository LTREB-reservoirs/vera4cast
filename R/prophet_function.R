generate_prophet_forecast <- function(targets,
                                      site,
                                      var,
                                      forecast_date = Sys.Date(),
                                      h,
                                      depth = 'target',
                                      model_name = 'vera_prophet',
                                      ...) {

  if (depth == 'target') {
    # only generates forecasts for target depths
    target_depths <- c(1.5, 1.6, NA)
  } else {
    target_depths <- depth
  }

  # Prepare data for prophet (requires columns 'ds' for date and 'y' for observation)
  target_ts <- targets |>
    filter(site_id == site,
           variable == var,
           depth_m %in% target_depths,
           datetime < forecast_date) |>
    drop_na(observation)

  prophet_df <- target_ts |>
    select(ds = datetime, y = observation)


  # Fit the prophet model
  m <- prophet::prophet(prophet_df, daily.seasonality = "auto")

  # Create a 35-day forecast horizon
  future <- make_future_dataframe(m, periods = h)
  forecast_raw <- predict(m, future)

  # Format output to vera4cast standard
  model_depth <- unique(target_ts$depth_m)

  prophet_forecast <- forecast_raw |>
    filter(as_date(ds) >= forecast_date) |>
    mutate(
      mu = yhat,
      sigma = (yhat_upper - yhat_lower) / (2 * qnorm(0.9))  # convert 80% CI to SD
    ) |>
    pivot_longer(c("mu", "sigma"), names_to = "parameter", values_to = "prediction") |>
    mutate(
      model_id = model_name,
      datetime = as_date(ds),
      reference_datetime = forecast_date,
      site_id = site,
      variable = var,
      family = "normal",
      depth_m = model_depth,
      project_id = "vera4cast",
      duration = "P1D"
    ) |>
    select(model_id, datetime, reference_datetime, site_id, variable, family,
           parameter, prediction, depth_m, project_id, duration)

}
