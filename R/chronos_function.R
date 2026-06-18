
# Requires the Python package 'chronos-forecasting' to be installed in the
# active reticulate environment:
#   reticulate::py_install("chronos-forecasting", pip = TRUE)
#
# Chronos-Bolt (Chronos2) model card: https://huggingface.co/amazon/chronos-bolt-small
# Available model sizes: chronos-bolt-tiny, -mini, -small, -base, -large

generate_chronos_forecast <- function(targets,
                                      site,
                                      var,
                                      forecast_date = Sys.Date(),
                                      h,
                                      depth = 'target',
                                      model_name = 'vera_chronos',
                                      chronos_model = "amazon/chronos-bolt-small",
                                      ...) {

  if (depth == 'target') {
    target_depths <- c(1.5, 1.6, NA)
  } else {
    target_depths <- depth
  }

  target_ts <- targets |>
    filter(site_id == site,
           variable == var,
           depth_m %in% target_depths,
           datetime < forecast_date) |>
    drop_na(observation) |>
    arrange(datetime)

  if (nrow(target_ts) == 0) {
    message('No targets available for ', var, ' at ', site)
    return(NULL)
  }

  model_depth <- unique(target_ts$depth_m)

  # Create a regular daily time series and interpolate gaps
  full_dates <- seq(min(target_ts$datetime), max(target_ts$datetime), by = "1 day")
  ts_values <- tibble(datetime = full_dates) |>
    left_join(target_ts |> select(datetime, observation), by = "datetime") |>
    mutate(observation = imputeTS::na_interpolation(observation)) |>
    pull(observation)

  message('Running Chronos forecast for ', var, ' at ', site,
          ' using model: ', chronos_model)

  # --- Python setup via reticulate ---
  torch   <- reticulate::import("torch")
  chronos <- reticulate::import("chronos")

  # Load the pretrained Chronos-Bolt pipeline (downloads on first call, then cached)
  pipeline <- chronos$BaseChronosPipeline$from_pretrained(
    chronos_model,
    device_map = "cpu",
    torch_dtype = torch$float32
  )

  # Convert the R vector to a 1-D torch tensor (shape: [context_length])
  context <- torch$tensor(ts_values, dtype = torch$float32)

  # ChronosBoltPipeline.predict() takes context and prediction_length positionally.
  # It returns a SINGLE tensor of shape [batch, num_quantiles, prediction_length]
  # with 9 fixed quantile levels: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
  raw_forecast <- pipeline$predict(
    context,
    as.integer(h)
  )

  # Convert single tensor to R array of shape [1, 9, h]
  quant_arr <- reticulate::py_to_r(raw_forecast$numpy())

  # batch = 1, so first index is always 1 in R
  # quantile index 5 = 0.5 (median) used as mu
  # quantile index 1 = 0.1 (q10), index 9 = 0.9 (q90) used for sigma
  mu_vec    <- as.vector(quant_arr[1, 5, ])
  q10_vec   <- as.vector(quant_arr[1, 1, ])
  q90_vec   <- as.vector(quant_arr[1, 9, ])
  sigma_vec <- (q90_vec - q10_vec) / (2 * qnorm(0.9))

  # Build forecast dates and format output to vera4cast standard
  forecast_dates <- seq(forecast_date, by = "1 day", length.out = h)

  chronos_forecast <- tibble(
    datetime = forecast_dates,
    mu       = mu_vec,
    sigma    = sigma_vec
  ) |>
    pivot_longer(c("mu", "sigma"), names_to = "parameter", values_to = "prediction") |>
    mutate(
      model_id           = model_name,
      reference_datetime = forecast_date,
      site_id            = site,
      variable           = var,
      family             = "normal",
      depth_m            = model_depth,
      project_id         = "vera4cast",
      duration           = "P1D"
    ) |>
    select(model_id, datetime, reference_datetime, site_id, variable, family,
           parameter, prediction, depth_m, project_id, duration)

  return(chronos_forecast)
}
