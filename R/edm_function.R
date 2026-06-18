
generate_edm_forecast <- function(targets,
                                  site,
                                  var,
                                  forecast_date = Sys.Date(),
                                  h,
                                  depth = 'target',
                                  model_name = 'vera_edm',
                                  ...) {

  if (depth == 'target') {
    # only generates forecasts for target depths
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

  # Create a regular daily time series and interpolate gaps (rEDM requires uniform spacing)
  full_dates <- seq(min(target_ts$datetime), max(target_ts$datetime), by = "1 day")
  ts_regular <- tibble(datetime = full_dates) |>
    left_join(target_ts |> select(datetime, observation), by = "datetime") |>
    mutate(observation = imputeTS::na_interpolation(observation),
           t = row_number())

  edm_df <- ts_regular |>
    select(t, y = observation)

  n <- nrow(edm_df)

  # Find optimal embedding dimension E via leave-one-out cross-validation
  embed_out <- rEDM::EmbedDimension(
    dataFrame = edm_df,
    lib = c(1, n),
    pred = c(1, n),
    target = "y",
    columns = "y",
    maxE = 10,
    showPlot = FALSE
  )
  best_E <- embed_out$E[which.max(embed_out$rho)]

  message('EDM forecast for ', var, ' at ', site, ' using E = ', best_E)

  # Estimate sigma from in-sample one-step-ahead leave-one-out predictions
  insample <- rEDM::Simplex(
    dataFrame = edm_df,
    lib = c(1, n),
    pred = c(1, n),
    target = "y",
    columns = "y",
    E = best_E,
    Tp = 1
  )
  sigma_base <- sqrt(mean((insample$Predictions - insample$Observations)^2, na.rm = TRUE))

  # Generate multi-step forecasts iteratively using Tp=1 at each step.
  # rEDM requires the prediction target row (pred + Tp) to have a non-NA observation,
  # so we grow the dataframe step-by-step, appending each predicted value as the next row.
  current_df  <- edm_df
  forecast_mu <- numeric(h)

  for (step in seq_len(h)) {
    n_curr <- nrow(current_df)
    # Append one non-NA placeholder row so the Tp=1 target row exists
    ext_df <- bind_rows(current_df, tibble(t = n_curr + 1L, y = tail(current_df$y, 1)))
    # rEDM adjusts pred[1] internally by +(E-1) for embedding validation, so
    # pred = c(n, n) fails when E > 1. Widen pred to c(n-E+1, n) so the
    # effective range resolves to exactly row n; take only the last prediction.
    pred_start <- max(1L, n_curr - best_E + 1L)
    out <- rEDM::Simplex(
      dataFrame = ext_df,
      lib       = c(1, n_curr),
      pred      = c(pred_start, n_curr),
      target    = "y",
      columns   = "y",
      E         = best_E,
      Tp        = 1,
      showPlot  = FALSE
    )
    next_val          <- out$Predictions[nrow(out)]
    forecast_mu[step] <- next_val
    # Append predicted value so subsequent embedding steps have valid context
    current_df <- bind_rows(current_df, tibble(t = n_curr + 1L, y = next_val))
  }

  forecast_preds <- tibble(Tp = seq_len(h), mu = forecast_mu)

  # Build forecast dates and format output to vera4cast standard
  forecast_dates <- seq(forecast_date, by = "1 day", length.out = h)

  edm_forecast <- forecast_preds |>
    mutate(
      datetime = forecast_dates,
      sigma = sigma_base * sqrt(Tp)  # uncertainty grows with forecast horizon
    ) |>
    pivot_longer(c("mu", "sigma"), names_to = "parameter", values_to = "prediction") |>
    mutate(
      model_id = model_name,
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

  return(edm_forecast)
}
