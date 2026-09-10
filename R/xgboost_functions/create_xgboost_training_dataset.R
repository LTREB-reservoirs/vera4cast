config <- yaml::read_yaml("challenge_configuration.yaml")

# Read in targets
targets_insitu <- readr::read_csv(paste0("https://", config$endpoint, "/", config$targets_bucket, "/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"), guess_max = 10000)
targets_met <- readr::read_csv(paste0("https://", config$endpoint, "/", config$targets_bucket, "/project_id=vera4cast/duration=P1D/daily-met-targets.csv.gz"), guess_max = 10000, show_col_types = FALSE)
targets_tubr <- readr::read_csv(paste0("https://", config$endpoint, "/", config$targets_bucket, "/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz"), guess_max = 10000, show_col_types = FALSE)

tubr_targets_wide <- targets_tubr |>
  select(-duration, -project_id) |>
  pivot_wider(names_from = 'variable', values_from = 'observation')

tubr_targets_long <- tubr_targets_wide |>
  rename_with(~ paste0(., "_inflow"), names(tubr_targets_wide)[4:17]) |>
  pivot_longer(c(-site_id, -datetime, -depth_m), names_to = 'variable', values_to = 'observation') |>
  mutate(site_id = 'fcre')


## run through xgboost analysis on FCR first (can use insitu and met for BVR but not tubr inflows (no inflow data for BVR)))
fcr_all_targets_wide <- bind_rows(targets_insitu, targets_met, tubr_targets_long) |>
  mutate(datetime = lubridate::as_datetime(as.Date(datetime))) |>
  filter(site_id == 'fcre') |>
  mutate(depth_m = ifelse(variable %in% c('Bluegreens_ugL_sample',
                                          'GreenAlgae_ugL_sample',
                                          'BrownAlgae_ugL_sample',
                                          'MixedAlgae_ugL_sample',
                                          'TotalConc_ugL_sample'),
                          1.6, depth_m)) |>
  select(-duration, -project_id) |>
  drop_na(observation) |>
  distinct(datetime, variable, depth_m, site_id, .keep_all = T) |>
  pivot_wider(names_from = 'variable', values_from = 'observation') |>
  bind_rows(tubr_targets_long) |>
  mutate(doy = lubridate::yday(datetime),
         sin_doy = sin(2 * pi * doy / 365),
         cos_doy = cos(2 * pi * doy / 365)) |>
  select(-doy, -observation, -variable)

write.csv(fcr_all_targets_wide, './R/xgboost_functions/fcr_xgboost_training_dataset.csv', row.names = FALSE)
