print(paste0("Running Creating baselines at ", Sys.time()))

library(tidyverse)
library(lubridate)
library(aws.s3)
library(imputeTS)
library(tsibble)
library(fable)

source('R/ClimatologyModelFunction.R')
source('R/convert2binary.R')

#' set the random number for reproducible MCMC runs
set.seed(329)

config <- yaml::read_yaml("challenge_configuration.yaml")
team_name <- "climatology"

# where are the relevant targets?
targets_insitu <- readr::read_csv(paste0("https://", config$endpoint, "/", config$targets_bucket, "/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"), guess_max = 10000)
targets_met <- readr::read_csv(paste0("https://", config$endpoint, "/", config$targets_bucket, "/project_id=vera4cast/duration=P1D/daily-met-targets.csv.gz"), guess_max = 10000, show_col_types = FALSE)
targets_tubr <- readr::read_csv(paste0("https://", config$endpoint, "/", config$targets_bucket, "/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz"), guess_max = 10000, show_col_types = FALSE)

# Get site information
sites <- readr::read_csv(config$catalog_config$site_metadata_url, show_col_types = FALSE)
site_names <- sites$site_id

# Inflow variables
climatology_inflow <- purrr::map_dfr(.x = c('Flow_cms_mean', 'Temp_C_mean'),
                                     .f = ~ generate_baseline_climatology(targets = targets_tubr,
                                                                          h = 35,
                                                                          forecast_date = Sys.Date(),
                                                                          site = 'tubr', depth = 'target', var = .x))
# Met variables
climatology_met <- generate_baseline_climatology(targets = targets_met,
                                               h = 35,
                                               site = 'fcre',
                                               var = 'AirTemp_C_mean',
                                               depth = 'target',
                                               forecast_date = Sys.Date())

# Insitu variables
# get all combinations
site_var_combinations_focal <- expand.grid(var = c('DO_mgL_mean',
                                             'DOsat_percent_mean',
                                             'Chla_ugL_mean',
                                             'Secchi_m_sample',
                                             'Temp_C_mean',
                                             'fDOM_QSU_mean',
                                             'SpCond_uScm_mean',
                                             'Turbidity_FNU_mean'),
                                             #'CH4_umolL_sample',
                                             #'CO2_umolL_sample'),
                                     site = c('fcre',
                                              'bvre'))

climatology_insitu_focal <- purrr::pmap_dfr(site_var_combinations_focal,
                                      .f = ~ generate_baseline_climatology(targets = targets_insitu,
                                                                         h = 35,
                                                                         forecast_date = Sys.Date(),
                                                                         depth = 'target',
                                                                         ...))

## GHG VARIABLES (TAKEN FROM DIFFERENT DEPTH)
site_var_combinations_ghg_insitu <- expand.grid(var = c('CH4_umolL_sample',
                                                        'CO2_umolL_sample'),
                                                site = c('fcre',
                                                         'bvre'))

climatology_ghg_insitu <- purrr::pmap_dfr(site_var_combinations_ghg_insitu,
                                            .f = ~ generate_baseline_climatology(targets = targets_insitu,
                                                                                   h = 35,
                                                                                   model_id = team_name,
                                                                                   forecast_date = Sys.Date(),
                                                                                   depth = c(0.1),
                                                                                   ...))
## Productivity variables
site_var_combinations_productivity <- expand.grid(var = c(#'DeepChlorophyllMaximum_binary',
                                                   'TotalConc_ugL_sample',
                                                   'GreenAlgae_ugL_sample',
                                                   'Bluegreens_ugL_sample',
                                                   'BrownAlgae_ugL_sample',
                                                   'MixedAlgae_ugL_sample'),
                                                   # 'TotalConcCM_ugL_sample',
                                                   # 'GreenAlgaeCM_ugL_sample',
                                                   # 'BluegreensCM_ugL_sample',
                                                   # 'BrownAlgaeCM_ugL_sample',
                                                   # 'MixedAlgaeCM_ugL_sample',
                                                   # 'ChlorophyllMaximum_depth_sample',
                                                   # 'MOM_binary_sample',
                                                   # 'MOM_min_sample',
                                                   # 'MOM_max_sample'),
                                           site = c('fcre',
                                                    'bvre'))

climatology_insitu_productivity <- purrr::pmap_dfr(site_var_combinations_productivity,
                                            .f = ~ generate_baseline_climatology(targets = targets_insitu,
                                                                                 h = 35,
                                                                                 forecast_date = Sys.Date(),
                                                                                 depth = 'target',
                                                                                 ...))

## CHLA maxiumum variables
cmax_vars <- c('DeepChlorophyllMaximum_binary_sample',
               'TotalConcCM_ugL_sample',
               'GreenAlgaeCM_ugL_sample',
               'BluegreensCM_ugL_sample',
               'BrownAlgaeCM_ugL_sample',
               'MixedAlgaeCM_ugL_sample',
               'ChlorophyllMaximum_depth_sample',
               'MOM_binary_sample',
               'MOM_min_sample',
               'MOM_max_sample')

targets_cmax <- targets_insitu |> dplyr::filter(variable %in% cmax_vars) |>
  mutate(depth_m = NA)

site_var_combinations_chla_max <- expand.grid(var = cmax_vars,
                                                  site = c('fcre',
                                                           'bvre'))

climatology_insitu_chla_max <- purrr::pmap_dfr(site_var_combinations_chla_max,
                                                   .f = ~ generate_baseline_climatology(targets = targets_cmax,
                                                                                        h = 35,
                                                                                        forecast_date = Sys.Date(),
                                                                                        depth = 'target',
                                                                                        ...))
## CHEM variables
site_var_combinations_chem <- expand.grid(var = c('TN_ugL_sample',
                                                          'TP_ugL_sample',
                                                          'SRP_ugL_sample',
                                                          'NO3NO2_ugL_sample',
                                                          'NH4_ugL_sample',
                                                          'DOC_mgL_sample',
                                                  'DRSI_mgL_sample',
                                                  #'DIC_mgL_sample',
                                                  'DC_mgL_sample',
                                                  'DN_mgL_sample'),
                                                  site = c('fcre',
                                                           'bvre'))

targets_insitu <- targets_insitu |>
  mutate(depth_m = ifelse(variable %in% c('TN_ugL_sample',
                                          'TP_ugL_sample',
                                          'SRP_ugL_sample',
                                          'NO3NO2_ugL_sample',
                                          'NH4_ugL_sample',
                                          'DOC_mgL_sample',
                                          'DC_mgL_sample',
                                          'DN_mgL_sample',
                                          'DIC_mgL_sample') & site_id == 'bvre',
                          1.5,
                          depth_m))

targets_insitu <- targets_insitu |>
  mutate(depth_m = ifelse(variable == 'DRSI_mgL_sample' & depth_m %in% c(0.1, 4, 5),
                          1.5,
                          depth_m))

climatology_insitu_chem <- purrr::pmap_dfr(site_var_combinations_chem,
                                                   .f = ~ generate_baseline_climatology(targets = targets_insitu,
                                                                                        h = 35,
                                                                                        forecast_date = Sys.Date(),
                                                                                        depth = 'target',
                                                                                        ...))

## Physical variables
site_var_combinations_physical <- expand.grid(var = c('ThermoclineDepth_m_mean',
                                                  'SchmidtStability_Jm2_mean'),
                                          site = c('fcre',
                                                   'bvre'))

climatology_insitu_physical <- purrr::pmap_dfr(site_var_combinations_physical,
                                           .f = ~ generate_baseline_climatology(targets = targets_insitu,
                                                                                h = 35,
                                                                                forecast_date = Sys.Date(),
                                                                                depth = 'target',
                                                                                ...))

# ## Generate Metals
print('Metals model')

site_var_combinations_metals <- expand.grid(var = c('TFe_mgL_sample',
                                                    'SFe_mgL_sample',
                                                    'TMn_mgL_sample',
                                                    'SMn_mgL_sample',
                                                    ''),
                                            site = c('fcre',
                                                     'bvre'))

climatology_insitu_metals <- purrr::pmap_dfr(site_var_combinations_metals,
                                             .f = ~ generate_baseline_climatology(targets = targets_insitu,
                                                                                  h = 35,
                                                                                  forecast_date = Sys.Date(),
                                                                                  depth = 'target',
                                                                                  ...))

# Flux variables
# get all combinations
print('Flux model')

climatology_flux <- purrr::map_dfr(.x = c('CO2flux_umolm2s_mean', 'CH4flux_umolm2s_mean'),
               .f = ~ generate_baseline_climatology(targets = targets_insitu,
                                                    h = 35,
                                                    forecast_date = Sys.Date(),
                                                    site = 'fcre', depth = 'target', var = .x))

# Generate binary forecasts from continuous
binary_site_var_comb <- data.frame(site = c('fcre', 'bvre'),
                                   depth = c(1.6, 1.5))

climatology_insitu_binary <- purrr::pmap_dfr(binary_site_var_comb,
                                             .f = ~convert_continuous_binary(continuous_var = 'Chla_ugL_mean',
                                                                             binary_var = 'Bloom_binary_mean',
                                                                             forecast = climatology_insitu_focal,
                                                                             targets = targets_insitu,
                                                                             threshold = 20,
                                                                             ...))


# combine and submit
combined_climatology <- bind_rows(climatology_met, climatology_inflow, climatology_insitu_focal, climatology_insitu_binary, climatology_flux,
                                  climatology_ghg_insitu, climatology_insitu_productivity, climatology_insitu_chem, climatology_insitu_physical, climatology_insitu_metals,
                                  climatology_insitu_chla_max)

# 4. Write forecast file
file_date <- combined_climatology$reference_datetime[1]

forecast_file <- paste0(paste("daily", file_date, team_name, sep = "-"), ".csv.gz")

write_csv(combined_climatology, forecast_file)

# combined_climatology %>%
#   filter(family == 'normal') |>
#   pivot_wider(names_from = parameter, values_from = prediction) |>
#   ggplot(aes(x = datetime, y = mu, fill = as_factor(depth_m))) +
#   geom_line() +
#   geom_ribbon(aes(ymax = mu+sigma, ymin = mu-sigma), alpha = 0.3, fill = 'blue') +
#   facet_grid(variable~site_id, scales = 'free')

# combined_climatology %>%
#   filter(family == 'bernoulli') |>
#   ggplot(aes(x = datetime, y = prediction, colour = as_factor(depth_m))) +
#   geom_line() +
#   facet_grid(variable~site_id, scales = 'free')

vera4castHelpers::submit(forecast_file = forecast_file,
                         ask = FALSE,
                         first_submission = FALSE)

unlink(forecast_file)
