library(RCurl)
library(curl)

## set EDI API Key
edi_access_key = Sys.getenv('EDI_ACCESS_KEY')

## set destination s3 paths
s3 <- arrow::s3_bucket("bio230121-bucket01", endpoint_override = "amnh1.osn.mghpcc.org")
s3$CreateDir("vera4cast/targets_v2/duration=P1D")
s3$CreateDir("vera4cast/targets_v2/duration=PT1H")

duckdbfs::duckdb_secrets(
  endpoint = 'amnh1.osn.mghpcc.org',
  key = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"))

s3_daily <- arrow::s3_bucket("bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D", endpoint_override = "amnh1.osn.mghpcc.org")
s3_hourly <- arrow::s3_bucket("bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=PT1H", endpoint_override = "amnh1.osn.mghpcc.org")

column_names <- c("project_id", "site_id","datetime","duration", "depth_m","variable","observation")


## water level
source('targets/target_functions_v2/target_generation_waterlevel_daily.R')

fcr_wl_files <- c(paste0("https://pasta.lternet.edu/package/data/eml/edi/271/10/814580ebec0385c66f0a0a97c38e9136?key=",edi_access_key),
                  "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")

bvr_wl_files <- c(paste0("https://pasta.lternet.edu/package/data/eml/edi/725/6/37e2587d2ab477068b295f97f1598cf9?key=",edi_access_key),
                  "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv")

water_level_df <- target_generation_waterlevel_daily(fcr_files = fcr_wl_files,
                                                     bvr_files = bvr_wl_files)

water_level_df$datetime <- lubridate::as_datetime(water_level_df$datetime)


## EXO
print('EXO')
source('targets/target_functions_v2/target_generation_exo_daily.R')
fcr_files <- c(paste0("https://pasta.lternet.edu/package/data/eml/edi/271/10/814580ebec0385c66f0a0a97c38e9136?key=",edi_access_key),
               "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")

bvr_files <- c("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv",
               paste0("https://pasta.lternet.edu/package/data/eml/edi/725/6/37e2587d2ab477068b295f97f1598cf9?key=",edi_access_key))

exo_daily <- target_generation_exo_daily(fcr_files, bvr_files, waterlevel_file = water_level_df)

exo_daily$duration <- 'P1D'
exo_daily$project_id <- 'vera4cast'


### NOTE : RDO DO DATA IS INCLUDED IN THE EXO TARGET GENERATION SCRIPT


## FLUOROPROBE
print('Fluoroprobe')
source('targets/target_functions_v2/target_generation_FluoroProbe.R')
historic_data <- paste0("https://pasta.lternet.edu/package/data/eml/edi/272/10/6d7576cc758ca378fe004ad0ac9eed85?key=",edi_access_key)
current_data <- "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/refs/heads/master/Data/DataNotYetUploadedToEDI/FluoroProbe/fluoroprobe_L1.csv"

fluoro_daily <- target_generation_FluoroProbe(current_file = current_data, historic_file = historic_data)
fluoro_daily$duration <- 'P1D'
fluoro_daily$project_id <- 'vera4cast'


### TEMP STRING
source('targets/target_functions_v2/target_generation_ThermistorTemp_C_daily.R')

#
print('FCR Thermistor')
fcr_latest <- "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv"
fcr_edi <- paste0("https://pasta.lternet.edu/package/data/eml/edi/271/10/814580ebec0385c66f0a0a97c38e9136?key=",edi_access_key)

fcr_thermistor_temp_daily <- target_generation_ThermistorTemp_C_daily(current_file = fcr_latest, historic_file = fcr_edi, waterlevel_file = water_level_df)
fcr_thermistor_temp_daily$duration <- 'P1D'
fcr_thermistor_temp_daily$project_id <- 'vera4cast'

# BVR
print('BVR Thermistor')
bvr_latest <- "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv"
bvr_edi <- paste0("https://pasta.lternet.edu/package/data/eml/edi/725/6/37e2587d2ab477068b295f97f1598cf9?key=",edi_access_key)

bvr_thermistor_temp_daily <- target_generation_ThermistorTemp_C_daily(current_file = bvr_latest, historic_file = bvr_edi, waterlevel_file = water_level_df)
bvr_thermistor_temp_daily$duration <- 'P1D'
bvr_thermistor_temp_daily$project_id <- 'vera4cast'
bvr_thermistor_temp_daily$site_id <- 'bvre'


#Secchi
print('Secchi')
source('targets/target_functions_v2/target_generation_daily_secchi_m.R')
current = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Secchi/secchi_L1.csv"
edi = paste0("https://pasta.lternet.edu/package/data/eml/edi/198/14/547f9a388a26711b4b10e4c7ad7e1a4e?key=",edi_access_key)

secchi_daily <- target_generation_daily_secchi_m(current = current, edi = edi) |>
  filter(site_id %in% c('fcre', 'bvre'))

secchi_daily$duration <- 'P1D'
secchi_daily$project_id <- 'vera4cast'



##Eddy Flux
print( 'Eddy Flux')
source('targets/target_functions_v2/generate_EddyFlux_ghg_targets_function.R')
eddy_flux <- generate_EddyFlux_ghg_targets_function(
  flux_current_data_file = "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-eddyflux-data-qaqc/EddyFlux_streaming_L1.csv",
  flux_edi_data_file = paste0("https://pasta.lternet.edu/package/data/eml/edi/1061/5/9686456c091a205d2444fb01342eeedc?key=",edi_access_key),
  met_current_data_file = "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCRmet_L1.csv",
  met_edi_data_file = paste0("https://pasta.lternet.edu/package/data/eml/edi/389/10/d3f3d2fa40c41fdcd505ae49b2fdcf8b?key=",edi_access_key))

eddy_flux$datetime <- lubridate::as_datetime(eddy_flux$datetime)


## CHEM
print('Chemistry')
source('targets/target_functions_v2/target_generation_chemistry_daily.R')
chem_data <- target_generation_chemistry_daily(current_data_file = NULL,
                                               historic_data_file = paste0('https://pasta.lternet.edu/package/data/eml/edi/199/13/3f09a3d23b7b5dd32ed7d28e9bc1b081?key=',edi_access_key),
                                               waterlevel_file = water_level_df)
chem_data$datetime <- lubridate::as_datetime(chem_data$datetime)

## METALS
print("Metals")
source('targets/target_functions_v2/target_generation_metals_daily.R')
metals_data <- target_generation_metals_daily(
current_data_file = 'https://raw.githubusercontent.com/CareyLabVT/Reservoirs/refs/heads/master/Data/DataNotYetUploadedToEDI/Metals_Data/metals_L1.csv',
edi_data_file = paste0("https://pasta.lternet.edu/package/data/eml/edi/455/9/9a072c4e4af39f96f60954fc4f7d8be5?key=",edi_access_key),
waterlevel_file = water_level_df)

metals_data$datetime <- lubridate::as_datetime(metals_data$datetime)

## SILICA
print('Silica')
source('targets/target_functions_v2/target_generation_silica_daily.R')
silica_data <- target_generation_silica_daily(current_data_file = NULL,
                                              historic_data_file = paste0("https://pasta.lternet.edu/package/data/eml/edi/542/1/791ec9ca0f1cb9361fa6a03fae8dfc95?key=",edi_access_key))
silica_data$datetime <- lubridate::as_datetime(silica_data$datetime)


## GHG
print('GHG')
source('targets/target_functions_v2/target_generation_ghg_daily.R')
ghg_data <- target_generation_ghg_daily(current_data_file = 'https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Raw_GHG/L1_manual_GHG.csv',
                                       edi_data_file = paste0('https://pasta.lternet.edu/package/data/eml/edi/551/10/ee8e65a2a380c9fb63bbf7f4a542c895?key=',edi_access_key),
                                       waterlevel_file = water_level_df)
ghg_data$datetime <- lubridate::as_datetime(ghg_data$datetime)


## CTD  - MOM
print('CTD - MOM')
source('targets/target_functions_v2/target_generation_daily_MOM.R')
historic_file  <- paste0("https://pasta.lternet.edu/package/data/eml/edi/200/16/d8de9befde67007072850897d5dd2e06?key=",edi_access_key)
current_file <-  "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Raw_CTD/ctd_L1.csv"

mom_daily_targets <- targets_generation_daily_MOM(current_file = current_file, historic_file = historic_file)

## Thermocline Depth
print('Thermocline Depth')
source('targets/target_functions_v2/generate_thermoclineD.R')
fcr_latest <- "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv"
fcr_edi <- paste0("https://pasta.lternet.edu/package/data/eml/edi/271/10/814580ebec0385c66f0a0a97c38e9136?key=",edi_access_key)

thermocline_depth_fcr <- generate_thermocline_depth(current_file = fcr_latest,
                                                    historic_file = fcr_edi)

bvr_latest <- "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv"
bvr_edi <- paste0("https://pasta.lternet.edu/package/data/eml/edi/725/6/37e2587d2ab477068b295f97f1598cf9?key=",edi_access_key)

thermocline_depth_bvr <- generate_thermocline_depth(current_file = bvr_latest,
                                                    historic_file = bvr_edi)

thermocline_depth <- bind_rows(thermocline_depth_fcr, thermocline_depth_bvr)

## Schmidt Stability
print('Schmidt Stability')
source('targets/target_functions_v2/target_generation_SchmidtStability.R')
fcr_files <- c(paste0("https://pasta.lternet.edu/package/data/eml/edi/271/10/814580ebec0385c66f0a0a97c38e9136?key=",edi_access_key),
               "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")

schmidt_stability_fcr <- generate_schmidt.stability(current_file = fcr_files[2], historic_file = fcr_files[1])

bvr_files <- c(paste0("https://pasta.lternet.edu/package/data/eml/edi/725/6/37e2587d2ab477068b295f97f1598cf9?key=",edi_access_key),
               "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv")

#schmidt_stability_bvr <- generate_schmidt.stability(current_file = bvr_files[2], historic_file = bvr_files[1])

#schmidt_stability <- bind_rows(schmidt_stability_fcr, schmidt_stability_bvr)
schmidt_stability <- schmidt_stability_fcr


## combine the data and perform final adjustments (depth, etc.)

# combined_targets <- bind_rows(exo_daily, fluoro_daily, fcr_thermistor_temp_daily, bvr_thermistor_temp_daily, secchi_daily,
#                               mom_daily_targets, thermocline_depth, schmidt_stability, eddy_flux, chem_data, metals_data,ghg_data, silica_data, water_level_df) #|>
#   #select(all_of(column_names))


## combined variables with position information here (non-NA values)
combined_targets_w_position <- bind_rows(exo_daily, fcr_thermistor_temp_daily, bvr_thermistor_temp_daily, fluoro_daily,
                                         chem_data, metals_data,ghg_data, silica_data) |>
  mutate(duration = 'P1D',
         project_id = 'vera4cast')


## assign depth positions ##
## add position assignment ##
water_level_match_df <- water_level_df |>
  select(datetime, site_id, observation) |>
  rename(waterlevel_m = observation)

targets_df_with_wl <- combined_targets_w_position |>
  left_join(water_level_match_df, by = c('datetime','site_id')) |>
  mutate(obs_z_ref = waterlevel_m - z,
         position_flag = NA)

## rows without a water level can't be flagged as top (height) or bottom (depth)
missing_wl <- targets_df_with_wl |>
  filter(is.na(waterlevel_m))

missing_wl_days <- missing_wl |>
  dplyr::distinct(site_id, datetime) |>
  dplyr::count(site_id, name = 'days_missing_water_level')

print('Days with targets but no water level (no shallowest/deepest assignment possible):')
print(missing_wl_days)

## for each date, extract the shallowest depth within 2m of the surface
# depth_m is measured from the surface so the shallowest sample is the smallest depth
# flagged depths without a sample within 2 m of the surface are left unflagged
targets_position_assign_df_height <- targets_df_with_wl |>
  filter(z_ref == 'height_m') |>
  drop_na(observation, z) |> ## don't end up picking the best position if there is no data (or no height)
  group_by(site_id, datetime, variable) |>
  mutate(
    position_flag = case_when( ## HEIGHT REFERENCE
      z <= 2 & z == min(z, na.rm = TRUE) ~ 'bottom_most_2m', # get min height value
      obs_z_ref <= 2 & z == max(z, na.rm = TRUE) ~ 'top_most_2m' # get max height value
    )
  ) |>
  ungroup() |>
  select(datetime, site_id, z, z_ref, observation, variable, duration, position_flag) # rearrange order of columns

## create new variables for "shallowest" and "deepest"
shallowest_height_vars_df <- targets_position_assign_df_height |>
  filter(position_flag == 'top_most_2m') |>
  mutate(variable = glue::glue('{variable}_shallowest'),
         z = NA) |>
  select(-position_flag)

deepest_height_vars_df <- targets_position_assign_df_height |>
  filter(position_flag == 'bottom_most_2m') |>
  mutate(variable = glue::glue('{variable}_deepest'),
         z = NA) |>
  select(-position_flag)

## DEPTH
## for each date, extract the shallowest depth within 2m of the surface
# depth_m is measured from the surface so the shallowest sample is the smallest depth
# flagged depths without a sample within 2 m of the surface are left unflagged
targets_position_assign_df_depth <- targets_df_with_wl |>
  filter(z_ref == 'depth_m') |>
  drop_na(observation, z) |> ## rows without a depth can't be shallowest/deepest
  group_by(site_id, datetime, variable) |>
  mutate(
    position_flag = case_when(
      z <= 2 & z == min(z, na.rm = TRUE) ~ 'top_most_2m',
      obs_z_ref <= 2 & z == max(z, na.rm = TRUE) ~ 'bottom_most_2m'
    )
  ) |>
  ungroup() |>
  select(datetime, site_id, z, z_ref, observation, variable, duration, position_flag) # rearrange order of columns

## create new variables for "shallowest" and "deepest"
shallowest_depth_vars_df <- targets_position_assign_df_depth |>
  filter(position_flag == 'top_most_2m') |>
  mutate(variable = glue::glue('{variable}_shallowest'),
         z = NA) |>
  select(-position_flag)

deepest_depth_vars_df <- targets_position_assign_df_depth |>
  filter(position_flag == 'bottom_most_2m') |>
  mutate(variable = glue::glue('{variable}_deepest'),
         z = NA) |>
  select(-position_flag)

## combine new position vars together (assume shallowest DO from the EXO; assume shallowest Temp from the TempString)
new_position_vars_combined <- bind_rows(shallowest_height_vars_df, deepest_height_vars_df, shallowest_depth_vars_df, deepest_depth_vars_df) |>
  filter(!(variable %in% c("DO_mgL_mean_shallowest", "DOsat_percent_mean_shallowest") & z_ref == 'height_m')) |> ## remove DO from the top of the RDO string
  filter(!(variable %in% c("Temp_C_mean_shallowest") & z_ref == 'depth_m')) ## remove Temp from the EXO (temp string always higher)

## create full dataset with only position specific variables
combined_targets_all_position_vars <- bind_rows(combined_targets_w_position, new_position_vars_combined)

## combined variables with NO position information here (NA values)
combined_targets_no_position_vars <- bind_rows(secchi_daily, eddy_flux, mom_daily_targets, thermocline_depth, schmidt_stability)

combined_targets <- bind_rows(combined_targets_all_position_vars, combined_targets_no_position_vars)

## average any duplicate observations at the same site, time, variable and position
combined_targets_deduped <- combined_targets |>
  mutate(project_id = 'vera4cast',
         variable = as.character(variable)) |> ## glue() in the shallowest/deepest step changes the class
  group_by(project_id, site_id, datetime, duration, variable, z, z_ref) |>
  summarise(observation = mean(observation, na.rm = TRUE), .groups = 'drop') |>
  mutate(observation = ifelse(is.nan(observation), NA, observation)) |>
  select(project_id, site_id, datetime, duration, z, z_ref, variable, observation)

## duration is left out of the check so rows that only differ by duration are caught
combined_dup_check <- combined_targets_deduped |>
  dplyr::count(datetime, site_id, variable, z, z_ref) |>
  dplyr::filter(n > 1)

if (nrow(combined_dup_check) != 0){
  print('target duplicates found...please fix')
  stop()
}

#arrow::write_csv_arrow(combined_targets_deduped, sink = s3_daily$path("daily-insitu-targets.csv.gz"))
duckdbfs::write_dataset(combined_targets_deduped,
                        path = "s3://bio230121-bucket01/vera4cast/targets_v2/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz",
                        format = 'csv')


## HOURLY INSITU (TEMPERATURE STRING: thermistor temp + RDO DO)
source('targets/target_functions_v2/target_generation_ThermistorTemp_C_hourly.R')

#FCR
print('FCR Hourly Thermistor')
fcr_latest <- "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv"
fcr_edi <- paste0("https://pasta.lternet.edu/package/data/eml/edi/271/10/814580ebec0385c66f0a0a97c38e9136?key=",edi_access_key)

fcr_thermistor_temp_hourly <- target_generation_ThermistorTemp_C_hourly_v2(current_file = fcr_latest, historic_file = fcr_edi)

# BVR
print('BVR Hourly Thermistor')
bvr_latest <- "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv"
bvr_edi <- paste0("https://pasta.lternet.edu/package/data/eml/edi/725/6/37e2587d2ab477068b295f97f1598cf9?key=",edi_access_key)

bvr_thermistor_temp_hourly <- target_generation_ThermistorTemp_C_hourly_v2(current_file = bvr_latest, historic_file = bvr_edi)

vera_hourly_thermistor_df <- dplyr::bind_rows(fcr_thermistor_temp_hourly, bvr_thermistor_temp_hourly) |>
  mutate(duration = 'PT1H',
         project_id = 'vera4cast')


## assign hourly positions ##
## match the daily water level to every hour of that day
hourly_df_with_wl <- vera_hourly_thermistor_df |>
  mutate(wl_date = lubridate::as_datetime(lubridate::as_date(datetime))) |>
  left_join(water_level_match_df, by = c('wl_date' = 'datetime', 'site_id')) |>
  mutate(obs_z_ref = waterlevel_m - z) |>
  select(-wl_date)

## hours without a water level can't be flagged as top (height)
hourly_missing_wl_days <- hourly_df_with_wl |>
  filter(is.na(waterlevel_m)) |>
  dplyr::distinct(site_id, date = lubridate::as_date(datetime)) |>
  dplyr::count(site_id, name = 'days_missing_water_level')

print('Hourly: days with targets but no water level (no shallowest assignment possible):')
print(hourly_missing_wl_days)

## same rules as the daily height-referenced data
hourly_position_assign_df <- hourly_df_with_wl |>
  filter(z_ref == 'height_m') |>
  drop_na(observation, z) |>
  group_by(site_id, datetime, variable) |>
  mutate(
    position_flag = case_when(
      z <= 2 & z == min(z, na.rm = TRUE) ~ 'bottom_most_2m', # get min height value
      obs_z_ref <= 2 & z == max(z, na.rm = TRUE) ~ 'top_most_2m' # get max height value
    )
  ) |>
  ungroup() |>
  select(datetime, site_id, z, z_ref, observation, variable, duration, project_id, position_flag)

hourly_shallowest_vars_df <- hourly_position_assign_df |>
  filter(position_flag == 'top_most_2m') |>
  mutate(variable = paste0(variable, '_shallowest'),
         z = NA)

hourly_deepest_vars_df <- hourly_position_assign_df |>
  filter(position_flag == 'bottom_most_2m') |>
  mutate(variable = paste0(variable, '_deepest'),
         z = NA)

## remove DO from the top of the RDO string (same as the daily targets)
hourly_new_position_vars <- bind_rows(hourly_shallowest_vars_df, hourly_deepest_vars_df) |>
  filter(!(variable %in% c("DO_mgL_mean_shallowest", "DOsat_percent_mean_shallowest"))) |>
  select(-position_flag)

hourly_targets <- bind_rows(vera_hourly_thermistor_df, hourly_new_position_vars)

## average any duplicate observations at the same site, time, variable and position
hourly_targets_deduped <- hourly_targets |>
  mutate(project_id = 'vera4cast') |>
  group_by(project_id, site_id, datetime, duration, variable, z, z_ref) |>
  summarise(observation = mean(observation, na.rm = TRUE), .groups = 'drop') |>
  mutate(observation = ifelse(is.nan(observation), NA, observation)) |>
  select(project_id, site_id, datetime, duration, z, z_ref, variable, observation)

hourly_dup_check <- hourly_targets_deduped |>
  dplyr::count(datetime, site_id, variable, z, z_ref) |>
  dplyr::filter(n > 1)

if (nrow(hourly_dup_check) != 0){
  print('hourly target duplicates found...please fix')
  stop()
}

duckdbfs::write_dataset(hourly_targets_deduped,
                        path = "s3://bio230121-bucket01/vera4cast/targets_v2/project_id=vera4cast/duration=PT1H/hourly-insitu-targets.csv.gz",
                        format = 'csv')
