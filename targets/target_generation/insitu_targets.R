library(RCurl)

## set destination s3 paths
s3 <- arrow::s3_bucket("bio230121-bucket01", endpoint_override = "amnh1.osn.mghpcc.org")
s3$CreateDir("vera4cast/targets/duration=P1D")
s3$CreateDir("vera4cast/targets/duration=PT1H")

duckdbfs::duckdb_secrets(
  endpoint = 'amnh1.osn.mghpcc.org',
  key = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"))

s3_daily <- arrow::s3_bucket("bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D", endpoint_override = "amnh1.osn.mghpcc.org")
s3_hourly <- arrow::s3_bucket("bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=PT1H", endpoint_override = "amnh1.osn.mghpcc.org")

column_names <- c("project_id", "site_id","datetime","duration", "depth_m","variable","observation")

## EXO
print('EXO')
source('targets/target_functions/target_generation_exo_daily.R')
fcr_files <- c("https://pasta.lternet.edu/package/data/eml/edi/271/10/814580ebec0385c66f0a0a97c38e9136",
               "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")

bvr_files <- c("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv",
               "https://pasta.lternet.edu/package/data/eml/edi/725/6/37e2587d2ab477068b295f97f1598cf9")

exo_daily <- target_generation_exo_daily(fcr_files, bvr_files)

exo_daily$duration <- 'P1D'
exo_daily$project_id <- 'vera4cast'


### NOTE : RDO DO DATA IS INCLUDED IN THE EXO TARGET GENERATION SCRIPT


## FLUOROPROBE
print('Fluoroprobe')
source('targets/target_functions/target_generation_FluoroProbe.R')
historic_data <- "https://pasta.lternet.edu/package/data/eml/edi/272/10/6d7576cc758ca378fe004ad0ac9eed85"
current_data <- "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/refs/heads/master/Data/DataNotYetUploadedToEDI/FluoroProbe/fluoroprobe_L1.csv"

fluoro_daily <- target_generation_FluoroProbe(current_file = current_data, historic_file = historic_data)
fluoro_daily$duration <- 'P1D'
fluoro_daily$project_id <- 'vera4cast'


### TEMP STRING
source('targets/target_functions/target_generation_ThermistorTemp_C_daily.R')

#
print('FCR Thermistor')
fcr_latest <- "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv"
fcr_edi <- "https://pasta.lternet.edu/package/data/eml/edi/271/10/814580ebec0385c66f0a0a97c38e9136"

fcr_thermistor_temp_daily <- target_generation_ThermistorTemp_C_daily(current_file = fcr_latest, historic_file = fcr_edi)
fcr_thermistor_temp_daily$duration <- 'P1D'
fcr_thermistor_temp_daily$project_id <- 'vera4cast'

# BVR
print('BVR Thermistor')
bvr_latest <- "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv"
bvr_edi <- "https://pasta.lternet.edu/package/data/eml/edi/725/6/37e2587d2ab477068b295f97f1598cf9"

bvr_thermistor_temp_daily <- target_generation_ThermistorTemp_C_daily(current_file = bvr_latest, historic_file = bvr_edi)
bvr_thermistor_temp_daily$duration <- 'P1D'
bvr_thermistor_temp_daily$project_id <- 'vera4cast'


#Secchi
print('Secchi')
source('targets/target_functions/target_generation_daily_secchi_m.R')
current = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Secchi/secchi_L1.csv"
edi = "https://pasta.lternet.edu/package/data/eml/edi/198/14/547f9a388a26711b4b10e4c7ad7e1a4e"

secchi_daily <- target_generation_daily_secchi_m(current = current, edi = edi) |>
  filter(site_id %in% c('fcre', 'bvre'))

secchi_daily$duration <- 'P1D'
secchi_daily$project_id <- 'vera4cast'



##Eddy Flux
print( 'Eddy Flux')
source('targets/target_functions/generate_EddyFlux_ghg_targets_function.R')
eddy_flux <- generate_EddyFlux_ghg_targets_function(
  flux_current_data_file = "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-eddyflux-data-qaqc/EddyFlux_streaming_L1.csv",
  flux_edi_data_file = "https://pasta.lternet.edu/package/data/eml/edi/1061/5/9686456c091a205d2444fb01342eeedc",
  met_current_data_file = "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-metstation-data-qaqc/FCRmet_L1.csv",
  met_edi_data_file = "https://pasta.lternet.edu/package/data/eml/edi/389/10/d3f3d2fa40c41fdcd505ae49b2fdcf8b")

eddy_flux$datetime <- lubridate::as_datetime(eddy_flux$datetime)


## CHEM
print('Chemistry')
source('targets/target_functions/target_generation_chemistry_daily.R')
chem_data <- target_generation_chemistry_daily(current_data_file = NULL,
                                               historic_data_file = 'https://pasta.lternet.edu/package/data/eml/edi/199/13/3f09a3d23b7b5dd32ed7d28e9bc1b081')
chem_data$datetime <- lubridate::as_datetime(chem_data$datetime)

## SILICA
print('Silica')
source('targets/target_functions/target_generation_silica_daily.R')
silica_data <- target_generation_silica_daily(current_data_file = NULL,
                                              historic_data_file = "https://pasta.lternet.edu/package/data/eml/edi/542/1/791ec9ca0f1cb9361fa6a03fae8dfc95")
silica_data$datetime <- lubridate::as_datetime(silica_data$datetime)


## GHG
print('GHG')
source('targets/target_functions/target_generation_ghg_daily.R')
ghg_data <- target_generation_ghg_daily(current_data_file = 'https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Raw_GHG/L1_manual_GHG.csv',
                                       edi_data_file = 'https://pasta.lternet.edu/package/data/eml/edi/551/10/ee8e65a2a380c9fb63bbf7f4a542c895')
ghg_data$datetime <- lubridate::as_datetime(ghg_data$datetime)


## CTD  - MOM
print('CTD - MOM')
source('targets/target_functions/targets_generation_daily_MOM.R')
historic_file  <- "https://pasta.lternet.edu/package/data/eml/edi/200/16/d8de9befde67007072850897d5dd2e06"
current_file <-  "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Raw_CTD/ctd_L1.csv"

mom_daily_targets <- targets_generation_daily_MOM(current_file = current_file, historic_file = historic_file)

## Thermocline Depth
print('Thermocline Depth')
source('targets/target_functions/generate_thermoclineD.R')
fcr_latest <- "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv"
fcr_edi <- "https://pasta.lternet.edu/package/data/eml/edi/271/10/814580ebec0385c66f0a0a97c38e9136"

thermocline_depth_fcr <- generate_thermocline_depth(current_file = fcr_latest,
                                                    historic_file = fcr_edi)

bvr_latest <- "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv"
bvr_edi <- "https://pasta.lternet.edu/package/data/eml/edi/725/6/37e2587d2ab477068b295f97f1598cf9"

thermocline_depth_bvr <- generate_thermocline_depth(current_file = bvr_latest,
                                                    historic_file = bvr_edi)

thermocline_depth <- bind_rows(thermocline_depth_fcr, thermocline_depth_bvr)
## Schmidt Stability
print('Schmidt Stability')
source('targets/target_functions/target_generation_SchmidtStability.R')
fcr_files <- c("https://pasta.lternet.edu/package/data/eml/edi/271/10/814580ebec0385c66f0a0a97c38e9136",
               "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv")

schmidt_stability_fcr <- generate_schmidt.stability(current_file = fcr_files[2], historic_file = fcr_files[1])

bvr_files <- c("https://pasta.lternet.edu/package/data/eml/edi/725/5/f649de0e8a468922b40dcfa34285055e",
               "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv")

#schmidt_stability_bvr <- generate_schmidt.stability(current_file = bvr_files[2], historic_file = bvr_files[1])

#schmidt_stability <- bind_rows(schmidt_stability_fcr, schmidt_stability_bvr)
schmidt_stability <- schmidt_stability_fcr

## combine the data and perform final adjustments (depth, etc.)

combined_targets <- bind_rows(exo_daily, fluoro_daily, fcr_thermistor_temp_daily, bvr_thermistor_temp_daily, secchi_daily,
                              mom_daily_targets, thermocline_depth, schmidt_stability, eddy_flux, chem_data, ghg_data, silica_data) |>
  select(all_of(column_names))

combined_targets_deduped <- combined_targets |>
  group_by(datetime, site_id, variable, depth_m) |>
  mutate(obs_deduped = mean(observation, na.rm = TRUE)) |>
  ungroup() |>
  distinct(datetime, site_id, variable, depth_m, .keep_all = TRUE) |>
  select(project_id, site_id, datetime, duration, depth_m, variable, observation)

combined_targets_deduped$project_id <- 'vera4cast'

combined_dup_check <- combined_targets_deduped  %>%
  dplyr::group_by(datetime, site_id, variable, depth_m) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1)

if (nrow(combined_dup_check) != 0){
  print('target duplicates found...please fix')
  stop()
}

#arrow::write_csv_arrow(combined_targets_deduped, sink = s3_daily$path("daily-insitu-targets.csv.gz"))
duckdbfs::write_dataset(combined_targets_deduped,
                        path = "s3://bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz",
                        format = 'csv')


## HOURLY INSITU (TEMPERATURE)
source('targets/target_functions/target_generation_ThermistorTemp_C_hourly.R')

#FCR
print('Hourly Temperature')
fcr_latest <- "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv"
fcr_edi <- "https://pasta.lternet.edu/package/data/eml/edi/271/10/814580ebec0385c66f0a0a97c38e9136"

fcr_thermistor_temp_hourly <- target_generation_ThermistorTemp_C_hourly(current_file = fcr_latest, historic_file = fcr_edi)
fcr_thermistor_temp_hourly$duration <- 'PT1H'
fcr_thermistor_temp_hourly$project_id <- 'vera4cast'


# BVR
print('BVR Thermistor')
bvr_latest <- "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv"
bvr_edi <- "https://pasta.lternet.edu/package/data/eml/edi/725/6/37e2587d2ab477068b295f97f1598cf9"

bvr_thermistor_temp_hourly <- target_generation_ThermistorTemp_C_hourly(current_file = bvr_latest, historic_file = bvr_edi)
bvr_thermistor_temp_hourly$duration <- 'PT1H'
bvr_thermistor_temp_hourly$project_id <- 'vera4cast'


s3_hourly <- arrow::s3_bucket("bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=PT1H", endpoint_override = "amnh1.osn.mghpcc.org")
vera_hourly_thermistor_df <- dplyr::bind_rows(fcr_thermistor_temp_hourly, bvr_thermistor_temp_hourly)

#arrow::write_csv_arrow(vera_hourly_thermistor_df, sink = s3_hourly$path("hourly-insitu-targets.csv.gz"))
duckdbfs::write_dataset(combined_targets_deduped,
                        path = "s3://bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=PT1H/hourly-insitu-targets.csv.gz",
                        format = 'csv')
