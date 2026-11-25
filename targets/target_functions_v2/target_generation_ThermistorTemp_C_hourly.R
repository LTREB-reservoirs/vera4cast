## Hourly temperature string (thermistor + RDO) targets for FCR and BVR
##
## Hourly version of target_generation_ThermistorTemp_C_daily.R (v2). The
## 10-minute sensor data are averaged to an hourly time step (following
## target_generation_ThermistorTemp_C_hourly.R) and the same position
## assignments as the daily v2 script are applied:
##   - BVR: sensor Position is matched to the BVR depth-offset file and the
##          height is Distance_above_sediments for that time period
##   - FCR: sensor depth label is converted to a height above the sediments
##          as z = (max(depth) + 0.5) - depth
##
## Output columns: datetime, site_id, z, z_ref, observation, variable, duration

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

bvr_depth_offset_url <- "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv"

# an hourly mean needs at least half of the six 10-minute readings in that hour
min_obs_per_hour <- 6/2

# ---- helpers ----------------------------------------------------------------

## pivot the wide sensor columns to long format
## (Reservoir, Site, DateTime, variable, units, <position column>, observation)
temp_string_to_long <- function(df, position_col) {
  df |>
    dplyr::select(any_of(c("Reservoir", "Site", "DateTime")),
                  starts_with("Ther"),
                  starts_with("RDO")) |>
    tidyr::pivot_longer(-any_of(c("Reservoir", "Site", "DateTime")),
                        names_to = "variable",
                        values_to = "observation",
                        values_drop_na = FALSE) |>
    tidyr::separate_wider_delim(cols = variable,
                                names = c("variable", "units", position_col),
                                delim = "_")
}

## BVR: aggregate to hourly by sensor position, then assign heights from the
## depth-offset file
bvr_hourly <- function(df, depth_offsets) {
  long <- df |>
    temp_string_to_long(position_col = "Position") |>
    dplyr::mutate(Position = as.numeric(Position),
                  DateTime = lubridate::floor_date(DateTime, unit = "hour")) |>
    na.omit() |>
    dplyr::group_by(DateTime, Reservoir, Position, variable) |>
    dplyr::summarise(observation = mean(observation, na.rm = TRUE),
                     n = dplyr::n(),
                     .groups = 'drop') |>
    dplyr::mutate(observation = ifelse(n < min_obs_per_hour, NA, observation),
                  Reservoir = 'BVR') |>
    dplyr::select(-n) |>
    dplyr::filter(variable != 'RDOTemp') ## remove RDO Temp

  # merge with the depth offsets file using the time period each offset applies to
  by <- dplyr::join_by(between(x$DateTime, y$Offset_start, y$Offset_end), Position, Reservoir)

  dplyr::full_join(long, depth_offsets, by) |>
    tidyr::drop_na(DateTime) |>
    tidyr::drop_na(observation) |>
    dplyr::mutate(site_id = 'bvre',
                  datetime = DateTime,
                  z = Distance_above_sediments,
                  z_ref = 'height_m') |>
    dplyr::select(site_id, datetime, variable, z, z_ref, observation)
}

## FCR: use the adjusted RDO columns, aggregate to hourly by sensor depth, then
## convert depth to height above the sediments
fcr_hourly <- function(df) {
  df |>
    dplyr::select(-RDO_mgL_5, -RDOsat_percent_5, -RDO_mgL_9, -RDOsat_percent_9) |>
    dplyr::rename(RDO_mgL_5 = RDO_mgL_5_adjusted, RDOsat_percent_5 = RDOsat_percent_5_adjusted,
                  RDO_mgL_9 = RDO_mgL_9_adjusted, RDOsat_percent_9 = RDOsat_percent_9_adjusted) |>
    temp_string_to_long(position_col = "depth") |>
    dplyr::mutate(depth = as.numeric(ifelse(depth == "surface", 0.1, depth))) |>
    dplyr::select(-units) |>
    dplyr::mutate(Reservoir = 'fcre',
                  DateTime = lubridate::floor_date(DateTime, unit = "hour")) |>
    na.omit() |>
    dplyr::group_by(DateTime, Reservoir, depth, variable) |>
    dplyr::summarise(observation = mean(observation, na.rm = TRUE),
                     n = dplyr::n(),
                     .groups = 'drop') |>
    dplyr::mutate(observation = ifelse(n < min_obs_per_hour, NA, observation)) |>
    dplyr::rename(site_id = Reservoir,
                  datetime = DateTime) |>
    dplyr::select(-n) |>
    dplyr::mutate(z = (max(depth) + 0.5) - depth,
                  z_ref = 'height_m') |>
    dplyr::select(-depth) |>
    dplyr::filter(variable != 'RDOTemp') ## remove RDO Temp
}

## read one file (current or EDI) and return the hourly, height-referenced data
process_temp_string_file <- function(file) {
  df <- readr::read_csv(file, show_col_types = FALSE) |>
    dplyr::filter(Site == 50)

  if (df$Reservoir[1] == 'BVR') {
    # "current" offsets are open-ended. Use a far-future end date rather than
    # Sys.time(): the newest hourly timestamps can be later than Sys.time()
    # (EST times labelled UTC) and would otherwise get no height assigned
    depth_offsets <- readr::read_csv(bvr_depth_offset_url, show_col_types = FALSE) |>
      dplyr::mutate(Offset_end = lubridate::ymd_hms(ifelse(Offset_end == "current",
                                                           "2100-01-01 00:00:00",
                                                           Offset_end)))
    return(bvr_hourly(df, depth_offsets))
  }

  if (df$Reservoir[1] == 'FCR') {
    return(fcr_hourly(df))
  }

  stop("Unrecognized Reservoir in ", file, ": ", df$Reservoir[1])
}

# ---- main function ----------------------------------------------------------

## waterlevel_file is accepted for parity with the daily v2 function but is not
## currently used (the position flags are commented out in the daily script too)
target_generation_ThermistorTemp_C_hourly_v2 <- function(current_file, historic_file, waterlevel_file = NULL){

  current_df_1 <- process_temp_string_file(current_file)
  message('Current file ready')
  print(nrow(current_df_1))

  historic_df_1 <- process_temp_string_file(historic_file)
  message('EDI file ready')

  final_df <- dplyr::bind_rows(historic_df_1, current_df_1) |>
    dplyr::mutate(variable = ifelse(variable == 'RDO', 'DO_mgL_mean', variable),
                  variable = ifelse(variable == 'RDOsat', 'DOsat_percent_mean', variable),
                  variable = ifelse(variable == 'ThermistorTemp', 'Temp_C_mean', variable),
                  observation = ifelse(is.nan(observation), NA, observation),
                  duration = 'PT1H') |>
    dplyr::select(datetime, site_id, z, z_ref, observation, variable, duration)

  return(final_df)
}

# fcr_latest <- "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv"
# fcr_edi <- paste0("https://pasta.lternet.edu/package/data/eml/edi/271/10/814580ebec0385c66f0a0a97c38e9136?key=", edi_access_key)
# fcr_hourly_df <- target_generation_ThermistorTemp_C_hourly_v2(current_file = fcr_latest, historic_file = fcr_edi)
#
# bvr_latest <- "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv"
# bvr_edi <- paste0("https://pasta.lternet.edu/package/data/eml/edi/725/6/37e2587d2ab477068b295f97f1598cf9?key=", edi_access_key)
# bvr_hourly_df <- target_generation_ThermistorTemp_C_hourly_v2(current_file = bvr_latest, historic_file = bvr_edi)
