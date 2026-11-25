# fcr_current <- "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv"
# bvr_current <- "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv"
# historic_ice <- "https://pasta.lternet.edu/package/data/eml/edi/456/4/8454a757d0203bd8913d8adc8607f6c4"

target_IceCover_binary <- function(current_file, historic_file){

  ## read in current data file
  # Github, Googlesheet, etc.
  current_df <- readr::read_csv(current_file, show_col_types = F)|>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp')) |>
    tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                        names_to = 'depth',
                        names_prefix = 'ThermistorTemp_C_',
                        values_to = 'observation') |>
    dplyr::mutate(Reservoir = ifelse(Reservoir == 'FCR',
                                     'fcre',
                                     ifelse(Reservoir == 'BVR',
                                            'bvre', NA)),
                  datetime = lubridate::as_datetime(paste0(format(DateTime, "%Y-%m-%d %H"), ":00:00"))) |>
    dplyr::group_by(Reservoir, datetime, depth) |>
    dplyr::summarise(observation = mean(observation, na.rm = T),
                     .groups = 'drop') |>
    dplyr::rename(site_id = Reservoir)

  # the depths used to assess will change depending on the current depth of FCR
  depths_use <- current_df |>
    dplyr::mutate(depth = ifelse(depth == "surface", 0.1, depth)) |>
    na.omit() |>
    dplyr::group_by(datetime) |>
    dplyr::summarise(top = min(as.numeric(depth)),
                     bottom = max(as.numeric(depth))) |>
    tidyr::pivot_longer(cols = top:bottom,
                        values_to = 'depth')


  current_on_off <- current_df |>
    dplyr::mutate(depth = as.numeric(ifelse(depth == "surface", 0.1, depth))) |>
    dplyr::right_join(depths_use, by = c('datetime', 'depth')) |>
    dplyr::select(-depth) |>
    tidyr::pivot_wider(names_from = name,
                       values_from = observation) |>
    # Ice defined as when the top is cooler than the bottom, and temp below 4 oC
    dplyr::mutate(temp_diff = top - bottom,
                  variable = ifelse(temp_diff < -0.1 & top <= 4, 'IceOn', 'IceOff')) |>
                  #surface cooler than bottom (within error (0.1) of the sensors)
    dplyr::select(datetime, site_id, variable) |>
    tidyr::pivot_longer(cols = variable,
                        names_to = 'variable',
                        values_to = 'observation')

  #when does the value change from ice on to off? (vice versa)
  rle_ice <- rle(current_on_off$observation)

  current_ice_df <- data.frame(variable = rle_ice$values, # this is whether the ice is on or off
                               length = rle_ice$lengths) |> # how long is the run of this condition?
    dplyr::mutate(end_n = cumsum(length), # cumsum is the index of the end of each run
                  start_n = 1 +lag(end_n) - length, # this the index of the start of the run
                  datetime = lubridate::as_date(current_on_off$datetime[start_n])) |> # relates this the date
    dplyr::select(variable, datetime) |>
    dplyr::mutate(site_id = current_df$site_id[1],
                  observation = 1) |> # these are all where ice on/off is occuring
    dplyr::select(site_id, datetime, variable, observation)

  message('Current file ready')

  # read in historical data file
  # EDI
  infile <- tempfile()
  try(download.file(historic_file, infile, method="curl"))
  if (is.na(file.size(infile))) download.file(historic_file,infile,method="auto")
  historic_df <- readr::read_csv(infile, show_col_types = F) |>
    dplyr::mutate(site_id = ifelse(Reservoir == 'FCR', 'fcre',
                                   ifelse(Reservoir == 'BVR', 'bvre', NA))) |>
    dplyr::filter(site_id == current_df$site_id[1])

  historic_ice_df <- historic_df |>
    dplyr::select(site_id, Date, IceOn, IceOff) |>
    tidyr::pivot_longer(names_to = 'variable',
                        values_to = 'observation',
                        cols = IceOn:IceOff) |>
    dplyr::rename(datetime = Date)
  message('EDI file ready')

  ## manipulate the data files to match each other

  # dates
  period <- dplyr::bind_rows(historic_ice_df, current_ice_df) |>
    dplyr::summarise(first = min(datetime),
                     last = max(datetime))

  # get all the days to fill in with 0
  all_dates <- expand.grid(datetime = seq.Date(period$first,
                                               period$last, by = 'day'),
                           variable = c('IceOn', 'IceOff'),
                           site_id = current_df$site_id[1])

  ## bind the two files using row.bind()
  final_df <- dplyr::bind_rows(historic_ice_df, current_ice_df) |>
    dplyr::full_join(all_dates, by = dplyr::join_by(site_id, datetime, variable)) |>
    dplyr::arrange(site_id, datetime) |>
    dplyr::mutate(observation = ifelse(is.na(observation), 0, observation))
  ## Match data to flare targets file
  # Use pivot_longer to create a long-format table
  # for time specific - use midnight UTC values for daily
  # for hourly

  final_df$depth_m <- NA
  final_df$duration <- 'P1D'
  final_df$project_id <- 'vera4cast'

  ## return dataframe formatted to match FLARE targets
  return(final_df)
}

#test_ice <- target_IceCover_binary(current_file = fcr_current, historic_file = historic_ice)
