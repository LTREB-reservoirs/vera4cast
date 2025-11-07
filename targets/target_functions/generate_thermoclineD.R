install.packages('rLakeAnalyzer')
library(rLakeAnalyzer)

generate_thermocline_depth <- function(current_file, historic_file) {
  options(dplyr.summarise.inform = FALSE)
  source('targets/target_functions/find_depths.R')

  ## Read in current data
  # Github, Googlesheet, etc.
  current_df <- readr::read_csv(current_file, show_col_types = F) |>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp'))

  if (current_df$Reservoir[1] == 'BVR') {
    bvr_depths <- find_depths(data_file = current_file,
                              depth_offset = "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv",
                              output = NULL) |>
      dplyr::filter(variable == 'ThermistorTemp') |>
      dplyr::select(Reservoir, DateTime, variable, depth_bin, Position)

    current_df_1 <- current_df  |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = c('variable','Position'),
                          names_sep = '_C_',
                          values_to = 'observation') |>
      dplyr::mutate(Position = as.numeric(Position)) |>
      na.omit() |>
      dplyr::left_join(bvr_depths, # join with the depths
                       by = c('Position', 'DateTime', 'Reservoir', 'variable')) |>
      dplyr::select(DateTime, observation, depth_bin) |>
      dplyr::group_by(DateTime, depth_bin) |>
      dplyr::summarise(observation = mean(observation, na.rm = T), .groups = 'drop') |>
      # in case there are multiple in the same bin
      dplyr::arrange(depth_bin, DateTime) |>
      na.omit() |>
      tidyr::pivot_wider(values_from = observation,
                         names_from = depth_bin,
                         names_prefix = 'wtr_') |>
      dplyr::mutate(Reservoir = 'BVR')
  }

  if (current_df$Reservoir[1] == 'FCR') {

    current_df_1 <- current_df |>
      dplyr::rename_with(~gsub('ThermistorTemp_C', 'wtr', .x)) |>
      dplyr::rename_with(~gsub('surface', '0.1', .x))
  }



  # what is the maximum depth?
  max_depth_current <- current_df_1 |>
    tidyr::pivot_longer(cols = tidyr::starts_with('wtr_'),
                        names_to = 'depth',
                        names_prefix = 'wtr_',
                        values_to = 'observation') |>
    dplyr::mutate(depth = as.numeric(depth)) |>
    dplyr::group_by(DateTime, Reservoir) |>
    na.omit() |>
    dplyr::summarise(max_depth = max(depth))

  # calculate the thermocline depth
  td_current <- rLakeAnalyzer::ts.thermo.depth(wtr = current_df_1[,-match('Reservoir', names(current_df_1))],
                                               na.rm = T) |>
    dplyr::left_join(max_depth_current, by = c('DateTime')) |>
    dplyr::mutate(Reservoir = ifelse(unique(current_df$Reservoir) == 'FCR',
                                     'fcre',
                                     ifelse(unique(current_df$Reservoir) == 'BVR',
                                            'bvre', NA)),
                  date = lubridate::as_date(DateTime),
                  thermo.depth = as.numeric(ifelse(is.nan(thermo.depth), max_depth, thermo.depth))) |>
    na.omit() |>
    dplyr::group_by(date, Reservoir) |>
    dplyr::summarise(thermo.depth = mean(thermo.depth, na.rm = T),
                     n = dplyr::n(),
                     .groups = 'drop') |>
    dplyr::mutate(thermo.depth = ifelse(n < 144/2,
                                        NA,
                                        thermo.depth)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
    dplyr::rename(site_id = Reservoir,
                  datetime = date) |>
    dplyr::select(-n)
  message('Current file ready')

  # read in historical data file
  # EDI
  infile <- tempfile()
  try(download.file(historic_file, infile, method="curl"))
  if (is.na(file.size(infile))) download.file(historic_file,infile,method="auto")
  historic_df <- readr::read_csv(historic_file, show_col_types = F) |>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp'))

  if (historic_df$Reservoir[1] == 'BVR') {
    bvr_depths <- find_depths(data_file = historic_file,
                              depth_offset = "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv",
                              output = NULL) |>
      dplyr::filter(variable == 'ThermistorTemp') |>
      dplyr::select(Reservoir, DateTime, variable, depth_bin, Position)

    historic_df_1 <- historic_df  |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = c('variable','Position'),
                          names_sep = '_C_',
                          values_to = 'observation') |>
      dplyr::mutate(Position = as.numeric(Position)) |>
      na.omit() |>
      dplyr::left_join(bvr_depths,
                       by = c('Position', 'DateTime', 'Reservoir', 'variable')) |>
      dplyr::select(DateTime, observation, depth_bin) |>
      dplyr::group_by(DateTime, depth_bin) |>
      dplyr::summarise(observation = mean(observation, na.rm = T), .groups = 'drop') |>
      dplyr::arrange(depth_bin, DateTime) |>
      na.omit() |>
      # in case there are multiple in the same bin
      tidyr::pivot_wider(values_from = observation,
                         names_from = depth_bin,
                         names_prefix = 'wtr_') |>
      dplyr::mutate(Reservoir = 'BVR')
  }

  if (historic_df$Reservoir[1] == 'FCR') {

    historic_df_1 <- historic_df |>
      dplyr::rename_with(~gsub('ThermistorTemp_C', 'wtr', .x)) |>
      dplyr::rename_with(~gsub('surface', '0.1', .x))
  }

  # what is the maximum depth?
  max_depth_historic <- historic_df_1 |>
    tidyr::pivot_longer(cols = tidyr::starts_with('wtr_'),
                        names_to = 'depth',
                        names_prefix = 'wtr_',
                        values_to = 'observation') |>
    dplyr::mutate(depth = as.numeric(depth)) |>
    dplyr::group_by(DateTime, Reservoir) |>
    na.omit() |>
    dplyr::summarise(max_depth = max(depth))

  # calculate the thermocline depth
  td_historic <- rLakeAnalyzer::ts.thermo.depth(wtr = historic_df_1[,-match('Reservoir', names(historic_df_1))],
                                                na.rm = T) |>
    dplyr::left_join(max_depth_historic, by = c('DateTime')) |>
    dplyr::mutate(Reservoir = ifelse(unique(current_df$Reservoir) == 'FCR',
                                     'fcre',
                                     ifelse(unique(current_df$Reservoir) == 'BVR',
                                            'bvre', NA)),
                  date = lubridate::as_date(DateTime),
                  thermo.depth = as.numeric(ifelse(is.nan(thermo.depth), max_depth, thermo.depth))) |>
    na.omit() |>
    dplyr::group_by(date, Reservoir) |>
    dplyr::summarise(thermo.depth = mean(thermo.depth, na.rm = T),
                     n = dplyr::n(),
                     .groups = 'drop') |>
    dplyr::mutate(thermo.depth = ifelse(n < 144/2,
                                        NA,
                                        thermo.depth)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
    dplyr::rename(site_id = Reservoir,
                  datetime = date) |>
    dplyr::select(-n)
  message('EDI file ready')

  ## bind the two files using row.bind()
  final_df <- dplyr::bind_rows(td_historic, td_current) |>
    tidyr::pivot_longer(cols = thermo.depth,
                        names_to = 'variable',
                        # names_prefix = 'mean_',
                        values_to = 'observation') |>
    dplyr::mutate(variable = 'ThermoclineDepth_m_mean',
                  duration = "P1D",
                  project_id = 'vera4cast',
                  depth_m = as.numeric(NA))
  ## Match data to flare targets file
  return(final_df)
}
