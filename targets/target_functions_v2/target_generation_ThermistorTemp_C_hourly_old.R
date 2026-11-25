target_generation_ThermistorTemp_C_hourly <- function(current_file, historic_file){
  source('targets/target_functions/find_depths.R')
  ## read in current data file
  # Github, Googlesheet, etc.
  current_df <- readr::read_csv(current_file, show_col_types = F) |>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp'))

  if (current_df$Reservoir[1] == 'BVR') {
    bvr_depths <- find_depths(data_file = current_file,
                                  depth_offset = "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv",
                                  output <- NULL) |>
      dplyr::filter(variable == 'ThermistorTemp') |>
      dplyr::select(Reservoir, DateTime, variable, depth_bin, Position)

    current_df_1 <- current_df  |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = c('variable','Position'),
                          names_sep = '_C_',
                          values_to = 'observation') |>
      dplyr::mutate(date = lubridate::as_datetime(paste0(format(DateTime, "%Y-%m-%d %H"), ":00:00")),
                    Position = as.numeric(Position)) |>
      na.omit() |>
      dplyr::left_join(bvr_depths,
                       by = c('Position', 'DateTime', 'Reservoir', 'variable')) |>
      dplyr::group_by(date, Reservoir, depth_bin) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 6/2, NA, observation), # 6 = 6(10 minute intervals/hr)
                    Reservoir = 'bvre') |>

      dplyr::rename(site_id = Reservoir,
                    datetime = date,
                    depth = depth_bin) |>
      dplyr::select(-n)
  }

  # read in differently for FCR
  if (current_df$Reservoir[1] == 'FCR') {
    current_df_1 <- current_df |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = 'depth',
                          names_prefix = 'ThermistorTemp_C_',
                          values_to = 'observation') |>
      dplyr::mutate(Reservoir = ifelse(Reservoir == 'FCR',
                                       'fcre',
                                       ifelse(Reservoir == 'BVR',
                                              'bvre', NA)),
                    date = lubridate::as_datetime(paste0(format(DateTime, "%Y-%m-%d %H"), ":00:00"))) |>
      na.omit() |>
      dplyr::group_by(date, Reservoir, depth) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 6/2, NA, observation)) |> # 6 = 6(10 minute intervals/hr)
      dplyr::rename(site_id = Reservoir,
                    datetime = date) |>
      dplyr::select(-n)
  }
  message('Current file ready')

  # read in historical data file
  # EDI
  # infile <- tempfile()
  # try(download.file(historic_file, infile, method="curl"))
  # if (is.na(file.size(infile))) download.file(historic_file,infile,method="auto")

  historic_df <- readr::read_csv(historic_file, show_col_types = F) |>
    dplyr::filter(Site == 50) |>
    dplyr::select(Reservoir, DateTime,
                  dplyr::starts_with('ThermistorTemp'))

  # Extract depths for BVR
  if (historic_df$Reservoir[1] == 'BVR') {
    bvr_depths <- find_depths(data_file = historic_file,
                              depth_offset = "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv",
                              output <- NULL) |>
      dplyr::filter(variable == 'ThermistorTemp') |>
      dplyr::select(Reservoir, DateTime, variable, depth_bin, Position)

    historic_df_1 <- historic_df |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = c('variable','Position'),
                          names_sep = '_C_',
                          values_to = 'observation') |>
      dplyr::mutate(date = lubridate::as_datetime(paste0(format(DateTime, "%Y-%m-%d %H"), ":00:00")),
                    Position = as.numeric(Position)) |>
      na.omit() |>
      dplyr::left_join(bvr_depths,
                       by = c('Position', 'DateTime', 'Reservoir', 'variable')) |>
      dplyr::group_by(date, Reservoir, depth_bin) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 144/3, NA, observation), # 144 = 24(hrs) * 6(10 minute intervals/hr)
                    Reservoir = 'bvre') |>

      dplyr::rename(site_id = Reservoir,
                    datetime = date,
                    depth = depth_bin) |>
      dplyr::select(-n)
  }

  if (historic_df$Reservoir[1] == 'FCR') {
    historic_df_1 <- historic_df |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = 'depth',
                          names_prefix = 'ThermistorTemp_C_',
                          values_to = 'observation') |>
      dplyr::mutate(Reservoir = ifelse(Reservoir == 'FCR',
                                       'fcre',
                                       ifelse(Reservoir == 'BVR',
                                              'bvre', NA)),
                    date = lubridate::as_datetime(paste0(format(DateTime, "%Y-%m-%d %H"), ":00:00"))) |>
      dplyr::group_by(date, Reservoir, depth)  |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 6/2, NA, observation)) |> # 6 = 6(10 minute intervals/hr)
      dplyr::rename(site_id = Reservoir,
                    datetime = date)|>
      dplyr::select(-n)
  }

  message('EDI file ready')

  ## manipulate the data files to match each other


  ## bind the two files using row.bind()
  final_df <- dplyr::bind_rows(historic_df_1, current_df_1) |>
    dplyr::mutate(variable = 'Temp_C_mean',
                  depth_m = as.numeric(ifelse(depth == "surface", 0.1, depth)))

  final_df$depth_m <- ifelse(final_df$site_id == 'fcre' & final_df$depth_m == 2,
                           1.6,
                           final_df$depth_m)

  final_df$depth_m <- ifelse(final_df$site_id == 'bvre' & final_df$depth_m == 2,
                           1.5,
                           final_df$depth_m)

  final_df$depth_m <- as.numeric(final_df$depth_m)
  final_df$depth <- NULL
  ## Match data to flare targets file
  # Use pivot_longer to create a long-format table
  # for time specific - use midnight UTC values for daily
  # for hourly

  ## return dataframe formatted to match FLARE targets
  return(final_df)
}
