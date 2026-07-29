## FO

generate_schmidt.stability <- function(current_file, historic_file) {

  ## set EDI API Key
  edi_access_key = Sys.getenv('EDI_ACCESS_KEY')

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
                              output = NULL) |>
      dplyr::filter(variable == 'ThermistorTemp') |>
      dplyr::select(Reservoir, DateTime, Depth_m, variable, depth_bin, Position) |>
      dplyr::rename(WaterLevel = Depth_m,
                    depth = depth_bin) |>
      dplyr::mutate(date = lubridate::as_date(DateTime))

    current_df_1 <- current_df  |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = c('variable','Position'),
                          names_sep = '_C_',
                          values_to = 'observation') |>
      dplyr::mutate(date = lubridate::as_date(DateTime),
                    Position = as.numeric(Position)) |>
      na.omit() |>
      dplyr::left_join(bvr_depths,
                       by = c('Position', 'date', 'Reservoir', 'variable'), multiple = 'all') |>
      dplyr::group_by(date, Reservoir, depth) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       WaterLevel = mean(WaterLevel, na.rm=T),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 144/3, NA, observation)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
      na.omit()

  }

  # read in differently for FCR
  if (current_df$Reservoir[1] == 'FCR') {
    current_df_1 <- current_df |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = 'depth',
                          names_prefix = 'ThermistorTemp_C_',
                          values_to = 'observation') |>
      na.omit()|>
      dplyr::mutate(date = lubridate::as_date(DateTime),
                    depth = ifelse(depth == 'surface', 0.1, depth)) |>
      na.omit() |>
      dplyr::group_by(date, Reservoir, depth) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 144/3, NA, observation)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
      na.omit()

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

  if (historic_df$Reservoir[1] == 'BVR') {
    bvr_depths <- find_depths(data_file = historic_file,
                              depth_offset = "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv",
                              output = NULL) |>
      dplyr::filter(variable == 'ThermistorTemp') |>
      dplyr::select(Reservoir, DateTime, Depth_m, variable, depth_bin, Position) |>
      dplyr::rename(WaterLevel = Depth_m,
                    depth = depth_bin) |>
      dplyr::mutate(date = lubridate::as_date(DateTime))

    historic_df_1 <- historic_df  |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = c('variable','Position'),
                          names_sep = '_C_',
                          values_to = 'observation') |>
      dplyr::mutate(date = lubridate::as_date(DateTime),
                    Position = as.numeric(Position)) |>
      na.omit() |>
      dplyr::left_join(bvr_depths,
                       by = c('Position', 'date', 'Reservoir', 'variable'), multiple = 'all') |>
      dplyr::group_by(date, Reservoir, depth) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       WaterLevel = mean(WaterLevel, na.rm=T),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 144/3, NA, observation)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
      na.omit()

  }

  # read in differently for FCR
  if (historic_df$Reservoir[1] == 'FCR') {
    historic_df_1 <- historic_df |>
      tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
                          names_to = 'depth',
                          names_prefix = 'ThermistorTemp_C_',
                          values_to = 'observation') |>
      na.omit()|>
      dplyr::mutate(date = lubridate::as_date(DateTime),
                    depth = ifelse(depth == 'surface', 0.1, depth)) |>
      na.omit() |>
      dplyr::group_by(date, Reservoir, depth) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 144/3, NA, observation)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
      na.omit()

  }
  message('EDI file ready')

  #combine the current and historic dataframes
  final_df <- dplyr::bind_rows(historic_df_1, current_df_1) |>
    dplyr::select(any_of(c('date', 'Reservoir', 'depth', 'observation', 'WaterLevel')))

  # # Need bathymetry
  # infile2 <- tempfile()
  # try(download.file("https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184",
  #                   infile2, method="curl"))
  # if (is.na(file.size(infile2))) download.file("https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184",
  #                                              infile2,method="auto")

  bathymetry <- readr::read_csv(paste0('https://pasta.lternet.edu/package/data/eml/edi/1254/1/f7fa2a06e1229ee75ea39eb586577184?key=',edi_access_key), show_col_types = F)  |>
    dplyr::select(Reservoir, Depth_m, SA_m2) |>
    # dplyr::rename(depths = Depth_m,
    #               areas = SA_m2) |>
    dplyr::filter(Reservoir == unique(final_df$Reservoir))

  # BVR requires flexible bathymetry to generate schmidt stability
  if (final_df$Reservoir[1] == 'BVR') {

    #Create a dataframe with bathymetry at each date
    flexible_bathy <- final_df |> # takes the depth at each day
      dplyr::distinct(date, WaterLevel, Reservoir) |>
      dplyr::full_join(bathymetry, multiple = 'all', by = 'Reservoir') |>
      dplyr::group_by(date) |>
      dplyr::mutate(Depth_m = Depth_m - (max(Depth_m) - mean(unique(WaterLevel))),
                    WaterLevel = mean(WaterLevel)) |>
      dplyr::filter(Depth_m>=0) |>
      dplyr::distinct()

  }

  if (final_df$Reservoir[1] == 'FCR') {
    flexible_bathy <- final_df|>
      dplyr::full_join(bathymetry, multiple = 'all', by = 'Reservoir')
  }


  #Calculate schmidt stability each day
  schmidts <- numeric(length(unique(final_df$date)))
  dates <- unique(final_df$date)

  for(i in 1:length(dates)) {
    baths <- flexible_bathy |>
      dplyr::filter(date==dates[i])

    temps <- final_df |>
      dplyr::filter(date == dates[i],
                    # cannot have an observation at a depth shallower than the
                    # shallowest bathymetry (returns NA below) so these are filtered out
                    depth >= min(baths$Depth_m))



    schmidts[i] <- rLakeAnalyzer::schmidt.stability(wtr = temps$observation,
                                                    depths = temps$depth,
                                                    bthA = baths$SA_m2,
                                                    bthD = baths$Depth_m,
                                                    sal = rep(0,length(temps$observation)))

    # print(i)

  }

  final_ss <- data.frame(datetime = unique(final_df$date),
                         site_id = current_df$Reservoir[1],
                         depth_m = as.numeric(NA),
                         observation = schmidts,
                         variable = 'SchmidtStability_Jm2_mean',
                         duration = "P1D",
                         project_id = 'vera4cast') |>
    dplyr::mutate(site_id = ifelse(site_id == 'FCR',
                              'fcre',
                              ifelse(site_id == 'BVR',
                                     'bvre', NA)))
  ## Match data to flare targets file
  return(final_ss)
}

#a <- generate_schmidt.stability(current_file = latest, historic_file = edi)
