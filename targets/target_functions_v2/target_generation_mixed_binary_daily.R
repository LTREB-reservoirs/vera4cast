# FO

# bvr_current <- c("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv")
# bvr_historic <- c("https://pasta.lternet.edu/package/data/eml/edi/725/3/a9a7ff6fe8dc20f7a8f89447d4dc2038")
#
# fcr_current <- "https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv"
# fcr_historic <- "https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f"

target_generation_mixed_binary_daily <- function(current_file, historic_file){
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
                  date = lubridate::as_date(DateTime)) |>
    dplyr::group_by(date, Reservoir, depth) |>
    dplyr::summarise(observation = mean(observation, na.rm = T),
                     n = dplyr::n(),
                     .groups = 'drop') |>
    dplyr::mutate(observation = ifelse(n < 144/2, NA, observation)) |> # 144 = 24 * 6(10 minute intervals/hr)
    dplyr::rename(site_id = Reservoir,
                  datetime = date)

  message('Current file ready')

  # read in historical data file
  # EDI
  infile <- tempfile()
  try(download.file(historic_file, infile, method="curl"))
  if (is.na(file.size(infile))) download.file(historic_file,infile,method="auto")
  historic_df <- readr::read_csv(infile, show_col_types = F) |>
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
                  date = lubridate::as_date(DateTime)) |>
    dplyr::group_by(date, Reservoir, depth) |>
    dplyr::summarise(observation = mean(observation, na.rm = T),
                     n = dplyr::n(),
                     .groups = 'drop') |>
    dplyr::mutate(observation = ifelse(n < 144/2, NA, observation)) |> # 144 = 24 * 6(10 minute intervals/hr)
    dplyr::rename(site_id = Reservoir,
                  datetime = date)
  message('EDI file ready')

  ## extract the depths that will be used to calculate the mixing metric (surface, bottom)
  depths_use <- dplyr::bind_rows(historic_df, current_df)  |>
    dplyr::mutate(depth = ifelse(depth == "surface", 0, depth)) |>
    na.omit() |>
    dplyr::group_by(datetime) |>
    dplyr::summarise(top = min(as.numeric(depth)),
                     bottom = max(as.numeric(depth))) |>
    tidyr::pivot_longer(cols = top:bottom,
                        values_to = 'depth')

  ## bind the two files using row.bind()
  final_df <- dplyr::bind_rows(historic_df, current_df) |>
    dplyr::mutate(depth = as.numeric(ifelse(depth == "surface", 0, depth))) |>
    dplyr::right_join(depths_use, by = c('datetime', 'depth')) |>
    dplyr::mutate(density = rLakeAnalyzer::water.density(observation)) |>
    dplyr::select(-observation, -depth) |>
    tidyr::pivot_wider(names_from = name,
                       names_prefix = 'density_',
                       values_from = density) |>
    dplyr::mutate(density_diff = density_top - density_bottom,
                  NotStratified_binary = ifelse(abs(density_diff) < 0.1, 1, 0)) |>
    dplyr::select(datetime, site_id, NotStratified_binary) |>
    tidyr::pivot_longer(cols = NotStratified_binary,
                        names_to = 'variable',
                        values_to = 'observation')

  final_df$variable <- 'Mixed_binary_mean'
  final_df$depth_m <- NA
  final_df$duration <- 'P1D'
  final_df$project_id <- 'vera4cast'


  ## Match data to flare targets file
  # Use pivot_longer to create a long-format table
  # for time specific - use midnight UTC values for daily
  # for hourly

  ## return dataframe formatted to match FLARE targets
  return(final_df)
}

#a <- target_generation_mixed_binary(current_file = fcr_current, historic_file = fcr_historic)
