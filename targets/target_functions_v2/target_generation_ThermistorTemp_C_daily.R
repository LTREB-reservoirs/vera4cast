target_generation_ThermistorTemp_C_daily <- function(current_file, historic_file, waterlevel_file){
  #source('targets/target_functions/find_depths.R')
  ## read in current data file
  # Github, Googlesheet, etc.
  current_df <- readr::read_csv(current_file, show_col_types = F) |>
    dplyr::filter(Site == 50) #|>
    #dplyr::select(Reservoir, DateTime)#,
                  #dplyr::starts_with('ThermistorTemp'))

  if (current_df$Reservoir[1] == 'BVR') {
    depth_offset <-  read_csv("https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv")

    # Make sure dates are in the right format
    depth <- depth_offset|>
      mutate(
        Offset_end = ymd_hms(ifelse(Offset_end == "current", as.character(Sys.time()),Offset_end)))

    # bvr_depths <- find_depths(data_file = current_file,
    #                               depth_offset = "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv",
    #                               output <- NULL) |>
    #   dplyr::filter(variable == 'ThermistorTemp') |>
    #   dplyr::select(Reservoir, DateTime, variable, depth_bin, Position)

    long <- current_df |>
      dplyr::select(any_of(c("Reservoir", "Site", "DateTime")),
                    #starts_with("Depth"),
                    starts_with("Ther"),
                    starts_with("RDO")
                    #starts_with("Lvl")
                    ) |>
      dplyr::rename("Depth_m" = contains("Depth")) |>
      tidyr::pivot_longer(-any_of(c("Reservoir", "Site", "DateTime", "Depth_m")),
                          names_to = "variable",
                          values_to = "observation",
                          values_drop_na = FALSE) |>
      tidyr::separate_wider_delim(cols = variable,
                                  names = c("variable","units","Position"),
                                  delim = "_") |>
      dplyr::mutate(Position = as.numeric(Position),
                    date = lubridate::as_date(DateTime)) |>
      na.omit() |>
      dplyr::group_by(date, Reservoir, Position, variable) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                        n = dplyr::n(),
                        .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 144/3, NA, observation), # 144 = 24(hrs) * 6(10 minute intervals/hr)
                    Reservoir = 'bvre') |>
      dplyr::rename(datetime = date) |>
      dplyr::select(-n) |>
      mutate(DateTime = lubridate::as_datetime(datetime),
             Reservoir = 'BVR') |>
      filter(variable != 'RDOTemp') ## remove RDO Temp


    # merge with the depths offset file
    by <- join_by(between(x$DateTime, y$Offset_start, y$Offset_end), Position, Reservoir)
    long_w_height  <- full_join(long, depth, by)|>
      drop_na(DateTime) # drop the NAs in DateTime column

    # Make a data frame with just the height above the sediment

    current_df_1 <- long_w_height |>
      select(Reservoir, datetime, variable, Position, observation, Distance_above_sediments)|>
      drop_na(observation) |>
      dplyr::rename(site_id = Reservoir) |>
      mutate(position_flag = NA) |>
      mutate(z = Distance_above_sediments,
             z_ref = 'height_m',
             site_id = 'bvre') |>
      select(site_id, datetime, variable, z, z_ref, observation, position_flag)


    #### end new updates ###

    # current_df_1 <- current_df  |>
    #   tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
    #                       names_to = c('variable','Position'),
    #                       names_sep = '_C_',
    #                       values_to = 'observation') |>
    #   dplyr::mutate(date = lubridate::as_date(DateTime),
    #                 Position = as.numeric(Position)) |>
    #   na.omit() |>
    #   dplyr::left_join(bvr_depths,
    #                    by = c('Position', 'DateTime', 'Reservoir', 'variable')) |>
    #   dplyr::group_by(date, Reservoir, depth_bin) |>
    #   dplyr::summarise(observation = mean(observation, na.rm = T),
    #                    n = dplyr::n(),
    #                    .groups = 'drop') |>
    #   dplyr::mutate(observation = ifelse(n < 144/3, NA, observation), # 144 = 24(hrs) * 6(10 minute intervals/hr)
    #                 Reservoir = 'bvre') |>
    #
    #   dplyr::rename(site_id = Reservoir,
    #                 datetime = date,
    #                 depth = depth_bin) |>
    #   dplyr::select(-n)
  }

  # read in differently for FCR
  if (current_df$Reservoir[1] == 'FCR') {
    current_df_1 <- current_df |>
      select(-RDO_mgL_5, -RDOsat_percent_5, -RDO_mgL_9, -RDOsat_percent_9) |>
      rename(RDO_mgL_5 = RDO_mgL_5_adjusted, RDOsat_percent_5 = RDOsat_percent_5_adjusted,
             RDO_mgL_9 = RDO_mgL_9_adjusted, RDOsat_percent_9 = RDOsat_percent_9_adjusted) |>
      dplyr::select(any_of(c("Reservoir", "Site", "DateTime")),
                    #starts_with("Depth"),
                    starts_with("Ther"),
                    starts_with("RDO")
                    #starts_with("Lvl")
      ) |>
      dplyr::rename("Depth_m" = contains("Depth")) |>
      tidyr::pivot_longer(-any_of(c("Reservoir", "Site", "DateTime", "Depth_m")),
                          names_to = "variable",
                          values_to = "observation",
                          values_drop_na = FALSE) |>
      tidyr::separate_wider_delim(cols = variable,
                                  names = c("variable","units","depth"),
                                  delim = "_") |>
      mutate(depth = as.numeric(ifelse(depth == "surface", 0.1, depth))) |>
      select(-units) |>
      dplyr::mutate(Reservoir = ifelse(Reservoir == 'FCR',
                                       'fcre',
                                       ifelse(Reservoir == 'BVR',
                                              'bvre', NA)),
                    date = lubridate::as_date(DateTime)) |>
      na.omit() |>
      dplyr::group_by(date, Reservoir, depth, variable) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 144/2, NA, observation)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
      dplyr::rename(site_id = Reservoir,
                    datetime = date) |>
      dplyr::select(-n) |>
      mutate(z = (max(depth) + 0.5) - depth,
             z_ref = 'height_m') |>
      select(-depth) |>
      filter(variable != 'RDOTemp') ## remove RDO Temp
  }
  message('Current file ready')
  print(nrow(current_df_1))

  # read in historical data file
  # EDI
  # infile <- tempfile()
  # try(download.file(historic_file, infile, method="curl"))
  # if (is.na(file.size(infile))) download.file(historic_file,infile,method="auto")

  historic_df <- readr::read_csv(historic_file, show_col_types = F) |>
     dplyr::filter(Site == 50) #|>
    # dplyr::select(Reservoir, DateTime,
    #               dplyr::starts_with('ThermistorTemp'))

  # Extract depths for BVR
  if (historic_df$Reservoir[1] == 'BVR') {

    long_historic <- historic_df |>
      dplyr::select(any_of(c("Reservoir", "Site", "DateTime")),
                    #starts_with("Depth"),
                    starts_with("Ther"),
                    starts_with("RDO")
                    #starts_with("Lvl")
      ) |>
      dplyr::rename("Depth_m" = contains("Depth")) |>
      tidyr::pivot_longer(-any_of(c("Reservoir", "Site", "DateTime", "Depth_m")),
                          names_to = "variable",
                          values_to = "observation",
                          values_drop_na = FALSE) |>
      tidyr::separate_wider_delim(cols = variable,
                                  names = c("variable","units","Position"),
                                  delim = "_") |>
      dplyr::mutate(Position = as.numeric(Position),
                    date = lubridate::as_date(DateTime)) |>
      na.omit() |>
      dplyr::group_by(date, Reservoir, Position, variable) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 144/3, NA, observation), # 144 = 24(hrs) * 6(10 minute intervals/hr)
                    Reservoir = 'bvre') |>
      dplyr::rename(datetime = date) |>
      dplyr::select(-n) |>
      mutate(DateTime = lubridate::as_datetime(datetime),
             Reservoir = 'BVR') |>
      filter(variable != 'RDOTemp') ## remove RDO Temp


    # historic_long <- historic_df |>
    #   dplyr::select(any_of(c("Reservoir", "Site", "DateTime")),
    #                 #starts_with("Depth"),
    #                 starts_with("Ther")
    #                 #starts_with("RDO"),
    #                 #starts_with("Lvl")
    #   ) |>
    #   dplyr::rename("Depth_m" = contains("Depth")) |>
    #   tidyr::pivot_longer(-any_of(c("Reservoir", "Site", "DateTime", "Depth_m")),
    #                       names_to = "variable",
    #                       values_to = "observation",
    #                       values_drop_na = FALSE) |>
    #   tidyr::separate_wider_delim(cols = variable,
    #                               names = c("variable","units","Position"),
    #                               delim = "_") |>
    #   dplyr::mutate(Position = as.numeric(Position))

    # merge with the depths offset file
    by <- join_by(between(x$DateTime, y$Offset_start, y$Offset_end), Position, Reservoir)
    long_w_ref  <- full_join(long_historic, depth, by)|>
      drop_na(DateTime) # drop the NAs in DateTime column

    # Make a data frame with just the height above the sediment

    historic_df_1 <- long_w_ref |>
      select(Reservoir,DateTime, variable, Position, observation, Distance_above_sediments)|>
      drop_na(observation) |>
      dplyr::rename(site_id = Reservoir) |>
      mutate(datetime = as.Date(DateTime),
             position_flag = NA) |>
      mutate(z = Distance_above_sediments,
             z_ref = 'height_m',
             site_id = 'bvre') |>
      select(site_id, datetime, variable, z, z_ref, observation, position_flag)


    # bvr_depths <- find_depths(data_file = historic_file,
    #                               depth_offset = "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/BVR_Depth_offsets.csv",
    #                               output <- NULL) |>
    #   dplyr::filter(variable == 'ThermistorTemp') |>
    #   dplyr::select(Reservoir, DateTime, variable, depth_bin, Position)

    # historic_df_1 <- historic_df |>
    #   tidyr::pivot_longer(cols = starts_with('ThermistorTemp'),
    #                       names_to = c('variable','Position'),
    #                       names_sep = '_C_',
    #                       values_to = 'observation') |>
    #   dplyr::mutate(date = lubridate::as_date(DateTime),
    #                 Position = as.numeric(Position)) |>
    #   na.omit() |>
    #   dplyr::left_join(bvr_depths,
    #                    by = c('Position', 'DateTime', 'Reservoir', 'variable')) |>
    #   dplyr::group_by(date, Reservoir, depth_bin) |>
    #   dplyr::summarise(observation = mean(observation, na.rm = T),
    #                    n = dplyr::n(),
    #                    .groups = 'drop') |>
    #   dplyr::mutate(observation = ifelse(n < 144/3, NA, observation), # 144 = 24(hrs) * 6(10 minute intervals/hr)
    #                 Reservoir = 'bvre') |>
    #
    #   dplyr::rename(site_id = Reservoir,
    #                 datetime = date,
    #                 depth = depth_bin) |>
    #   dplyr::select(-n)
  }

  if (historic_df$Reservoir[1] == 'FCR') {
    historic_df_1 <- historic_df |>
      select(-RDO_mgL_5, -RDOsat_percent_5, -RDO_mgL_9, -RDOsat_percent_9) |>
      rename(RDO_mgL_5 = RDO_mgL_5_adjusted, RDOsat_percent_5 = RDOsat_percent_5_adjusted,
             RDO_mgL_9 = RDO_mgL_9_adjusted, RDOsat_percent_9 = RDOsat_percent_9_adjusted) |>
      dplyr::select(any_of(c("Reservoir", "Site", "DateTime")),
                    #starts_with("Depth"),
                    starts_with("Ther"),
                    starts_with("RDO")
                    #starts_with("Lvl")
                    ) |>
      dplyr::rename("Depth_m" = contains("Depth")) |>
      tidyr::pivot_longer(-any_of(c("Reservoir", "Site", "DateTime", "Depth_m")),
                          names_to = "variable",
                          values_to = "observation",
                          values_drop_na = FALSE) |>
      tidyr::separate_wider_delim(cols = variable,
                                  names = c("variable","units","depth"),
                                  delim = "_") |>
      mutate(depth = as.numeric(ifelse(depth == "surface", 0.1, depth))) |>
      select(-units) |>
      dplyr::mutate(Reservoir = ifelse(Reservoir == 'FCR',
                                       'fcre',
                                       ifelse(Reservoir == 'BVR',
                                              'bvre', NA)),
                    date = lubridate::as_date(DateTime)) |>
      na.omit() |>
      dplyr::group_by(date, Reservoir, depth, variable) |>
      dplyr::summarise(observation = mean(observation, na.rm = T),
                       n = dplyr::n(),
                       .groups = 'drop') |>
      dplyr::mutate(observation = ifelse(n < 144/2, NA, observation)) |> # 144 = 24(hrs) * 6(10 minute intervals/hr)
      dplyr::rename(site_id = Reservoir,
                    datetime = date) |>
      dplyr::select(-n) |>
      mutate(z = (max(depth) + 0.5) - depth,
             z_ref = 'height_m') |>
      select(-depth) |>
      filter(variable != 'RDOTemp') ## remove RDO Temp
  }

  message('EDI file ready')

  ## manipulate the data files to match each other


  ## bind the two files using row.bind()
  final_df <- dplyr::bind_rows(historic_df_1, current_df_1) |>
    dplyr::mutate(variable = ifelse(variable == 'RDO', 'DO_mgL_mean', variable),
                  variable = ifelse(variable == 'RDOsat', 'DOsat_percent_mean', variable),
                  variable = ifelse(variable == 'ThermistorTemp', 'Temp_C_mean', variable)) #|>
    #dplyr::mutate(variable = 'Temp_C_mean') #|>
    #rename(depth_m = depth)

  final_df <- final_df |>
    mutate(observation = ifelse(is.nan(observation), NA, observation)) |>
    #drop_na(depth_m) |>
    mutate(duration = 'P1D') |> #,
           #site_id = 'bvre') |>
    select(datetime, site_id, z, z_ref, observation, variable, duration) # rearrange order of columns


  # ## assign depth positions ##
  # ## add position assignment ##
  # water_level_match_df <- waterlevel_file |>
  #   select(datetime, site_id, observation) |>
  #   rename(waterlevel_m = observation)
  #
  # targets_df_with_wl <- final_df |>
  #   left_join(water_level_match_df, by = c('datetime','site_id')) |>
  #   mutate(obs_z_ref = waterlevel_m - z,
  #          position_flag = NA)
  #
  # ## for each date, extract the shallowest depth within 2m of the surface
  # # depth_m is measured from the surface so the shallowest sample is the smallest depth
  # # flagged depths without a sample within 2 m of the surface are left unflagged
  # targets_position_assign_df <- targets_df_with_wl |>
  #   group_by(site_id, datetime, variable) |>
  #   mutate(
  #     position_flag = case_when( ## HEIGHT REFERENCE
  #       z <= 2 & z == min(z) ~ 'bottom_most_2m', # get min height value
  #       obs_z_ref <= 2 & z == max(z) ~ 'top_most_2m' # get max height value
  #     )
  #   ) |>
  #   ungroup() |>
  #   select(datetime, site_id, z, z_ref, observation, variable, duration, position_flag) # rearrange order of columns
  #
  # ## create new variables for "shallowest" and "deepest" -- exclude RDO data (we just assume EXO has shallowest DO)
  # topmost_vars_df <- targets_position_assign_df |>
  #   filter(!(variable %in% c('DO_mgL_mean', 'DOsat_percent_mean')),
  #          position_flag == 'top_most_2m') |>
  #   mutate(variable = glue::glue('{variable}_shallowest'),
  #          z = NA)
  #
  # deepest_vars_df <- targets_position_assign_df |>
  #   filter(!(variable %in% c('DO_mgL_mean', 'DOsat_percent_mean')),
  #          position_flag == 'bottom_most_2m') |>
  #   mutate(variable = glue::glue('{variable}_deepest'),
  #          z = NA)
  #
  # targets_w_new_vars <- targets_position_assign_df |>
  #   bind_rows(topmost_vars_df,deepest_vars_df) |>
  #   select(-position_flag)



  ## return dataframe formatted to match FLARE targets
  return(final_df)
}

#a <- target_generation_ThermistorTemp_C_daily(current_file = latest, historic_file = edi)
