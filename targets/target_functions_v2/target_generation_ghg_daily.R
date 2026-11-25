# Function for generating the targets file for ghg samples
# Author: Adrienne Breef-Pilz
# 24 Aug 2023
#Edit: 16 May 2024 typo in the depth name column change to depth_m

target_generation_ghg_daily <- function(current_data_file, edi_data_file, waterlevel_file){

  ## read in current data file
  # ABP work on creating a perliminary GHG file for the current data
  dt1 <- read_csv(current_data_file)

  # read in historical data file
  # EDI
 # inUrl1 <- edi_data_file
 # infile1 <- tempfile()
 # try(download.file(inUrl1,infile1,method="curl"))
 # if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

  # read in the data file downloaded from EDI
  dt2 <-read_csv(edi_data_file)

  # Filter to what you need
  targets_df<-bind_rows(dt1,dt2)%>% # bind observations together
    filter(Reservoir=="FCR"|Reservoir=="BVR")%>% # select the reservoir you want. These are hard coded in right now
    filter(Site==50)%>% # select the site. Also hard coded in
    select(-Site,-starts_with("Flag"))%>% # get rid of the columns we don't want
    group_by(Reservoir,Depth_m,DateTime)%>%
    summarise_if(is.numeric, mean, na.rm = TRUE)%>% # average if there are reps taken at a depths
    ungroup()%>%
    mutate(Date=as.Date(DateTime))%>%
    group_by(Reservoir,Depth_m,Date)%>% # average if there are more than one sample taken during that day
    summarise_if(is.numeric, mean, na.rm = TRUE)%>%
    ungroup()%>%
    mutate(datetime=ymd_hms(paste0(Date,"","00:00:00")))%>%
    mutate(Reservoir=ifelse(Reservoir=="FCR",'fcre',Reservoir), # change the name to the the reservoir code for FLARE
           Reservoir=ifelse(Reservoir=="BVR",'bvre',Reservoir))%>%
    select(-Date, -Rep)%>%
    rename(site_id=Reservoir, # rename the columns for standard notation
           depth_m=Depth_m)%>%
    pivot_longer(cols=c(CH4_umolL:CO2_umolL), # make the wide data frame into a long one so each observation has a depth
                 names_to='variable',
                 values_to='observation')%>%
    mutate(variable = paste0(variable,'_sample')) |>
    # mutate(duration = 'P1D') |>
    # select(c('datetime', 'site_id', 'depth_m', "observation", 'variable', 'duration')) # rearrange order of columns
    mutate(duration = 'P1D') |>
    rename(z = depth_m) |>
    mutate(z_ref = 'depth_m') |>
    select(datetime, site_id, z, z_ref, observation, variable, duration) # rearrange order of columns


  # water_level_match_df <- waterlevel_file |>
  #   select(datetime, site_id, observation) |>
  #   rename(waterlevel_m = observation)
  #
  # targets_df_with_wl <- targets_df |>
  #   left_join(water_level_match_df, by = c('datetime','site_id')) |>
  #   mutate(obs_z_sed = waterlevel_m - z,
  #          position_flag = NA)
  #
  # ## for each date, extract the shallowest depth within 2m of the surface
  # # depth_m is measured from the surface so the shallowest sample is the smallest depth
  # # flagged depths without a sample within 2 m of the surface are left unflagged
  # targets_position_assign_df <- targets_df_with_wl |>
  #   group_by(site_id, datetime) |>
  #   mutate(
  #     position_flag = case_when(
  #       z <= 2 & z == min(z) ~ 'top_most_2m',
  #       obs_z_sed <= 2 & z == max(z) ~ 'bottom_most_2m'
  #     )
  #   ) |>
  #   ungroup() |>
  #   select(datetime, site_id, z, z_ref, observation, variable, duration, position_flag) # rearrange order of columns




  ## return dataframe formatted to match FLARE targets
  return(targets_df)
}

# Using the function with the EDI address for data
# target_generation_ghg_targets(
#  current_data_file=NULL,
#  edi_data_file= "https://pasta.lternet.edu/package/data/eml/edi/551/7/38d72673295864956cccd6bbba99a1a3")

