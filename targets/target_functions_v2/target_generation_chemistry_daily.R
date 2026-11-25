# Function for generating the targets file for chemistry
# Author: Adrienne Breef-Pilz
# Written: 17 Aug 2023
# Edited: 15 Apr 2025 - added in DIC, DN and DC


target_generation_chemistry_daily <- function(current_data_file, historic_data_file, waterlevel_file){

  ## read in current data file
  # Right now there is no current Chemistry file to read in
  dt1 <-current_data_file

  # read in historical data file
  # # EDI
  # inUrl1 <- historic_data_file
  # infile1 <- tempfile()
  # try(download.file(inUrl1,infile1,method="curl"))
  # if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

  dt2 <-read_csv(historic_data_file)

  ## manipulate the data files to match each other
  # secchi: match current data file and select only secchi data in EDI
  # temperature: in correct format as EDI
  # swimmibility: select the sfc water and air temp columns and just use those
  # Phtyos/MOMs: select columns that you need
  #MOM: EDI
  #Phytos: Taken from catwalk file

  ## bind the two files using row.bind()

    targets_df<-bind_rows(dt1,dt2)%>%
      filter(Reservoir=="FCR"|Reservoir=="BVR")%>%
      filter(Site==50)%>%
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
      rename(site_id=Reservoir,
             depth_m=Depth_m)|>
      pivot_longer(cols=c(TN_ugL:DN_mgL),
                   names_to='variable',
                   values_to='observation')%>%
      mutate(variable = paste0(variable,'_sample')) |>
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


    ## for each date, extract the deepest depth within 2m of the sediments


  ## return dataframe formatted to match FLARE targets
    return(targets_df)
}

# Using the function with the EDI address for data
# target_generation_chemistry_daily(
#  current_data_file=NULL,
#  historic_data_file="https://pasta.lternet.edu/package/data/eml/edi/199/11/509f39850b6f95628d10889d66885b76")

