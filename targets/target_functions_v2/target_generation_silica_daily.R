# Function for generating the targets file for silica data
# Author: Adrienne Breef-Pilz
# Written: 15 April 2025


target_generation_silica_daily <- function(current_data_file, historic_data_file){

  ## read in current data file
  # Right now there is no current silica file to read in
  dt1 <-current_data_file

  ## read in historical file from EDO
  dt2 <-read_csv(historic_data_file)

  ## manipulate the data files to match each other and the correct form for the targets file for VERA

  ## bind the two files using row_bind()

  targets_df <-bind_rows(dt1,dt2)%>%
    filter(Reservoir=="FCR"|Reservoir=="BVR")%>%
    filter(Site==50)%>%
    select(-Site,-starts_with("Flag"))%>% # get rid of the columns we don't want
    group_by(Reservoir,Depth_m,DateTime)%>%
    summarise_if(is.numeric, mean, na.rm = TRUE)%>% # average if there are reps taken at a depths
    ungroup()|>
    mutate(Date=as.Date(DateTime))%>%
    group_by(Reservoir,Depth_m,Date)%>% # average if there are more than one sample taken during that day
    summarise_if(is.numeric, mean, na.rm = TRUE)%>%
    ungroup()%>%
    mutate(datetime=ymd_hms(paste0(Date,"","00:00:00")))%>%
    mutate(Reservoir=ifelse(Reservoir=="FCR",'fcre',Reservoir), # change the name to the the reservoir code for FLARE
           Reservoir=ifelse(Reservoir=="BVR",'bvre',Reservoir))%>%
    select(-Date)%>%
    rename(site_id=Reservoir,
           depth_m=Depth_m)|>
    pivot_longer(cols=DRSI_mgL,
                 names_to='variable',
                 values_to='observation')%>%
    mutate(variable = paste0(variable,'_sample')) |>
    mutate(duration = 'P1D',
           z = depth_m,
           z_ref = 'depth_m') |>
    select(c('datetime', 'site_id', 'z', 'z_ref', "observation", 'variable', 'duration')) # rearrange order of columns



  ## return dataframe formatted to match FLARE targets
  return(targets_df)
}

# Using the function with the EDI address for data
# target_generation_silica_daily(
#  current_data_file=NULL,
#  historic_data_file="https://pasta.lternet.edu/package/data/eml/edi/542/1/791ec9ca0f1cb9361fa6a03fae8dfc95")

