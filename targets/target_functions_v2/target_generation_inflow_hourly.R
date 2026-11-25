# Function for generating the targets file for hourly inflow observations
# Author: Adrienne Breef-Pilz
# 24 Aug 2023

target_generation_inflow_hourly <- function(current_data_file, edi_data_file){
  
  ## read in current QAQCed data file from GitHub
  
  dt1 <-read_csv(current_data_file)
  
  # read in historical data file 
  # EDI
  inUrl1 <- edi_data_file
  infile1 <- tempfile()
  try(download.file(inUrl1,infile1,method="curl"))
  if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
  
  # read in the data file downloaded from EDI
  dt2 <-read_csv(infile1) 
  
  ## This is where the transfermation happens
  targets_df<-bind_rows(dt1,dt2)%>% # bind observations together
    filter(Reservoir=="FCR")%>% # select the reservoir you want. These are hard coded in right now
    filter(Site==100)%>% # select the site. Also hard coded in. There should only be one site
    select(Reservoir, DateTime,WVWA_Flow_cms, WVWA_Temp_C, VT_Flow_cms, VT_Temp_C)%>% # select only the columns we want
    pivot_longer(cols=c(WVWA_Flow_cms:VT_Temp_C), # make the wide data frame into a long one so each observation has a depth
                 names_to='variable',
                 values_to='observation')%>%
    # rename the observations in the variable column to get rid of the specific sensor name
    mutate(variable=ifelse(grepl('Flow', variable), "Flow_cms", ifelse(grepl('Temp', variable), "Inflow_Temp_cms", NA)))%>%
    mutate(Date=as.Date(DateTime),  # Gets column of just the date
           Hour = lubridate::hour(DateTime))%>% # Get columns for averaging across the hour
    group_by(Reservoir,variable,Date, Hour)%>% # average if there are more than one sample taken during that day
    summarise_if(is.numeric, mean, na.rm = TRUE)%>%
    ungroup()%>%
    mutate(datetime = ymd_hms(paste0(Date," ", Hour,":00:00")))%>%
    #mutate(datetime=ymd_hms(paste0(Date,"","00:00:00")))%>%
    mutate(Reservoir=ifelse(Reservoir=="FCR",'fcre',Reservoir))%>% # change the name to the the reservoir code for FLARE
    select(-Date, -Hour)%>%
    rename(site_id=Reservoir)%>% # rename the columns for standard notation)
      mutate(depth=NA)%>% # right now there are no depth readings with these but can add one
    select(c('datetime', 'site_id', 'depth', "observation", 'variable')) # rearrange order of columns
  
  
  
  ## return dataframe formatted to match FLARE targets
  return(targets_df)
}

# Using the function with the EDI address for data
# a <- target_generation_inflow_hourly(
#  current_data_file="https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-weir-data-qaqc/FCRWeir_L1.csv",
#  edi_data_file="https://pasta.lternet.edu/package/data/eml/edi/202/10/c065ff822e73c747f378efe47f5af12b")

