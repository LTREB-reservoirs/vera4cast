# Function for generating the targets file for metals samples
# Author: Adrienne Breef-Pilz
# 24 Aug 2023

target_generation_metals_daily <- function(current_data_file, edi_data_file){

  ## read in current data file
  # Make it flexible to read in the file or null when there is no file

  if(is.null(current_data_file)){

  dt1 <-current_data_file
  }else{
    dt1 <- read_csv(current_data_file)
   }

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
    mutate(Date=as.Date(DateTime))%>%
    group_by(Reservoir,Depth_m,Date)%>% # average if there are more than one sample taken during that day
    summarise_if(is.numeric, mean, na.rm = TRUE)%>%
    ungroup()%>%
    mutate(datetime=ymd_hms(paste0(Date,"","00:00:00")))%>%
    mutate(Reservoir=ifelse(Reservoir=="FCR",'fcre',Reservoir), # change the name to the the reservoir code for FLARE
           Reservoir=ifelse(Reservoir=="BVR",'bvre',Reservoir))%>%
    select(-Date)%>%
    rename(site_id=Reservoir, # rename the columns for standard notation
           depth_m=Depth_m)%>%
    pivot_longer(cols=c(TFe_mgL:SMn_mgL), # make the wide data frame into a long one so each observation has a depth
                 names_to='variable',
                 values_to='observation')%>%
    select(c('datetime', 'site_id', 'depth_m', "observation", 'variable')) # rearrange order of columns


  targets_df$variable <- paste0(targets_df$variable,'_sample')
  targets_df$duration <- 'P1D'
  ## return dataframe formatted to match FLARE targets
  return(targets_df)
}

# Using the function with the EDI address for data
 # target_generation_metals_daily(
 #  current_data_file=NULL,
 #  edi_data_file="https://pasta.lternet.edu/package/data/eml/edi/455/7/e71d70cac1650610e6a3fbbf7928007f")

