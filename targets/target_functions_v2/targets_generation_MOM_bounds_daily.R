#calculates MOM upper and lower bounds
#note that may want to set MOM bounds to 0 when MOM binary = 0 (need to link the two scripts?)
#16Oct2023 HLW

if (!require("pacman"))install.packages("pacman")
pacman::p_load(utils, tidyverse)

# historic_file  <- "https://pasta.lternet.edu/package/data/eml/edi/200/13/27ceda6bc7fdec2e7d79a6e4fe16ffdf"
# current_file <-  "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Raw_CTD/ctd_L1.csv"

targets_generation_daily_MOM_bounds <- function(current_file, historic_file){

  #read in current CTD data
  current_df <- readr::read_csv(current_file, show_col_types = F) |>
    dplyr::filter(Site == 50, Reservoir %in% c("FCR","BVR"), Depth_m > 0) |>
    dplyr::select(c(DateTime,Reservoir,Depth_m,DO_mgL))

  #download CTD data from EDI
  infile1 <- tempfile()
  try(download.file(historic_file,infile1,method="curl"))
  if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

  historic_df <- readr::read_csv(infile1) |>
    dplyr::filter(Site == 50, Reservoir %in% c("FCR","BVR"), Depth_m > 0)

  #select cols needed to generate targets
  historic_df <- historic_df |> dplyr::select(c(DateTime,Reservoir,Depth_m,DO_mgL))

  #combine current and historic ctd data
  DO <- dplyr::bind_rows(current_df, historic_df)

  #change reservoir site name
  DO$Reservoir <- ifelse(DO$Reservoir == "FCR", 'fcre', DO$Reservoir)
  DO$Reservoir <- ifelse(DO$Reservoir == "BVR", 'bvre', DO$Reservoir)

  #change names so df matches targets format
  DO <- DO |> dplyr::rename(datetime = DateTime,
                        site_id = Reservoir,
                        depth_m = Depth_m,
                        observation = DO_mgL)

  #add variable column
  DO$variable <- "DO_mgL"

  #order DO by date, site, then depth
  DO <- DO |> dplyr::arrange(datetime, site_id, depth_m)

  #now calculate MOM bounds
  MOM_bounds <- DO |> dplyr::group_by(datetime, site_id, variable) |>
    dplyr::summarise(MOM_min_sample =
                       round(first(depth_m[observation < 2]),1),
                     MOM_max_sample =
                       round(last(depth_m[observation < 2]),1))

  #wide to long
  MOM_bounds_long <- MOM_bounds |> select(-variable) |>
    tidyr::pivot_longer(cols = MOM_max_sample:
                          MOM_min_sample, names_to = "variable")

  #rename columns to match targets file
  MOM_bounds_long <- MOM_bounds_long |>
    rename(observation = value)

  #Depth is na bc full water column variable
  MOM_bounds_long$depth_m <- NA
  MOM_bounds_long$duration <- 'P1D'
  MOM_bounds_long$project_id <- 'vera4cast'

  mom_dup_check <- MOM_bounds_long  %>%
    dplyr::group_by(datetime, site_id, variable) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1)

  if (nrow(mom_dup_check) == 0){
    return(MOM_bounds_long)
  }else{
    print('MOM binary duplicates found...please fix')
    stop()
  }


  # return dataframe formatted to match FLARE targets
  r#eturn(MOM_binary)
}


#a <- targets_generation_daily_MOM(current_file = current_file, historic_file = historic_file)
