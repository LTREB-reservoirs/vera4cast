#calculates MOM_binary
#MOM is calculated as the layer where DO is < 2mg/L (for now)
#17Aug2023 HLW

if (!require("pacman"))install.packages("pacman")
pacman::p_load(utils, tidyverse)

# historic_file  <- "https://pasta.lternet.edu/package/data/eml/edi/200/13/27ceda6bc7fdec2e7d79a6e4fe16ffdf"
# current_file <-  "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Raw_CTD/ctd_L1.csv"

targets_generation_daily_MOM <- function(current_file, historic_file){

  #read in current CTD data
  current_df <- readr::read_csv(current_file, show_col_types = F) |>
    dplyr::filter(Site == 50, Reservoir %in% c("FCR","BVR"), Depth_m > 0) |>
    dplyr::select(c(DateTime,Reservoir,Depth_m,DO_mgL))

  #download CTD data from EDI
  #infile1 <- tempfile()
  #try(download.file(historic_file,infile1,method="curl"))
  #if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

  historic_df <- readr::read_csv(historic_file) |>
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

  #now calculate MOM binary
  MOM_binary <- DO |> dplyr::group_by(datetime, site_id, variable) |>
    dplyr::mutate(MOM_binary = ifelse((observation < 2 & depth_m >= 2 & depth_m <= 8), 1, 0)) |>
    dplyr::summarise(MOM_binary = max(MOM_binary), .groups = 'drop') |> dplyr::select(-variable)

  #rename columns to match targets file
  MOM_binary <- MOM_binary |>
    dplyr::rename(observation = MOM_binary)

  MOM_binary$variable <- "MOM_binary_sample"

  #Depth is na bc full water column variable
  MOM_binary$depth_m <- NA
  MOM_binary$duration <- 'P1D'
  MOM_binary$project_id <- 'vera4cast'

  MOM_binary <- MOM_binary |>
    mutate(z = NA,
           z_ref = 'depth_m') |>
    select(c('datetime', 'site_id', 'z', 'z_ref', "observation", 'variable', 'duration'))

  mom_dup_check <- MOM_binary  %>%
    dplyr::group_by(datetime, site_id, variable) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(n > 1)

  if (nrow(mom_dup_check) == 0){
    return(MOM_binary)
  }else{
    print('MOM binary duplicates found...please fix')
    stop()
  }

  # return dataframe formatted to match FLARE targets
  #return(MOM_binary)

  }

#a <- targets_generation_daily_MOM(current_file = current_file, historic_file = historic_file)

