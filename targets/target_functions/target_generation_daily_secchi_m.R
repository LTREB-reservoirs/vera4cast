#test_func <- function(supply argumentf1, f2) {read f1, read f2 -then- rbind() -then- return (targets_df)}
#KKH
#Secchi target generation script

#Install the required googlesheets4 package
library(tidyverse)
library(dplyr)
# install.packages('googlesheets4')
#Load the library
library(googlesheets4)
#library(janitor)
library(tibble)


target_generation_daily_secchi_m <- function(current, edi){
  # read in current file from github
  secchi_current_1 <- read_csv(current)

  #read in the EDI data
  # inUrl1  <- edi
  # infile1 <- tempfile()
  # try(download.file(inUrl1,infile1,method="curl"))
  # if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
  edi_df <-read_csv(edi)
#subset to only one spot in the reservoir (deep hole)
  edi_1 <- edi_df %>% subset(Site == "50")

  #site ids: BVR > bvre

  #should fit the format: targets_col_names <- c('datetime', 'site_id', 'depth', "observation", 'variable')
  #Reservoir = site_id, will need to change from upper to lower and add an e on the end of all the reservoirs so
  #BVR>bvre, FCR>fcre
  #secchi doesn't have an associated depth, secchi_m is an observation, need to create a depth column
  #need to create a variable column: add a column with every row = secchi_m
  secchi_edi <- subset(edi_1, select = c("DateTime", "Reservoir", "Secchi_m"))

  #should fit the format: targets_col_names <- c('datetime', 'site_id', 'depth', "observation", 'variable')
  #Reservoir = site_id, will need to change from upper to lower and add an e on the end of all the reservoirs so
  #BVR>bvre, FCR>fcre
  #secchi doesn't have an associated depth, Secchi_m is an observation, need to create a depth column
  #need to create a variable column: add a column with every row = secchi_m
  secchi_current_2 <- secchi_current_1 %>% subset(select = c("DateTime", "Reservoir", "Secchi_m"))
  secchi_current_2$DateTime <- as.POSIXct(secchi_current_2$DateTime)

  ## bind the two files using bind_rows()
  comb_secchi <- bind_rows(secchi_edi, secchi_current_2)

  #reformat the reservoir column
  #all lowercase
  #rename to site_id
  #mutate all BVR > bvre, FCR>fcre

  #change colnames to lowercase, add columns, rename and reorder columns to match scheme
  comb_secchi_1 <- comb_secchi %>%
   #clean_names %>%
    rename("site_id" = "Reservoir")%>%
    rename("datetime" = "DateTime")%>%
    rename("observation" = "Secchi_m") %>%
    add_column(variable = "Secchi_m_sample") %>%
    add_column(depth = NA) %>%
    subset(select = c('datetime', 'site_id', 'depth', "observation", 'variable'))

  #rename all of the site ids
  comb_secchi_2 <- comb_secchi_1 %>%
    mutate(site_id = replace(site_id, site_id == "BVR", "bvre")) %>%
    mutate(site_id = replace(site_id, site_id == "FCR", "fcre")) %>%
    mutate(site_id = replace(site_id, site_id == "CCR", "ccre")) %>%
    mutate(site_id = replace(site_id, site_id == "GWR", "gwre")) %>%
    mutate(site_id = replace(site_id, site_id == "SHR", "shre"))

  comb_secchi_4 <- comb_secchi_2 %>%
    distinct(datetime, observation, .keep_all=TRUE)

  comb_secchi_5 <- comb_secchi_4 %>%
    mutate(datetime = lubridate::as_date(datetime)) |>
    group_by(datetime, site_id, depth, variable) %>%
    summarise(across(c(observation),mean))

  #to check site ids
  # site_id <- unique(comb_secchi_2$site_id)



  return(comb_secchi_5)
}

#google_doc = 'https://docs.google.com/spreadsheets/d/1fvM0fDRliuthicQWZT7c9RYErikI5DwrzbOC7TCoMGI/edit#gid=1172894977'
# secchi <- target_generation_daily_secchi_m(current = "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Secchi/secchi_L1.csv",
#                                            edi = "https://pasta.lternet.edu/package/data/eml/edi/198/11/81f396b3e910d3359907b7264e689052")


