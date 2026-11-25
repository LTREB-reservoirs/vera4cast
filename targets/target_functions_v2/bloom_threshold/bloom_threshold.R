# # Title: Generate targets and calculate bloom threshold for EXO chl-a
# # Author: Mary Lofton
# # Date: 26JUL23
# 
# # load packages
# library(tidyverse)
# library(lubridate)
# 
# # download data ----
# 
# #1. FCR catwalk
# inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f" 
# infile1 <- paste0("./Desktop/bloom_threshold/FCR_Catwalk_EDI_2018_2022.csv")
# try(download.file(inUrl1,infile1, method = "curl"))
# if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
# 
# #2. BVR catwalk
# inUrl1  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.725.3&entityid=a9a7ff6fe8dc20f7a8f89447d4dc2038" 
# infile1 <- paste0("./Desktop/bloom_threshold/BVR_platform_data_2020_2022.csv")
# try(download.file(inUrl1,infile1,method="curl"))
# if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
# 
# 
# 
# # read in data ----
# 
# filepath1 = "./R/bloom_threshold/FCR_Catwalk_EDI_2018_2022.csv"
# filepath2 = "./R/bloom_threshold/BVR_platform_data_2020_2022.csv"
# 
# # calculate bloom thresholds ----
# 
# exo1 <- read_csv(filepath1) %>%
#   filter(Flag_EXOChla_ugL_1 == 0 & year(DateTime) %in% c(2018:2022)) %>%
#   select(DateTime, EXOChla_ugL_1) %>%
#   mutate(Date = date(DateTime),
#          Hour = hour(DateTime)) %>%
#   group_by(Date) %>%
#   summarize(Chla_ugL_mean = mean(EXOChla_ugL_1, na.rm = TRUE),
#             Chla_ugL_sd = sd(EXOChla_ugL_1, na.rm = TRUE))
# 
# exo2 <- read_csv(filepath1) %>%
#   filter(Flag_EXOChla_ugL_1 == 0 & year(DateTime) %in% c(2018:2022)) %>%
#   select(DateTime, EXOChla_ugL_1) %>%
#   mutate(Date = date(DateTime),
#          Hour = hour(DateTime)) %>%
#   group_by(Date, Hour) %>%
#   summarize(Chla_ugL_mean = mean(EXOChla_ugL_1, na.rm = TRUE),
#             Chla_ugL_sd = sd(EXOChla_ugL_1, na.rm = TRUE)) %>%
#   group_by(Date) %>%
#   summarize(Chla_ugL_mean = mean(Chla_ugL_mean, na.rm = TRUE),
#             Chla_ugL_sd = sd(Chla_ugL_sd, na.rm = TRUE))
# 
# # note differences for days when observations are missing
# 
# hist_mean <- mean(exo2$Chla_ugL_mean)
# hist_sd <- sd(exo2$Chla_ugL_mean)
# 
# bloom_threshold <- hist_mean + 2*hist_sd
# 
# platform1 <- read_csv(filepath2) %>%
#   filter(Flag_EXOChla_ugL_1.5 == 0 & year(DateTime) %in% c(2020:2022)) %>%
#   select(DateTime, EXOChla_ugL_1.5) %>%
#   mutate(Date = date(DateTime),
#          Hour = hour(DateTime)) %>%
#   group_by(Date, Hour) %>%
#   summarize(Chla_ugL_mean = mean(EXOChla_ugL_1.5, na.rm = TRUE),
#             Chla_ugL_sd = sd(EXOChla_ugL_1.5, na.rm = TRUE)) %>%
#   group_by(Date) %>%
#   summarize(Chla_ugL_mean = mean(Chla_ugL_mean, na.rm = TRUE),
#             Chla_ugL_sd = sd(Chla_ugL_sd, na.rm = TRUE))
# 
# hist_mean <- mean(platform1$Chla_ugL_mean)
# hist_sd <- sd(platform1$Chla_ugL_mean)
# 
# bloom_threshold <- hist_mean + 2*hist_sd
