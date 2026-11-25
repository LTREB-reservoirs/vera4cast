# 
# 
# cleaning_function <- function(file_1, file_2){
#   
#   ## read in current data file 
#   # Github, Googlesheet, etc. 
#   
#   # read in historical data file 
#   # EDI
#   
#   ## manipulate the data files to match each other 
#     # secchi: match current data file and select only secchi data in EDI
#     # temperature: in correct format as EDI 
#     # swimmibility: select the sfc water and air temp columns and just use those
#     # Phtyos/MOMs: select columns that you need 
#       #MOM: EDI 
#       #Phytos: Taken from catwalk file
#   
#   ## bind the two files using row.bind()
#   
#   ## Match data to flare targets file 
#     # Use pivot_longer to create a long-format table
#     # for time specific - use midnight UTC values for daily 
#     # for hourly 
#   
#   ## return dataframe formatted to match FLARE targets
# }
# 
# 
# 
# 
# # RESOURCES 
# 
# # FCRE catwalk L1 file 
# # https://github.com/FLARE-forecast/FCRE-data/blob/fcre-catwalk-data-qaqc/fcre-waterquality_L1.csv
# 
# # targets file 
# targets_col_names <- c('datetime', 'site_id', 'depth', "observation", 'variable')
# 
# # READ IN EDI FCRE Catwalk file 
# 
# inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f" 
# infile1 <- tempfile()
# try(download.file(inUrl1,infile1,method="curl"))
# if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
# dt1 <-read_csv(infile1)
