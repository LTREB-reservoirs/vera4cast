library(tidyverse)
library(RCurl)

## set destination s3 paths
s3 <- arrow::s3_bucket("bio230121-bucket01", endpoint_override = "amnh1.osn.mghpcc.org")
#s3$CreateDir("vera4cast/targets/duration=P1D")
#s3$CreateDir("vera4cast/targets/duration=PT1H")

duckdbfs::duckdb_secrets(
  endpoint = 'amnh1.osn.mghpcc.org',
  key = Sys.getenv("AWS_ACCESS_KEY_ID"),
  secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"))

s3_daily <- arrow::s3_bucket("bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D", endpoint_override = "amnh1.osn.mghpcc.org")
s3_hourly <- arrow::s3_bucket("bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=PT1H", endpoint_override = "amnh1.osn.mghpcc.org")

column_names <- c("project_id", "site_id","datetime","duration", "depth_m","variable","observation")

## INFLOWS
print('Inflows')
source('targets/target_functions/inflow/target_generation_inflows.R')

current_inflow <- 'https://raw.githubusercontent.com/FLARE-forecast/FCRE-data/fcre-weir-data-qaqc/FCRWeir_L1.csv'

historic_inflow <- "https://pasta.lternet.edu/package/data/eml/edi/202/14/91713d7c24533b742efa21cb3acf8f8a"

historic_silica <- 'https://pasta.lternet.edu/package/data/eml/edi/542/1/791ec9ca0f1cb9361fa6a03fae8dfc95'

historic_nutrients <- "https://pasta.lternet.edu/package/data/eml/edi/199/13/3f09a3d23b7b5dd32ed7d28e9bc1b081"

historic_ghg <- "https://pasta.lternet.edu/package/data/eml/edi/551/10/ee8e65a2a380c9fb63bbf7f4a542c895"

current_ghg <-  "https://raw.githubusercontent.com/CareyLabVT/Reservoirs/master/Data/DataNotYetUploadedToEDI/Raw_GHG/L1_manual_GHG.csv"


inflow_daily <- target_generation_inflows(historic_inflow = historic_inflow,
                                          current_inflow = current_inflow,
                                          historic_nutrients = historic_nutrients,
                                          historic_silica = historic_silica,
                                          historic_ghg = historic_ghg,
                                          current_ghg = current_ghg)

inflow_daily <- inflow_daily |> select(column_names)

#arrow::write_csv_arrow(inflow_daily, sink = s3_daily$path("daily-inflow-targets.csv.gz"))
duckdbfs::write_dataset(inflow_daily,
                        path = "s3://bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz",
                        format = 'csv')
