library(vera4castHelpers) #project_specific


library(readr)
library(dplyr)
library(arrow)
library(glue)
library(here)
library(minioclient)
library(tools)
library(fs)
library(stringr)
library(lubridate)

score4cast::ignore_sigpipe()

install_mc()

config <- yaml::read_yaml("challenge_configuration.yaml")

minioclient::mc_alias_set("s3_store",
             config$endpoint,
             Sys.getenv("OSN_KEY"),
             Sys.getenv("OSN_SECRET"))

minioclient::mc_alias_set("submit",
             config$submissions_endpoint,
             Sys.getenv("AWS_ACCESS_KEY_ID_SUBMISSIONS_VERA"),
             Sys.getenv("AWS_SECRET_ACCESS_KEY_SUBMISSIONS_VERA"))

message(paste0("Starting Processing Submissions ", Sys.time()))

local_dir <- file.path(here::here(), "submissions")
unlink(local_dir, recursive = TRUE)
fs::dir_create(local_dir)

message("Downloading forecasts ...")

minioclient::mc_mirror(from = paste0("submit/",config$submissions_bucket), to = local_dir)

submissions <- fs::dir_ls(local_dir, recurse = TRUE, type = "file")
submissions_filenames <- basename(submissions)

if(length(submissions) > 0){

  Sys.unsetenv("AWS_DEFAULT_REGION")
  Sys.unsetenv("AWS_S3_ENDPOINT")
  Sys.setenv(AWS_EC2_METADATA_DISABLED="TRUE")

  duckdbfs::duckdb_secrets(
    endpoint = config$endpoint,
    key = Sys.getenv("OSN_KEY"),
    secret = Sys.getenv("OSN_SECRET"))

  s3 <- arrow::s3_bucket(config$forecasts_bucket,
                         endpoint_override = config$endpoint,
                         access_key = Sys.getenv("OSN_KEY"),
                         secret_key = Sys.getenv("OSN_SECRET"))

  # s3_inventory <- arrow::s3_bucket(dirname(config$inventory_bucket),
  #                                  endpoint_override = config$endpoint,
  #                                  access_key = Sys.getenv("OSN_KEY"),
  #                                  secret_key = Sys.getenv("OSN_SECRET"))
  #
  # s3_inventory$CreateDir(paste0("inventory/catalog/forecasts/project_id=", config$project_id))
  #
  # s3_inventory <- arrow::s3_bucket(paste0(config$inventory_bucket,"/catalog/forecasts/project_id=", config$project_id),
  #                                  endpoint_override = config$endpoint,
  #                                  access_key = Sys.getenv("OSN_KEY"),
  #                                  secret_key = Sys.getenv("OSN_SECRET"))
  #
  # inventory_df <- arrow::open_dataset(s3_inventory) |> dplyr::collect()

  time_stamp <- format(Sys.time(), format = "%Y%m%d%H%M%S")

  sites <- readr::read_csv(config$site_table,show_col_types = FALSE) |>
  select(site_id, latitude, longitude)

  for(i in 1:length(submissions)){

    curr_submission <- basename(submissions[i])
    submission_dir <- dirname(submissions[i])
    print(curr_submission)

    if((tools::file_ext(curr_submission) %in% c("gz", "csv"))){

      valid <- forecast_output_validator(file.path(local_dir, curr_submission))

      if(valid){

        fc <- readr::read_csv(submissions[i], show_col_types = FALSE)

        pub_datetime <- strftime(Sys.time(), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

        if("depth_m" %in% names(fc)){
          fc <- fc |> dplyr::mutate(depth_m = as.numeric(depth_m))
        }

        if(!"duration" %in% names(fc)){
          if(stringr::str_detect(fc$datetime[1], ":")){
            fc <- fc |> dplyr::mutate(duration = "P1H")
          }else{
            fc <- fc |> dplyr::mutate(duration = "P1D")
          }
        }

        fc <- fc |>
          dplyr::mutate(pub_datetime = lubridate::as_datetime(pub_datetime),
                        reference_datetime = lubridate::as_datetime(reference_datetime),
                        reference_date = lubridate::as_date(reference_datetime),
                        parameter = as.character(parameter)) |>
          dplyr::filter(datetime >= reference_datetime)

        print(head(fc))
        s3$CreateDir(paste0("parquet/"))
        # fc |> arrow::write_dataset(s3$path(paste0("parquet")), format = 'parquet',
        #                     partitioning = c("project_id",
        #                                      "duration",
        #                                      "variable",
        #                                      "model_id",
        #                                      "reference_date"))

        ## arrow write has gone nuts... let's update
        fc |> duckdbfs::write_dataset(paste0("s3://", config$forecasts_bucket, "/parquet"),
                                      format = 'parquet',
                                      partitioning = c("project_id",
                                                       "duration",
                                                       "variable",
                                                       "model_id",
                                                       "reference_date"),
                                      options = list("PER_THREAD_OUTPUT false"))

        s3$CreateDir(paste0("summaries"))
        fc |>
          dplyr::summarise(prediction = mean(prediction), .by = dplyr::any_of(c("site_id", "datetime", "reference_datetime", "family", "depth_m", "duration", "model_id",
                                                                                "parameter", "pub_datetime", "reference_date", "variable", "project_id"))) |>
          score4cast::summarize_forecast(extra_groups = c("duration", "project_id", "depth_m")) |>
          dplyr::mutate(reference_date = lubridate::as_date(reference_datetime)) |>
          # arrow::write_dataset(s3$path("summaries"), format = 'parquet',
          #                      partitioning = c("project_id",
          #                                       "duration",
          #                                       "variable",
          #                                       "model_id",
          #                                       "reference_date"))
          duckdbfs::write_dataset(paste0("s3://", config$forecasts_bucket, "/summaries"), format = 'parquet',
                                  partitioning = c("project_id",
                                                   "duration",
                                                   "variable",
                                                   "model_id",
                                                   "reference_date"),
                                  options = list("PER_THREAD_OUTPUT false"))

        submission_timestamp <- paste0(submission_dir,"/T", time_stamp, "_", basename(submissions[i]))
        fs::file_copy(submissions[i], submission_timestamp)
        raw_bucket_object <- paste0("s3_store/",config$forecasts_bucket,"/raw/",basename(submission_timestamp))

        #minioclient::mc_cp(submission_timestamp, dirname(raw_bucket_object))
        minioclient::mc_cp(submission_timestamp, raw_bucket_object)

        if(length(minioclient::mc_ls(raw_bucket_object)) > 0){
          minioclient::mc_rm(file.path("submit",config$submissions_bucket,curr_submission))
        }
      } else {

        submission_timestamp <- paste0(submission_dir,"/T", time_stamp, "_", basename(submissions[i]))
        fs::file_copy(submissions[i], submission_timestamp)
        raw_bucket_object <- paste0("s3_store/",config$forecasts_bucket,"/raw/",basename(submission_timestamp))

        minioclient::mc_cp(submission_timestamp, dirname(raw_bucket_object))

        if(length(minioclient::mc_ls(raw_bucket_object)) > 0){
          minioclient::mc_rm(file.path("submit",config$submissions_bucket,curr_submission))
        }

      }
    }
  }

}

unlink(local_dir, recursive = TRUE)

message(paste0("Completed Processing Submissions ", Sys.time()))

## Call healthcheck
RCurl::url.exists("https://hc-ping.com/29cf0694-f351-4671-a71f-f9805a64ded6", timeout = 5)

