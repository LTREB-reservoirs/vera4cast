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

# Since 2026-09-08 the NRP key has had ListBucket only: both GetObject and
# DeleteObject return "Insufficient permissions". The bucket's own policy,
# however, grants GetObject-by-ACL and DeleteObject to Principal:* , so an
# unauthenticated client can do both - it just cannot list. So we list with the
# authenticated alias above and do the reads and deletes through this anonymous
# one. Drop it once NRP restores the key's permissions.
minioclient::mc_alias_set("submit_read", config$submissions_endpoint, "", "")

message(paste0("Starting Processing Submissions ", Sys.time()))

local_dir <- file.path(here::here(), "submissions")
unlink(local_dir, recursive = TRUE)
fs::dir_create(local_dir)

# Removal is now best-effort (see mark_processed below), so we also keep our own
# record of what has been handled, on OSN where we have write access, and filter
# those out before downloading. That keeps the pipeline correct even if deleting
# from the submissions bucket stops working again.
manifest_object <- paste0("s3_store/", config$processed_submissions)
#manifest_local <- file.path(tempdir(), "processed_submissions.csv")
manifest_local <- file.path(local_dir, "processed_submissions.csv")

processed <- tryCatch({
  minioclient::mc_cp(manifest_object, manifest_local)
  manifest <- readr::read_csv(manifest_local, show_col_types = FALSE)
  if ("key" %in% names(manifest)) as.character(manifest$key) else character(0)
}, error = function(e) {
  message("No processed-submissions record found; starting a new one.")
  character(0)
})

marked <- 0L   # submissions handled in this run
removed <- 0L  # objects deleted from the bucket: newly handled plus backlog

mark_processed <- function(key) {
  # Record first, remove second. If the removal succeeds but the record was
  # never written we would reprocess a submission that no longer exists; this
  # order fails safe in the other direction instead.
  processed <<- unique(c(processed, key))
  marked <<- marked + 1L
  readr::write_csv(data.frame(key = processed), manifest_local)
  minioclient::mc_cp(manifest_local, manifest_object)

  remove_from_bucket(key)
}

# The bucket policy grants DeleteObject to Principal:* , so the anonymous alias
# can clear the bucket even though our key cannot. Best effort only: callers
# have already recorded the submission, so a failure here costs space on the
# bucket, not correctness, and must not abort the run.
remove_from_bucket <- function(key) {
  tryCatch({
    minioclient::mc_rm(paste0("submit_read/", config$submissions_bucket, "/", key))
    removed <<- removed + 1L
    TRUE
  }, error = function(e) {
    warning("could not remove ", key, " from the submissions bucket: ",
            conditionMessage(e), call. = FALSE)
    FALSE
  })
}

message("Downloading forecasts ...")


# Replaces mc_mirror(), which cannot work while list and read live with
# different identities. Keys are listed recursively and copied one at a time,
# preserving the bucket's nested layout for the dir_ls() walk below. A single
# unreadable object is reported and skipped rather than aborting the batch.
all_keys <- minioclient::mc_ls(paste0("submit/", config$submissions_bucket),
                               recursive = TRUE,
                               details = TRUE)$key

# Same exclusions as the dir_ls() filters below, applied before downloading
# rather than after. These submissions are never processed, so they never enter
# the manifest, and fetching them every run means re-downloading the bulk of the
# bucket every two hours for nothing.
wanted <- all_keys[stringr::str_detect(all_keys, "2023", negate = TRUE) &
                     stringr::str_detect(all_keys, "usgsrc4cast", negate = TRUE)]
submission_keys <- setdiff(wanted, processed)

message(sprintf("%d objects in bucket, %d excluded, %d already processed, %d to download",
                length(all_keys),
                length(all_keys) - length(wanted),
                length(wanted) - length(submission_keys),
                length(submission_keys)))


# Submissions already recorded as handled but still sitting in the bucket,
# because removal failed or was never attempted on an earlier run. Clearing them
# here is what actually drains the backlog: they are filtered out of
# submission_keys above, so they never reach mark_processed() again.
stale <- intersect(all_keys, processed)
if (length(stale) > 0) {
  message(sprintf("Clearing %d already-processed submission(s) left in the bucket",
                  length(stale)))
  for (key in stale) remove_from_bucket(key)
}

failed <- character(0)
for (key in submission_keys) {
  dest <- file.path(local_dir, key)
  fs::dir_create(dirname(dest))
  copied <- tryCatch({
    minioclient::mc_cp(paste0("submit_read/", config$submissions_bucket, "/", key), dest)
    TRUE
  }, error = function(e) {
    warning("could not download ", key, ": ", conditionMessage(e), call. = FALSE)
    FALSE
  })
  if (!isTRUE(copied)) failed <- c(failed, key)
}

message(sprintf("Downloaded %d of %d submissions",
                length(submission_keys) - length(failed),
                length(submission_keys)))
if (length(failed) > 0) {
  message("Skipped unreadable submissions: ", paste(failed, collapse = ", "))
}

submissions <- fs::dir_ls(local_dir, recurse = TRUE, type = "file")
submissions <- submissions[stringr::str_detect(submissions, "2023", negate = TRUE)]
submissions <- submissions[stringr::str_detect(submissions, "usgsrc4cast", negate = TRUE)]

submissions_filenames <- basename(submissions)

## removing this for now because mc_mirror is no longer an option
#minioclient::mc_mirror(from = paste0("submit/",config$submissions_bucket), to = local_dir)

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

  time_stamp <- format(Sys.time(), format = "%Y%m%d%H%M%S")

  sites <- readr::read_csv(config$catalog_config$site_metadata_url,show_col_types = FALSE) |>
  select(site_id, latitude, longitude)

  for(i in 1:length(submissions)){

    curr_submission <- basename(submissions[i])
    submission_dir <- dirname(submissions[i])
    curr_key <- as.character(fs::path_rel(submissions[i], local_dir))
    print(curr_submission)

    # NEON USES THIS SELECTION CRITERIA (MORE THAN WE NEED FOR VERA BUT KEEPING FOR REFERENCE)
    # curr_submission <- basename(submissions[i])
    # # Bucket-relative path, not the basename: 33 of the keys sit under a prefix,
    # # and the manifest has to match what mc_ls() returns for them to be skipped.
    # curr_key <- as.character(fs::path_rel(submissions[i], local_dir))
    # theme <-  stringr::str_split(curr_submission, "-")[[1]][1]
    # file_name_model_id <-  stringr::str_split(tools::file_path_sans_ext(tools::file_path_sans_ext(curr_submission)), "-")[[1]][5]
    # file_name_reference_datetime <- lubridate::as_datetime(paste0(stringr::str_split(curr_submission, "-")[[1]][2:4], collapse = "-"))
    # submission_dir <- dirname(submissions[i])
    # print(curr_submission)


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
          mark_processed(curr_key)
          #minioclient::mc_rm(file.path("submit",config$submissions_bucket,curr_submission))
        }

        print("finishing submission processing")

        rm(fc)
        gc()

      } else {

        submission_timestamp <- paste0(submission_dir,"/T", time_stamp, "_", basename(submissions[i]))
        fs::file_copy(submissions[i], submission_timestamp)
        raw_bucket_object <- paste0("s3_store/",config$forecasts_bucket,"/raw/",basename(submission_timestamp))

        minioclient::mc_cp(submission_timestamp, dirname(raw_bucket_object))

        if(length(minioclient::mc_ls(raw_bucket_object)) > 0){
          mark_processed(curr_key)
          #minioclient::mc_rm(file.path("submit",config$submissions_bucket,curr_submission))
        }

      }
    }
  }

}

unlink(local_dir, recursive = TRUE)

message(sprintf("Processed %d submission(s) this run, removed %d object(s) from the bucket; %d recorded in total",
                marked, removed, length(processed)))
if (marked > 0L && removed == 0L) {
  message("Nothing could be removed: the bucket will keep growing until either ",
          "the anonymous delete or the NRP key's DeleteObject permission works.")
}

message(paste0("Completed Processing Submissions ", Sys.time()))

## Call healthcheck
RCurl::url.exists("https://hc-ping.com/29cf0694-f351-4671-a71f-f9805a64ded6", timeout = 5)

