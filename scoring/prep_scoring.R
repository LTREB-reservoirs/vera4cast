#devtools::install_version("duckdb", "1.2.2")
remotes::install_github('cboettig/duckdbfs', upgrade = 'never')

score4cast::ignore_sigpipe()

library(dplyr)
library(duckdbfs)
library(progress)
library(bench)

library(DBI)
con <- duckdbfs::cached_connection(tempfile())
#DBI::dbExecute(con, "SET THREADS=64;")

library(minioclient)

install_mc()
mc_alias_set("osn", "amnh1.osn.mghpcc.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))


remove_dir <- function(path) {
  tryCatch(
    {
      minioclient::mc_rm(path, recursive = TRUE)
      message('directory successfully removed...')
    },
    error = function(cond) {
      message("The removal directory could not be found...")
      message("Here's the original error message:")
      message(conditionMessage(cond))
      # Choose a return value in case of error
      NA
    },
    warning = function(cond) {
      message('Deleting the directory caused a warning...')
      message("Here's the original warning message:")
      message(conditionMessage(cond))
      # Choose a return value in case of warning
      NULL
    },
    finally = {
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally = <expression>'
      message("Finished the delete portion...")
    }
  )
}

remove_dir("osn/bio230121-bucket01/vera4cast/tmp/score_me")
remove_dir("osn/bio230121-bucket01/vera4cast/tmp/forecasts")
remove_dir("osn/bio230121-bucket01/vera4cast/tmp/targets")
remove_dir("osn/bio230121-bucket01/vera4cast/tmp/scores")


#fs::dir_create("new_scores")

project <- "vera4cast"
cut_off_date <- Sys.Date() - lubridate::dmonths(1)
rescore <- FALSE
obs_key_cols <- c("project_id", "site_id", "datetime", "duration", "variable", "depth_m")
score_key_cols <- c(obs_key_cols, "model_id", "family", "reference_datetime", "depth_m")


duckdbfs::duckdb_secrets(endpoint = "amnh1.osn.mghpcc.org",
                         key = Sys.getenv("OSN_KEY"),
                         secret = Sys.getenv("OSN_SECRET"),
                         bucket = "bio230121-bucket01/vera4cast")


print('read targets files...')

target_files <-
  c("https://amnh1.osn.mghpcc.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz",
    "https://amnh1.osn.mghpcc.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz",
    "https://amnh1.osn.mghpcc.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-met-targets.csv.gz"
  )

### Access the targets, forecasts, and scores subsets
targets <-
  open_dataset(target_files,
               recursive = FALSE,
               format = "csv",
               #parser_options = list(nullstr = "NA"),
               anonymous = TRUE,

  ) |>
  mutate(
    depth_m = as.numeric(depth_m),
    depth_m = ifelse(is.na(depth_m), -999999, depth_m),
    datetime = lubridate::as_datetime(as.Date(datetime))) |> ### THIS SHOULD FIX THE DATETIME INNER-JOIN ISSUE
  filter(project_id == {project},
         datetime > {cut_off_date},
         !is.na(observation)
  ) |>
  distinct(site_id, datetime, duration, depth_m, variable, .keep_all = T)


# No point in trying to score any forecasts still in future (relative to last observed)
# (pull forces eval, can take a minute)
last_observed_date <- targets |>
  select(datetime) |>
  distinct() |>
  filter(datetime == max(datetime)) |> pull(datetime)

print('read forecasts for scoring...')
# Omit scoring of daily forecasts that have a horizon > 35
forecasts <-
  open_dataset("s3://bio230121-bucket01/vera4cast/forecasts/bundled-parquet/",
               s3_endpoint = "amnh1.osn.mghpcc.org",
               anonymous=TRUE) |>
  mutate(
    depth_m = as.numeric(depth_m),
    depth_m = ifelse(is.na(depth_m), -999999, depth_m)) |>
  filter(project_id == {project},
         datetime > {cut_off_date},
         datetime <= {last_observed_date},
         !is.na(model_id),
         !is.na(parameter),
         !is.na(prediction)

  ) |>
  # if necessary, enforce naming convention on "family" to avoid perpetual rescoring
  mutate(family = ifelse(family == 'ensemble', "sample", family)) |>
  # enforce horizon filter
  mutate(horizon = date_diff('day', as.POSIXct(reference_datetime), as.POSIXct(datetime))) |>
  filter(! (duration == "P1D" & horizon > 35))

# THIS ONLY SCORES EARLIEST SUBMISSION OF A REFERENCE DATETIME.  SO A RESUBMISSION WILL NOT
# BE SCORED.  CHANGGING slice_min(pub_datetime) to slice_max(pub_datetime) WILL SCORE THE MOST
# RECENT SUBMITTED FORECAST
variable_ids <- forecasts |> distinct(variable) |> collect() |> pull(variable)

for(i in 1:length(variable_ids)){
  curr_variable_id<- variable_ids[i]
  forecasts |>
    filter(variable == curr_variable_id) |>
    group_by(model_id, variable, reference_datetime) |>
    slice_min(pub_datetime) |>
    ungroup() |>
    group_by(variable) |>
    write_dataset("s3://bio230121-bucket01/vera4cast/tmp/forecasts")
}

scores <-
  open_dataset("s3://bio230121-bucket01/vera4cast/scores/bundled-parquet/",
               s3_endpoint = "amnh1.osn.mghpcc.org", anonymous=TRUE) |>
  mutate(
    depth_m = as.numeric(depth_m),
    depth_m = ifelse(is.na(depth_m), -999999, depth_m)) |>
  filter(project_id == {project},
         datetime > {cut_off_date},
         !is.na(observation)
  )

tol <- 1e-2
if(rescore) {
  print("rescoring changed observations")
  # drop rows from scores if the scores and targets disagree on "observation"
  scores <- scores |>
    inner_join(targets, by = obs_key_cols) |>
    filter( abs(observation.x - observation.y)/observation.x < {tol})

  ## Note: Only used to anti-join (filter).
  ## The new observations will come from latest targets

  ## union() won't overwrite those rows.

}

## NOTE In theory we just want to do this:
# bench::bench_time({
#  forecasts |>
#    anti_join(scores) |> # forecast is unscored
#    inner_join(targets) |> # forecast has targets available
#    write_dataset("score_me.parquet")
#})


print("Caching forecasts, scores, targets...")

duckdbfs::duckdb_secrets(endpoint = "amnh1.osn.mghpcc.org",
                         key = Sys.getenv("OSN_KEY"),
                         secret = Sys.getenv("OSN_SECRET"),
                         bucket = "bio230121-bucket01/vera4cast")


## INSTEAD, we pull our subset to local disk first.
## This looks silly but is much better for RAM and speed!!
#bench::bench_time({ # ~ 5.4m (w/ 6mo cutoff)
#  forecasts |> group_by(variable) |> write_dataset("s3://bio230121-bucket01/vera4cast/tmp/forecasts")
#})

bench::bench_time({
  scores |> group_by(variable) |> write_dataset("s3://bio230121-bucket01/vera4cast/tmp/scores")
})

bench::bench_time({
  targets |> group_by(variable) |> write_dataset("s3://bio230121-bucket01/vera4cast/tmp/targets")
})

bench::bench_time({
  forecasts <- open_dataset("s3://bio230121-bucket01/vera4cast/tmp/forecasts/**")
  scores <- open_dataset("s3://bio230121-bucket01/vera4cast/tmp/scores/**")
  targets <- open_dataset("s3://bio230121-bucket01/vera4cast/tmp/targets/**")
})

## Magic rock&roll time: Subset unscored + targets available:
print("Compute who needs to be scored...")

bench::bench_time({ # ~ 13s
  forecasts |>
    anti_join(select(scores, all_of(score_key_cols))) |> # forecast is unscored
    inner_join(targets) |> # forecast has targets available
    distinct() |> # remove duplicate submissions
    group_by(variable) |>
    write_dataset("s3://bio230121-bucket01/vera4cast/tmp/score_me")

})



