remotes::install_github("cboettig/duckdbfs", upgrade=FALSE)

score4cast::ignore_sigpipe()

library(tidyverse)
library(duckdbfs)
library(minioclient)
library(bench)
library(glue)
library(fs)
library(future.apply)
library(progressr)
handlers(global = TRUE)
handlers("cli")


# # bundled count at start
b <- open_dataset("s3://bio230121-bucket01/vera4cast/forecasts/bundled-summaries",
                  s3_endpoint = "amnh1.osn.mghpcc.org",
                  anonymous = TRUE)

b |> count() |> print()
b |> summarise(date = max(reference_datetime)) |> print()


install_mc()
mc_alias_set("osn", "amnh1.osn.mghpcc.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))
# mc_alias_set("nrp", "s3-west.nrp-nautilus.io", Sys.getenv("EFI_NRP_KEY"), Sys.getenv("EFI_NRP_SECRET"))

duckdb_secrets(endpoint = "amnh1.osn.mghpcc.org",
               key = Sys.getenv("OSN_KEY"),
               secret = Sys.getenv("OSN_SECRET"),
               bucket = "bio230121-bucket01")


remote_path <- "osn/bio230121-bucket01/vera4cast/forecasts/summaries/project_id=vera4cast/"
contents <- mc_ls(remote_path, recursive = TRUE, details = TRUE)
data_paths <- contents |> filter(!is_folder) |> pull(path)

# model paths are paths with at least one reference_datetime containing data files
model_paths <-
  data_paths |>
  str_replace_all("reference_date=\\d{4}-\\d{2}-\\d{2}/.*", "") |>
  str_replace("^osn\\/", "s3://") |>
  unique()

#print(model_paths)

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


bundle_me <- function(path) {

  print(path)
  con = duckdbfs::cached_connection(tempfile())
  duckdb_secrets(endpoint = "amnh1.osn.mghpcc.org",
                 key = Sys.getenv("OSN_KEY"),
                 secret = Sys.getenv("OSN_SECRET"),
                 bucket = "bio230121-bucket01")

  bundled_path <- path |>
    str_replace(fixed("forecasts/summaries"), "forecasts/bundled-summaries")

  #open_dataset(path, conn = con, unify_schemas = TRUE) |> write_dataset("tmp_new.parquet")
  #open_dataset(bundled_path, conn = con, unify_schemas = TRUE) |> write_dataset("tmp_old.parquet")

  # these are both local, so we can stream back.
  #new <- open_dataset("tmp_new.parquet")
  #old <- open_dataset("tmp_old.parquet")

  # union_all(old, new) |>
  #   write_dataset(bundled_path,
  #                 options = list("PER_THREAD_OUTPUT false"))


  files_exist <- minioclient::mc_ls(bundled_path |> str_replace(fixed("s3://"), "osn/"))
  files_exist <- files_exist[files_exist != "/"]

  if (length(files_exist) > 0){
    #print('previous bundle files exist')
    open_dataset(path, conn = con, unify_schemas = TRUE) |> write_dataset("tmp_new.parquet")
    open_dataset(bundled_path, conn = con, unify_schemas = TRUE) |> write_dataset("tmp_old.parquet")

    #these are both local, so we can stream back.
    new <- open_dataset("tmp_new.parquet")
    old <- open_dataset("tmp_old.parquet")

    union_all(old, new) |>
      write_dataset(paste0(bundled_path,"data_0.parquet"),
                    options = list("PER_THREAD_OUTPUT false"))

  } else {

    print(paste0('No bundle files exist for: ', bundled_path))

    open_dataset(path, conn = con, unify_schemas = TRUE) |> write_dataset("tmp_new.parquet")

    new <- open_dataset("tmp_new.parquet")

    new |>
      write_dataset(paste0(bundled_path,"data_0.parquet"),
                    options = list("PER_THREAD_OUTPUT false"))
  }

  #We should now archive anything we have bundled:

  mc_path <- path |> str_replace(fixed("s3://"), "osn/")
  dest_path <- mc_path |>
    str_replace(fixed("forecasts/summaries"), "forecasts/archive-parquet-summaries")
  mc_mv(mc_path, dest_path, recursive = TRUE)
  # clears up empty folders
  #mc_rm(mc_path, recursive = TRUE)
  remove_dir(mc_path)


  duckdbfs::close_connection(con); gc()
  invisible(path)
}



# We use future_apply framework to show progress while being robust to OOM kils.
# We are not actually running on multi-core, which would be RAM-inefficient

future::plan(future::sequential)

safe_bundles <- function(xs) {
  p <- progressor(along = xs)
  future_lapply(xs, function(x, ...) {
    bundle_me(x)
    p(sprintf("x=%s", x))
  },  future.seed = TRUE)
}


bench::bench_time({
  safe_bundles(model_paths)
})

## old method
# bench::bench_time({
#   out <- purrr::map(model_paths, bundle_me)
# })


# # bundled count at end
# count <- open_dataset("s3://bio230121-bucket01/vera4cast/forecasts/bundled-summaries",
#                       s3_endpoint = "amnh1.osn.mghpcc.org",
#                       anonymous = TRUE) |>
#   count()
#
# print(count)
