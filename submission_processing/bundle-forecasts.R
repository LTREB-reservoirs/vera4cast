remotes::install_github("cboettig/duckdbfs", upgrade=FALSE)

score4cast::ignore_sigpipe()


library(tidyverse)
library(duckdbfs)
library(minioclient)
library(bench)
library(glue)
library(fs)

install_mc()
mc_alias_set("osn", "amnh1.osn.mghpcc.org", Sys.getenv("OSN_KEY"), Sys.getenv("OSN_SECRET"))
# mc_alias_set("nrp", "s3-west.nrp-nautilus.io", Sys.getenv("EFI_NRP_KEY"), Sys.getenv("EFI_NRP_SECRET"))

duckdb_secrets(endpoint = "amnh1.osn.mghpcc.org", key = Sys.getenv("OSN_KEY"), secret = Sys.getenv("OSN_SECRET"), bucket = "bio230121-bucket01")



remote_path <- "osn/bio230121-bucket01/vera4cast/forecasts/parquet/project_id=vera4cast/"
contents <- mc_ls(remote_path, recursive = TRUE, details = TRUE)
data_paths <- contents |> filter(!is_folder) |> pull(path)

# model paths are paths with at least one reference_datetime containing data files
model_paths <-
  data_paths |>
  str_replace_all("reference_date=\\d{4}-\\d{2}-\\d{2}/.*", "") |>
  str_replace("^osn\\/", "s3://") |>
  unique()

print(model_paths)

# bundled count at start
open_dataset("s3://bio230121-bucket01/vera4cast/forecasts/bundled-parquet",
             s3_endpoint = "amnh1.osn.mghpcc.org",
             anonymous = TRUE) |>
  count()

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
  duckdbfs::duckdb_secrets(endpoint = "amnh1.osn.mghpcc.org", key = Sys.getenv("OSN_KEY"), secret = Sys.getenv("OSN_SECRET"), bucket = "bio230121-bucket01")
  bundled_path <- path |> str_replace(fixed("forecasts/parquet"), "forecasts/bundled-parquet")

  #print('write new')
  open_dataset(path, conn = con, unify_schemas = TRUE) |>
    filter( !is.na(model_id),
            !is.na(parameter),
            !is.na(prediction)) |>
    select(-any_of(c("date", "reference_date", "...1", "pub_date"))) |>
    #rename(pub_datetime = pub_date) |>
    write_dataset("tmp_new.parquet")

  #print('write old')
  # special filters should not be needed on bundled copy
  open_dataset(bundled_path, conn = con, unify_schemas = TRUE) |>
    write_dataset("tmp_old.parquet")

  # these are both local, so we can stream back.
  #print('open new')
  new <- open_dataset("tmp_new.parquet")
  #print('open old')
  old <- open_dataset("tmp_old.parquet")

  ## We can just "append", we no longer face duplicates:
  # by <- join_by(datetime, site_id, prediction, parameter, family, reference_datetime, pub_datetime, duration, model_id, project_id, variable)
  #  filtered_n <- old |> anti_join(new, by = by) |> count() |> pull(n) # is this the bottleneck?
  #  previous_n <- open_dataset("tmp_old.parquet") |> count() |> pull(n)
  #  stopifnot(previous_n - filtered_n == 0)

  ## no partition levels left so we must write to an explicit .parquet
  bundled_dir <- bundled_path |> str_replace(fixed("s3://"), "osn/") |> mc_ls(details = TRUE)
  mc_bundled_path <- bundled_dir |> filter(!is_folder) |> pull(path)
  stopifnot(length(mc_bundled_path) == 1)
  bundled_path <- mc_bundled_path |> str_replace(fixed("osn/"), fixed("s3://"))

  union_all(old, new) |>
    write_dataset(bundled_path,
                  options = list("PER_THREAD_OUTPUT false"))

  #We should now archive anything we have bundled:
  mc_path <- path |> str_replace(fixed("s3://"), "osn/")
  dest_path <- mc_path |>
    str_replace(fixed("forecasts/parquet"), "forecasts/archive-parquet")
  mc_mv(mc_path, dest_path, recursive = TRUE)

  # clears up empty folders (not necessary?)
  #mc_rm(mc_path, recursive = TRUE)
  remove_dir(mc_path)

  duckdbfs::close_connection(con); gc()

  invisible(path)
}


# We use future_apply framework to show progress while being robust to OOM kils.
# We are not actually running on multi-core, which would be RAM-inefficient

# future::plan(future::sequential)
#
# safe_bundles <- function(xs) {
#   p <- progressor(along = xs)
#   future_lapply(xs, function(x, ...) {
#     out <- bundle_me(x)
#     p(sprintf("x=%s", x))
#     out
#   },  future.seed = TRUE)
# }

# bench::bench_time({
#   out <- safe_bundles(model_paths)
# })
# print(out)

bench::bench_time({
  out <- purrr::map(model_paths, bundle_me)
})

# bundled count at end
count <- open_dataset("s3://bio230121-bucket01/vera4cast/forecasts/bundled-parquet",
                      s3_endpoint = "amnh1.osn.mghpcc.org",
                      anonymous = TRUE) |>
  count()
print(count)


most_recent <- open_dataset("s3://bio230121-bucket01/vera4cast/forecasts/bundled-parquet",
                            s3_endpoint = "amnh1.osn.mghpcc.org",
                            anonymous = TRUE) |>
  group_by(model_id, variable) |>
  summarise(most_recent = max(reference_datetime)) |>
  arrange(desc(most_recent))
print(most_recent)
