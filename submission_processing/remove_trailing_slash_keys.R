# Identify and remove trailing-slash "directory marker" objects in the
# forecasts/bundled-parquet prefix of the vera4cast OSN bucket.
#
# These keys (ending in "/") are sometimes real Parquet payloads written to a
# directory-style key by a bundling bug, which breaks arrow::open_dataset
# scans with "RESOURCE_NOT_FOUND during GetObject".
#
# IMPORTANT: neither mc_rm() nor arrow's DeleteFiles() can delete these keys.
# - mc_rm(recursive = TRUE) treats the marker key as a PREFIX and deletes the
#   sibling data_0.parquet too (confirmed by testing).
# - arrow's FileSystem$DeleteFiles() normalizes paths and strips the trailing
#   "/", then reports "Path does not exist" — it cannot address an object
#   whose key literally ends in "/".
# Only a raw S3 DeleteObject targets the exact key. aws.s3::delete_object()
# with region = "" does this correctly (verified by test: marker removed,
# sibling intact).
#
# Set DRY_RUN <- FALSE to actually delete. Requires OSN_KEY / OSN_SECRET
# env vars for deletion; the audit itself works anonymously.

library(tidyverse)
library(minioclient)
library(aws.s3)

# --- Configuration ---------------------------------------------------------

BUCKET   <- "bio230121-bucket01"
PREFIX   <- "vera4cast/forecasts/bundled-parquet/"
ENDPOINT <- "amnh1.osn.mghpcc.org"
DRY_RUN  <- TRUE   # set to FALSE to delete after reviewing the audit output

# --- Audit -----------------------------------------------------------------

# List every object under the bundled-parquet prefix and keep the ones whose
# key ends in "/" (directory markers — legitimate files all end in .parquet).
# mc_ls reports trailing-slash marker keys as is_folder == TRUE, so we must
# NOT exclude folders. Note also that these markers are often NON-EMPTY:
# they are real Parquet payloads written to a directory-style key.
remote_path <- file.path('osn', BUCKET, PREFIX)

contents <- mc_ls(remote_path, recursive = TRUE, details = TRUE)

markers <- contents |>
  filter(is_folder | str_detect(path, "/$"))

if (nrow(markers) == 0) {
  message("No trailing-slash objects found. Nothing to do.")
  quit(save = "no")
}

# Drop the "osn/bucket/" mc-alias prefix to recover raw S3 keys, then make
# them relative to the bucket for use with arrow's bucket-rooted filesystem.
markers <- markers |>
  mutate(
    key          = str_replace(path, fixed("osn/"), ""),
    key_relative = str_replace(key, paste0("^", BUCKET, "/"), "")
  )

message("Found ", nrow(markers), " trailing-slash object(s):")
markers |>
  select(key, bytes, is_folder) |>
  arrange(key) |>
  print(n = Inf)

# bytes > 0 means a real payload was written to the marker key (the
# forecasts case — typically Parquet data). bytes == 0 means an empty
# placeholder (the scores case).
markers |>
  mutate(problem_type = if_else(bytes > 0,
                                "payload at directory key",
                                "empty placeholder")) |>
  count(problem_type, sort = TRUE) |>
  print()

if (DRY_RUN) {
  message("DRY_RUN is TRUE — no deletions performed. ",
          "Review the list above, then set DRY_RUN <- FALSE and re-run.")
  quit(save = "no")
}

# --- Deletion (executed only when DRY_RUN <- FALSE) ------------------------

# NOTE: marker keys with bytes > 0 may contain Parquet rows. Review the audit
# output above before disabling DRY_RUN.
message("Deleting ", nrow(markers), " trailing-slash object(s)...")

# aws.s3 reads creds from AWS_* env vars; map the OSN ones over. region = ""
# is required — otherwise aws.s3 embeds the region into the non-AWS hostname
# (us-east-1.amnh1.osn.mghpcc.org) and the endpoint 404s.
Sys.setenv(AWS_ACCESS_KEY_ID     = Sys.getenv("OSN_KEY"),
           AWS_SECRET_ACCESS_KEY = Sys.getenv("OSN_SECRET"))

results <- markers$key_relative |>
  set_names() |>
  map_lgl(
    ~ tryCatch({
      aws.s3::delete_object(object = .x, bucket = BUCKET,
                            base_url = ENDPOINT, region = "")
      TRUE
    }, error = function(e) {
      message("FAILED to delete ", .x, ": ", conditionMessage(e))
      FALSE
    })
  )

message("Successfully deleted ", sum(results), " of ", length(results), ".")

# Verify: re-list and confirm nothing with a trailing slash remains, and
# that sibling .parquet files were untouched.
remaining <- mc_ls(remote_path, recursive = TRUE, details = TRUE)

remaining_markers <- remaining |>
  filter(is_folder | str_detect(path, "/$"))

sibling_check <- remaining |>
  filter(str_detect(path, "\\.parquet$"))

expected_siblings <- nrow(contents |> filter(str_detect(path, "\\.parquet$")))

if (nrow(remaining_markers) == 0) {
  message("Verification passed: no trailing-slash objects remain.")
} else {
  message("WARNING: ", nrow(remaining_markers), " trailing-slash object(s) still present:")
  print(remaining_markers, n = Inf)
}

if (nrow(sibling_check) == expected_siblings) {
  message("Verification passed: all ", expected_siblings, " sibling .parquet files intact.")
} else {
  message("WARNING: expected ", expected_siblings, " .parquet files, found ",
          nrow(sibling_check), " — some data files may have been deleted!")
}
