# =============================================================================
# XGBoost Feature Identification for VERA Forecast Challenge
# =============================================================================
# For each of the 42 target variables (Temp_C_mean through DRSI_mgL_sample)
# at site = "fcre", depth_m == 1.6 or NA, this script identifies the best
# features using:
#   1. XGBoost Gain Importance  - average loss reduction per split
#   2. SHAP (mean |SHAP| value) - via xgboost::predict(..., predcontrib=TRUE)
# Features are selected by INTERSECTION: must exceed the mean threshold on
# BOTH metrics. If the intersection produces < 3 features, the union is used.
# =============================================================================

library(xgboost)
library(dplyr)
library(tidyr)
library(tibble)
library(zoo)

set.seed(42)

# ── 1. Load & filter data ────────────────────────────────────────────────────
dat_raw <- read.csv(
  file.path("baseline_models", "fcr_xgboost_training_dataset.csv"),
  stringsAsFactors = FALSE
)

dat <- dat_raw |>
  filter(site_id == "fcre", is.na(depth_m) | depth_m == 1.6) |>
  arrange(datetime)

cat("Filtered rows:", nrow(dat), "\n")

# ── 2. Define target and feature columns ─────────────────────────────────────
all_cols    <- names(dat)
target_cols <- all_cols[which(all_cols == "Temp_C_mean"):which(all_cols == "DRSI_mgL_sample")]
meta_cols   <- c("site_id", "datetime", "depth_m")

# Columns that can serve as features (anything that is not metadata)
all_feature_pool <- setdiff(all_cols, meta_cols)

cat("Target variables:", length(target_cols), "\n")
cat("Max feature pool size:", length(all_feature_pool) - 1L, "(excluding current target)\n\n")

# ── 3. Circular day-of-year encoding ─────────────────────────────────────────
# Replace raw doy with sin/cos to capture seasonal continuity
dat <- dat |>
  mutate(
    sin_doy = sin(2 * pi * doy / 365),
    cos_doy = cos(2 * pi * doy / 365)
  ) |>
  select(-doy)

# Update feature pool after doy replacement
all_feature_pool <- setdiff(names(dat), meta_cols)

# ── 4. Helper: prepare a model matrix for one target variable ─────────────────
# - Remove features with > 50% NA (too sparse to be useful)
# - Median-impute remaining NAs
# - Drop rows where the target itself is NA
prepare_matrix <- function(df, target, feature_pool) {
  features <- setdiff(feature_pool, target)

  # Remove columns from the 42-target block if they are the current target
  # (other target-block columns are kept as potential features)
  sub <- df[, c(target, features), drop = FALSE]

  # Drop rows where target is NA
  sub <- sub[!is.na(sub[[target]]), ]

  if (nrow(sub) < 30) return(NULL)   # Too few observations to model

  # Drop features with > 50% NA
  na_frac  <- colMeans(is.na(sub[, features, drop = FALSE]))
  features <- features[na_frac <= 0.5]

  if (length(features) == 0) return(NULL)

  sub <- sub[, c(target, features), drop = FALSE]

  # Median imputation for remaining NAs (column-wise)
  for (col in features) {
    if (any(is.na(sub[[col]]))) {
      med <- median(sub[[col]], na.rm = TRUE)
      sub[[col]][is.na(sub[[col]])] <- if (is.finite(med)) med else 0
    }
  }

  list(
    y        = sub[[target]],
    X        = as.matrix(sub[, features, drop = FALSE]),
    features = features
  )
}

# ── 5. Helper: fit XGBoost and return gain + SHAP importances ─────────────────
fit_xgb <- function(y, X) {
  dtrain <- xgb.DMatrix(data = X, label = y)

  params <- list(
    objective        = "reg:squarederror",
    max_depth        = 5,
    eta              = 0.1,
    subsample        = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 3,
    nthread          = parallel::detectCores() - 1L
  )

  model <- xgb.train(
    params  = params,
    data    = dtrain,
    nrounds = 150,
    verbose = 0
  )

  # --- Gain importance ---
  imp <- xgb.importance(model = model)
  gain_df <- as.data.frame(imp)[, c("Feature", "Gain")]
  names(gain_df) <- c("feature", "gain")

  # --- SHAP values via predcontrib ---
  shap_mat <- predict(model, X, predcontrib = TRUE)
  # Last column is BIAS — drop it
  shap_mat  <- shap_mat[, seq_len(ncol(shap_mat) - 1L), drop = FALSE]
  shap_mean <- colMeans(abs(shap_mat))
  shap_df   <- data.frame(
    feature   = names(shap_mean),
    shap_mean = shap_mean,
    row.names = NULL
  )

  list(gain = gain_df, shap = shap_df)
}

# ── 6. Helper: select features using intersection (or union fallback) ──────────
select_features <- function(gain_df, shap_df) {
  # Threshold = mean value of each metric across all features
  gain_thresh <- mean(gain_df$gain, na.rm = TRUE)
  shap_thresh <- mean(shap_df$shap_mean, na.rm = TRUE)

  gain_selected <- gain_df$feature[gain_df$gain >= gain_thresh]
  shap_selected <- shap_df$feature[shap_df$shap_mean >= shap_thresh]

  intersection <- intersect(gain_selected, shap_selected)

  if (length(intersection) >= 3) {
    list(features = intersection, method = "XGBoost Gain Importance; SHAP (intersection)")
  } else {
    union_feats <- union(gain_selected, shap_selected)
    list(features = union_feats, method = "XGBoost Gain Importance; SHAP (union fallback)")
  }
}

# ── 7. Main loop over 42 target variables ─────────────────────────────────────
results <- vector("list", length(target_cols))

for (i in seq_along(target_cols)) {
  tgt <- target_cols[i]
  cat(sprintf("[%02d/%d] %s ... ", i, length(target_cols), tgt))

  prep <- prepare_matrix(dat, tgt, all_feature_pool)

  if (is.null(prep)) {
    cat("SKIPPED (insufficient data)\n")
    results[[i]] <- data.frame(
      variable = tgt,
      features = NA_character_,
      method   = "Skipped — insufficient data after NA removal",
      stringsAsFactors = FALSE
    )
    next
  }

  fit_out <- tryCatch(
    fit_xgb(prep$y, prep$X),
    error = function(e) {
      cat("ERROR:", conditionMessage(e), "\n")
      NULL
    }
  )

  if (is.null(fit_out)) {
    results[[i]] <- data.frame(
      variable = tgt,
      features = NA_character_,
      method   = "Skipped — model error",
      stringsAsFactors = FALSE
    )
    next
  }

  sel <- select_features(fit_out$gain, fit_out$shap)

  cat(length(sel$features), "features selected\n")

  results[[i]] <- data.frame(
    variable = tgt,
    features = paste(sort(sel$features), collapse = "; "),
    method   = sel$method,
    stringsAsFactors = FALSE
  )
}

# ── 8. Write output CSV ───────────────────────────────────────────────────────
out <- bind_rows(results)

write.csv(
  out,
  file      = file.path("baseline_models", "xgboost_variable_features.csv"),
  row.names = FALSE
)

cat("\nDone. Results written to baseline_models/xgboost_variable_features.csv\n")
print(out[, c("variable", "method")])
