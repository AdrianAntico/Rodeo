#' Fit scoring-safe tabular feature engineering
#'
#' @description
#' Preferred expert entry point for Rodeo tabular feature engineering. Important
#' transformation, encoding, interaction, and leakage controls remain explicit.
#' The returned fitted object can be applied to new data with [predict()].
#'
#' @param data Training data.
#' @param numeric_columns,categorical_columns,calendar_columns,text_columns Columns
#'   assigned to each feature family.
#' @param numeric_transforms Numeric transformations to fit and apply.
#' @param winsorize_probs Lower and upper winsorization probabilities.
#' @param calendar_features,text_features Features generated for calendar and text columns.
#' @param missingness_columns Columns receiving missingness indicators.
#' @param numeric_interactions Numeric column pairs.
#' @param categorical_numeric_interactions Lists containing `categorical` and `numeric`.
#' @param categorical_interactions Categorical column pairs.
#' @param max_interactions Maximum generated interaction columns.
#' @param top_n_levels Number of frequent categorical levels retained separately.
#' @param rare_level,unseen_level Labels for pooled rare and unseen levels.
#' @param keep_original Whether original categorical columns remain in output.
#' @param supervised_columns Columns receiving supervised encoding.
#' @param target Target column for supervised encoding.
#' @param supervised_method `"none"`, `"target_mean"`, or `"woe"`.
#' @param smoothing Supervised-encoding shrinkage strength.
#' @param folds Number of cross-fitting folds used on training rows.
#' @param positive_class Positive target class for WOE.
#' @param seed Reproducibility seed for cross-fitting.
#' @param copy_data Whether to copy before assigning generated columns.
#' @param metadata User metadata retained in the fitted plan.
#' @return A reusable `rodeo_features_fit` containing engineered training data,
#'   fitted state, feature lineage, diagnostics, and warnings.
#' @export
rodeo_features <- function(
  data,
  numeric_columns = character(),
  categorical_columns = character(),
  calendar_columns = character(),
  text_columns = character(),
  numeric_transforms = c("log1p", "sqrt", "standardize", "winsorize"),
  winsorize_probs = c(0.01, 0.99),
  calendar_features = c("year", "month", "day", "wday", "week", "quarter", "is_weekend"),
  text_features = c("char_count", "word_count", "digit_count", "punct_count", "upper_ratio", "blank"),
  missingness_columns = character(),
  numeric_interactions = list(),
  categorical_numeric_interactions = list(),
  categorical_interactions = list(),
  max_interactions = 50L,
  top_n_levels = 10L,
  rare_level = "__RARE__",
  unseen_level = "__UNSEEN__",
  keep_original = TRUE,
  supervised_columns = character(),
  target = NULL,
  supervised_method = c("none", "target_mean", "woe"),
  smoothing = 20,
  folds = 5L,
  positive_class = NULL,
  seed = 1L,
  copy_data = TRUE,
  metadata = list()
) {
  supervised_method <- match.arg(supervised_method)
  plan <- rodeo_feature_plan(
    numeric = list(columns = numeric_columns, transforms = numeric_transforms,
      winsorize_probs = winsorize_probs),
    categorical = list(columns = categorical_columns, top_n = top_n_levels,
      rare_level = rare_level, unseen_level = unseen_level, one_hot = TRUE,
      keep_original = keep_original),
    calendar = list(columns = calendar_columns, features = calendar_features),
    text = list(columns = text_columns, features = text_features),
    missingness = list(columns = missingness_columns, suffix = "_is_missing"),
    interactions = list(numeric_pairs = numeric_interactions,
      categorical_numeric = categorical_numeric_interactions,
      categorical_pairs = categorical_interactions, max_features = max_interactions),
    supervised_encoding = list(columns = supervised_columns, target = target,
      method = supervised_method, smoothing = smoothing, folds = folds,
      seed = seed, positive_class = positive_class, suffix = NULL),
    metadata = metadata
  )
  fitted <- rodeo_fit_transform_feature_plan(data, plan, copy_data = copy_data)
  out <- list(
    data = fitted$engineered_data,
    fitted_state = fitted$fitted_plan,
    feature_manifest = fitted$fitted_plan$feature_manifest,
    diagnostics = fitted$fitted_plan$diagnostics,
    warnings = fitted$fitted_plan$warnings,
    call = match.call()
  )
  class(out) <- c("rodeo_features_fit", "list")
  out
}

#' Apply a fitted Rodeo tabular feature workflow
#' @param object A fitted object returned by [rodeo_features()].
#' @param newdata New scoring data.
#' @param copy_data Whether to copy before adding columns.
#' @param ... Reserved for compatible extensions.
#' @return Engineered scoring data using retained training state.
#' @export
predict.rodeo_features_fit <- function(object, newdata, copy_data = TRUE, ...) {
  rodeo_transform_feature_plan(newdata, object$fitted_state, copy_data = copy_data)
}

#' Fit leakage-safe temporal feature engineering
#'
#' @description
#' Preferred temporal feature entry point. Lag, rolling, differencing, Fourier,
#' known-future, static, entity, and forecast-origin semantics are explicit.
#' Apply the retained temporal state with [predict()].
#'
#' @inheritParams rodeo_temporal_transformation_spec
#' @param data Training history.
#' @param forecast_origin Last timestamp allowed to contribute fitted history.
#' @return A reusable `rodeo_temporal_features_fit` with prepared history,
#'   fitted state, lineage, and diagnostics.
#' @export
rodeo_temporal_features <- function(
  data, date_col, target_col, frequency = "auto",
  calendar_features = c("year", "month", "day", "dow", "day_index"),
  lag_periods = 1L, seasonal_lag_periods = integer(),
  rolling_windows = integer(), rolling_stats = "mean",
  rolling_quantiles = c(0.25, 0.5, 0.75), difference_orders = integer(),
  seasonal_difference_periods = integer(), ewm_alphas = numeric(),
  fourier_periods = integer(), fourier_pairs = 1L,
  expanding_stats = character(), known_future_variables = character(),
  static_entity_features = character(), entity_id = NULL,
  forecast_horizon = 1L, forecast_origin = NULL, metadata = list()
) {
  spec <- rodeo_temporal_transformation_spec(
    date_col = date_col, target_col = target_col, frequency = frequency,
    calendar_features = calendar_features, lag_periods = lag_periods,
    seasonal_lag_periods = seasonal_lag_periods,
    rolling_windows = rolling_windows, rolling_stats = rolling_stats,
    rolling_quantiles = rolling_quantiles, difference_orders = difference_orders,
    seasonal_difference_periods = seasonal_difference_periods,
    ewm_alphas = ewm_alphas, fourier_periods = fourier_periods,
    fourier_pairs = fourier_pairs, expanding_stats = expanding_stats,
    known_future_variables = known_future_variables,
    static_entity_features = static_entity_features, entity_id = entity_id,
    forecast_horizon = forecast_horizon, metadata = metadata
  )
  fitted <- rodeo_fit_temporal_transformation(data, spec, forecast_origin)
  out <- list(
    data = fitted$prepared_history,
    fitted_state = fitted,
    feature_manifest = fitted$feature_manifest,
    diagnostics = fitted$diagnostics,
    warnings = character(),
    call = match.call()
  )
  class(out) <- c("rodeo_temporal_features_fit", "list")
  out
}

#' Apply a fitted Rodeo temporal feature workflow
#' @param object A fitted object returned by [rodeo_temporal_features()].
#' @param newdata Temporal data to transform.
#' @param copy_data Whether to copy before adding columns.
#' @param ... Reserved for compatible extensions.
#' @return Leakage-safe temporal features.
#' @export
predict.rodeo_temporal_features_fit <- function(object, newdata, copy_data = TRUE, ...) {
  rodeo_apply_temporal_transformation(newdata, object$fitted_state, copy_data)
}

#' Fit reproducible model partitions and folds
#'
#' @description
#' Preferred model-preparation entry point. Random, stratified, grouped, and
#' chronological partition semantics remain explicit and auditable.
#'
#' @inheritParams rodeo_partition_plan
#' @param data Data to partition.
#' @return A reusable `rodeo_model_prep_fit` with prepared data, assignments,
#'   manifests, diagnostics, and warnings.
#' @export
rodeo_model_prep <- function(
  data, method = c("random", "stratified", "grouped", "time"),
  fractions = c(train = 0.8, test = 0.2), target_col = NULL,
  group_col = NULL, date_col = NULL, seed = 123L,
  row_id_col = ".row_id", partition_col = ".partition",
  fold_col = ".fold_id", k = 5L, metadata = list()
) {
  method <- match.arg(method)
  plan <- rodeo_partition_plan(method, fractions, target_col, group_col,
    date_col, seed, row_id_col, partition_col, fold_col, k, metadata)
  fitted <- rodeo_fit_partition_plan(data, plan)
  out <- list(
    data = rodeo_apply_partition_plan(data, fitted),
    fitted_state = fitted,
    partition_manifest = fitted$partition_manifest,
    fold_manifest = fitted$fold_manifest,
    diagnostics = fitted$diagnostics,
    warnings = fitted$warnings,
    call = match.call()
  )
  class(out) <- c("rodeo_model_prep_fit", "list")
  out
}

#' Apply retained Rodeo partition assignments
#' @param object A fitted object returned by [rodeo_model_prep()].
#' @param newdata Data containing the retained row identities.
#' @param copy_data Whether to copy before joining assignments.
#' @param ... Reserved for compatible extensions.
#' @return Data joined to retained partition and fold assignments.
#' @export
predict.rodeo_model_prep_fit <- function(object, newdata, copy_data = TRUE, ...) {
  rodeo_apply_partition_plan(newdata, object$fitted_state, copy_data)
}

#' QA the preferred Rodeo expert API
#' @return Compact test table.
#' @export
qa_rodeo_preferred_api <- function() {
  train <- data.table::data.table(
    id = 1:12, x = seq_len(12), category = rep(c("a", "b", "c"), 4),
    target = rep(c(0, 1), 6), date = as.Date("2024-01-01") + 0:11
  )
  feature_fit <- rodeo_features(train,
    numeric_columns = "x", categorical_columns = "category",
    supervised_columns = "category", target = "target",
    supervised_method = "woe", positive_class = "1", folds = 3L)
  scored <- predict(feature_fit, data.table::data.table(
    id = 13L, x = 13, category = "unseen", target = 0,
    date = as.Date("2024-01-13")))
  temporal_fit <- rodeo_temporal_features(train, "date", "x",
    lag_periods = c(1L, 2L), rolling_windows = 3L,
    rolling_stats = c("mean", "sd"), forecast_origin = max(train$date))
  temporal_scored <- predict(temporal_fit, train)
  prep_fit <- rodeo_model_prep(train, method = "stratified",
    target_col = "target", fractions = c(train = 0.75, test = 0.25), k = 3L)
  prep_replay <- predict(prep_fit, train)
  data.table::data.table(
    test = c("tabular_fit", "scoring_state", "supervised_lineage",
      "temporal_fit", "temporal_application", "partition_replay"),
    passed = c(inherits(feature_fit, "rodeo_features_fit"),
      "category_woe" %in% names(scored),
      any(feature_fit$feature_manifest$family == "supervised_encoding"),
      inherits(temporal_fit, "rodeo_temporal_features_fit"),
      all(c("target_lag_1", "target_roll_mean_3") %in% names(temporal_scored)),
      identical(prep_fit$data$.partition, prep_replay$.partition)),
    detail = c("preferred tabular fit", "unseen category uses retained state",
      "target-derived feature is traceable", "preferred temporal fit",
      "temporal state applies without refit", "partition identities are stable")
  )
}
