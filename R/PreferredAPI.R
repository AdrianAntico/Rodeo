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

rodeo_preferred_copy <- function(data, copy_data = TRUE) {
  dt <- data.table::as.data.table(data)
  if (isTRUE(copy_data)) data.table::copy(dt) else dt
}

rodeo_preferred_manifest <- function(feature, source_column, family, transform,
    scoring_safe = TRUE) {
  data.table::data.table(
    feature = as.character(feature),
    source_column = as.character(source_column),
    family = as.character(family),
    transform = as.character(transform),
    scoring_safe = as.logical(scoring_safe)
  )
}

rodeo_preferred_unsafe_exports <- function() {
  c("MEOW", "DummifyDT", "DummyVariables", "Encoding", "EncodeCharacterVariables",
    "CalendarVariables", "HolidayVariables", "TimeSeriesFeatures",
    "AutoEncoder_H2O", "Clustering_H2O", "IsolationForest_H2O", "Word2Vec_H2O",
    "FakeDataGenerator", "BenchmarkData", "BuildBinary", "Install", "UpdateDocs",
    "Mode", "LB", "qa_rodeo_package", "qa_rodeo_preferred_api",
    "qa_rodeo_vnext_supervised_encoding", "rodeo_temporal_coerce_index",
    "rodeo_temporal_frequency_is_intraday", "rodeo_temporal_next_index",
    "rodeo_temporal_normalize_frequency", "rodeo_temporal_prediction_frame",
    "rodeo_temporal_resolve_frequency", "rodeo_validate_temporal_schema",
    "rodeo_validate_transformation_schema", "weekdays_in_month",
    "Test_Asin", "Test_Asinh", "Test_BoxCox", "Test_Identity", "Test_Log",
    "Test_LogPlus1", "Test_Logit", "Test_Sqrt", "Test_YeoJohnson")
}

#' Fit leakage-aware categorical encodings
#'
#' @description
#' Preferred encoding entry point. Wraps [CategoricalEncoding()] and retains
#' in-memory encoding maps so scoring does not depend on CSV side channels.
#' Mixed-effects (`meow`) is intentionally unavailable.
#'
#' @param data Training data.
#' @param columns Categorical columns to encode.
#' @param method Encoding method.
#' @param target Target column for target, WOE, and credibility encodings.
#' @param ml_type Model type consumed by the encoder.
#' @param keep_original Whether original categorical columns remain in output.
#' @param impute_unseen Value assigned to unseen levels at scoring time.
#' @param copy_data Whether to copy before encoding.
#' @return A reusable `rodeo_encode_fit` with encoded training data, maps, and
#'   lineage. Apply with [predict()].
#' @export
rodeo_encode <- function(
  data,
  columns,
  method = c("credibility", "target_encoding", "woe", "poly_encode",
    "backward_difference", "helmert"),
  target = NULL,
  ml_type = c("classification", "regression", "multiclass"),
  keep_original = TRUE,
  impute_unseen = 0,
  copy_data = TRUE
) {
  method <- match.arg(tolower(as.character(method)[[1L]]),
    c("credibility", "target_encoding", "woe", "poly_encode",
      "backward_difference", "helmert"))
  ml_type <- match.arg(ml_type)
  columns <- as.character(columns)
  if (!length(columns)) stop("columns must name at least one categorical column.",
    call. = FALSE)
  needs_target <- method %in% c("credibility", "target_encoding", "woe")
  if (needs_target && (is.null(target) || !nzchar(as.character(target)[[1L]]))) {
    stop("method '", method, "' requires target.", call. = FALSE)
  }
  dt <- rodeo_preferred_copy(data, copy_data)
  missing_cols <- setdiff(columns, names(dt))
  if (length(missing_cols)) {
    stop(paste("columns not found:", paste(missing_cols, collapse = ", ")),
      call. = FALSE)
  }
  encoded <- CategoricalEncoding(
    data = dt,
    ML_Type = ml_type,
    GroupVariables = columns,
    TargetVariable = if (needs_target) as.character(target)[[1L]] else NULL,
    Method = method,
    SavePath = NULL,
    Scoring = FALSE,
    ImputeValueScoring = impute_unseen,
    ReturnFactorLevelList = TRUE,
    SupplyFactorLevelList = NULL,
    KeepOriginalFactors = keep_original,
    Debug = FALSE
  )
  if (is.data.frame(encoded)) {
    stop("CategoricalEncoding did not return reusable encoding maps.", call. = FALSE)
  }
  maps <- encoded$FactorCompenents
  out <- list(
    data = encoded$data,
    fitted_state = list(
      columns = columns,
      method = method,
      target = if (needs_target) as.character(target)[[1L]] else NULL,
      ml_type = ml_type,
      keep_original = isTRUE(keep_original),
      impute_unseen = impute_unseen,
      encoding_maps = maps
    ),
    feature_manifest = rodeo_preferred_manifest(
      feature = setdiff(names(encoded$data), names(data.table::as.data.table(data))),
      source_column = paste(columns, collapse = ","),
      family = "categorical_encoding",
      transform = method,
      scoring_safe = TRUE
    ),
    diagnostics = data.table::data.table(
      check = c("encoding_maps", "method"),
      status = "ok",
      detail = c(paste(names(maps), collapse = ", "), method)
    ),
    warnings = character(),
    call = match.call()
  )
  class(out) <- c("rodeo_encode_fit", "list")
  out
}

#' Apply retained categorical encodings
#' @param object A fitted object returned by [rodeo_encode()].
#' @param newdata New scoring data.
#' @param copy_data Whether to copy before encoding.
#' @param ... Reserved for compatible extensions.
#' @return Encoded scoring data using retained maps.
#' @export
predict.rodeo_encode_fit <- function(object, newdata, copy_data = TRUE, ...) {
  state <- object$fitted_state
  dt <- rodeo_preferred_copy(newdata, copy_data)
  CategoricalEncoding(
    data = dt,
    ML_Type = state$ml_type,
    GroupVariables = state$columns,
    TargetVariable = state$target,
    Method = state$method,
    SavePath = NULL,
    Scoring = TRUE,
    ImputeValueScoring = state$impute_unseen,
    ReturnFactorLevelList = FALSE,
    SupplyFactorLevelList = state$encoding_maps,
    KeepOriginalFactors = state$keep_original,
    Debug = FALSE
  )
}

rodeo_transform_method_label <- function(method) {
  switch(method,
    yeojohnson = "YeoJohnson",
    boxcox = "BoxCox",
    asinh = "Asinh",
    log = "Log",
    logplus1 = "LogPlus1",
    sqrt = "Sqrt",
    asin = "Asin",
    logit = "Logit",
    identity = "Identity",
    method)
}

rodeo_transform_forced_results <- function(dt, columns, method) {
  rows <- lapply(columns, function(col) {
    x <- dt[[col]]
    lambda <- NA_real_
    if (identical(method, "yeojohnson")) {
      lambda <- Estimate_YeoJohnson_Lambda(x)
      data.table::set(dt, j = col, value = Apply_YeoJohnson(x, lambda))
    } else if (identical(method, "boxcox")) {
      lambda <- Estimate_BoxCox_Lambda(x)
      data.table::set(dt, j = col, value = Apply_BoxCox(x, lambda))
    } else if (identical(method, "logplus1")) {
      xx <- min(x, na.rm = TRUE)
      lambda <- if (is.finite(xx) && xx <= 0) abs(xx) else 0
      data.table::set(dt, j = col, value = Apply_LogPlus1(x, AbsXX = lambda))
    } else if (identical(method, "log")) {
      data.table::set(dt, j = col, value = Apply_Log(x))
    } else if (identical(method, "sqrt")) {
      data.table::set(dt, j = col, value = Apply_Sqrt(x))
    } else if (identical(method, "asin")) {
      data.table::set(dt, j = col, value = Apply_Asin(x))
    } else if (identical(method, "asinh")) {
      data.table::set(dt, j = col, value = Apply_Asinh(x))
    } else if (identical(method, "logit")) {
      data.table::set(dt, j = col, value = Apply_Logit(x))
    }
    data.table::data.table(
      ColumnName = col,
      MethodName = rodeo_transform_method_label(method),
      Lambda = lambda,
      NormalizedStatistics = NA_real_
    )
  })
  data.table::rbindlist(rows, use.names = TRUE)
}

#' Fit numeric transforms, grouped scaling, or percentile ranks
#'
#' @description
#' Preferred transformation entry point. `method = "auto"` wraps
#' [AutoTransformationCreate()]; `standardize` and `perc_rank` wrap
#' [Standardize()] and [PercRank()]; named invertible transforms wrap
#' `Apply_*` / `Estimate_*`. Inverse application is available through
#' [predict()] when the fitted method is invertible.
#'
#' @param data Training data.
#' @param columns Numeric columns to transform.
#' @param method Transformation family.
#' @param group_vars Optional grouping columns for standardize/percentile rank.
#' @param center,scale Standardize controls.
#' @param granularity Percentile-rank granularity.
#' @param copy_data Whether to copy before transforming.
#' @return A reusable `rodeo_transform_fit`. Apply with [predict()].
#' @export
rodeo_transform <- function(
  data,
  columns,
  method = c("auto", "standardize", "perc_rank", "yeojohnson", "boxcox",
    "asinh", "log", "logplus1", "sqrt", "asin", "logit", "identity"),
  group_vars = NULL,
  center = TRUE,
  scale = TRUE,
  granularity = 0.001,
  copy_data = TRUE
) {
  method <- match.arg(tolower(as.character(method)[[1L]]),
    c("auto", "standardize", "perc_rank", "yeojohnson", "boxcox", "asinh",
      "log", "logplus1", "sqrt", "asin", "logit", "identity"))
  columns <- as.character(columns)
  group_vars <- if (is.null(group_vars)) NULL else as.character(group_vars)
  if (!length(columns)) stop("columns must name at least one numeric column.",
    call. = FALSE)
  dt <- rodeo_preferred_copy(data, copy_data)
  missing_cols <- setdiff(c(columns, group_vars), names(dt))
  if (length(missing_cols)) {
    stop(paste("columns not found:", paste(missing_cols, collapse = ", ")),
      call. = FALSE)
  }

  if (identical(method, "standardize")) {
    if (length(group_vars)) {
      out_std <- Standardize(dt, ColNames = columns, GroupVars = group_vars,
        Center = center, Scale = scale, ScoreTable = TRUE)
      dt <- out_std$data
      score_table <- out_std$ScoreTable
    } else {
      means <- vapply(columns, function(col) mean(dt[[col]], na.rm = TRUE), numeric(1L))
      sds <- vapply(columns, function(col) stats::sd(dt[[col]], na.rm = TRUE), numeric(1L))
      score_table <- data.table::as.data.table(c(
        stats::setNames(as.list(means), paste0(columns, "_mean")),
        stats::setNames(as.list(sds), paste0(columns, "_sd"))
      ))
      dt <- Standardize(dt, ColNames = columns, GroupVars = NULL,
        Center = center, Scale = scale, ScoreTable = FALSE)
    }
    features <- paste0(columns, "_Standardize")
    fitted_state <- list(method = method, columns = columns,
      group_vars = group_vars, center = center, scale = scale,
      score_table = score_table, invertible = TRUE)
  } else if (identical(method, "perc_rank")) {
    out_pr <- PercRank(dt, ColNames = columns, GroupVars = group_vars,
      Granularity = granularity, ScoreTable = TRUE)
    dt <- out_pr$data
    features <- paste0(columns, "_PercRank")
    fitted_state <- list(method = method, columns = columns,
      group_vars = group_vars, granularity = granularity,
      score_table = out_pr$ScoreTable, invertible = FALSE)
  } else if (identical(method, "auto")) {
    created <- AutoTransformationCreate(dt, ColumnNames = columns,
      Methods = c("BoxCox", "YeoJohnson", "Asinh", "Log", "LogPlus1", "Sqrt",
        "Asin", "Logit", "Identity"),
      Path = NULL, TransID = "rodeo_transform", SaveOutput = FALSE)
    dt <- created$Data
    features <- columns
    fitted_state <- list(method = method, columns = columns,
      final_results = created$FinalResults, invertible = TRUE)
  } else {
    results <- rodeo_transform_forced_results(dt, columns, method)
    features <- columns
    fitted_state <- list(method = method, columns = columns,
      final_results = results, invertible = !identical(method, "identity"))
  }

  out <- list(
    data = dt,
    fitted_state = fitted_state,
    feature_manifest = rodeo_preferred_manifest(features, paste(columns, collapse = ","),
      "numeric_transform", method, TRUE),
    diagnostics = data.table::data.table(
      check = "method", status = "ok", detail = method
    ),
    warnings = character(),
    call = match.call()
  )
  class(out) <- c("rodeo_transform_fit", "list")
  out
}

#' Apply or invert a fitted Rodeo transformation
#' @param object A fitted object returned by [rodeo_transform()].
#' @param newdata New scoring data.
#' @param inverse If `TRUE`, invert an invertible fitted transform.
#' @param copy_data Whether to copy before transforming.
#' @param ... Reserved for compatible extensions.
#' @return Transformed scoring data using retained parameters.
#' @export
predict.rodeo_transform_fit <- function(object, newdata, inverse = FALSE,
    copy_data = TRUE, ...) {
  state <- object$fitted_state
  dt <- rodeo_preferred_copy(newdata, copy_data)
  if (isTRUE(inverse) && !isTRUE(state$invertible)) {
    stop("Fitted method '", state$method, "' is not invertible.", call. = FALSE)
  }
  if (identical(state$method, "standardize")) {
    if (length(state$group_vars)) {
      apply_flag <- if (isTRUE(inverse)) "backtransform" else "apply"
      return(StandardizeScoring(dt, ScoreTable = data.table::copy(state$score_table),
        Apply = apply_flag, GroupVars = state$group_vars))
    }
    for (col in state$columns) {
      mu <- state$score_table[[paste0(col, "_mean")]][[1L]]
      sdv <- state$score_table[[paste0(col, "_sd")]][[1L]]
      if (!is.finite(sdv) || sdv == 0) sdv <- 1
      if (isTRUE(inverse)) {
        std_col <- paste0(col, "_Standardize")
        src <- if (std_col %in% names(dt)) dt[[std_col]] else dt[[col]]
        data.table::set(dt, j = col, value = src * sdv + mu)
      } else {
        data.table::set(dt, j = paste0(col, "_Standardize"),
          value = (dt[[col]] - mu) / sdv)
      }
    }
    return(dt)
  }
  if (identical(state$method, "perc_rank")) {
    if (isTRUE(inverse)) stop("Percentile ranks are not invertible.", call. = FALSE)
    return(PercRankScoring(dt, ScoreTable = data.table::copy(state$score_table),
      GroupVars = state$group_vars, RollDirection = "forward"))
  }
  AutoTransformationScore(dt, FinalResults = state$final_results,
    Type = if (isTRUE(inverse)) "Inverse" else "Apply",
    TransID = "rodeo_transform", Path = NULL)
}

#' Fit general grouped lag, rolling, mode, or difference features
#'
#' @description
#' Preferred cross-row entry point outside forecast framing. Wraps
#' [DT_GDL_Feature_Engineering()], [AutoLagRollMode()], and [AutoDiffLagN()].
#' Retained history is concatenated at scoring time so lags are not refit from
#' the scoring window alone.
#'
#' @param data Training history.
#' @param date_col Date/time column.
#' @param targets Columns to lag, roll, or difference.
#' @param method Feature family.
#' @param group_vars Optional panel keys.
#' @param lags Lag periods.
#' @param roll_windows Rolling windows.
#' @param roll_stats Rolling statistics for `method = "stats"`.
#' @param type `"Lag"` or `"Lead"` for lag/mode families.
#' @param n_lag1,n_lag2 Difference offsets for `method = "diff"`.
#' @param simple_impute Whether to fill generated NA values.
#' @param time_unit Time aggregation label passed to GDL.
#' @param copy_data Whether to copy before generating features.
#' @return A reusable `rodeo_cross_row_features_fit`. Apply with [predict()].
#' @export
rodeo_cross_row_features <- function(
  data,
  date_col,
  targets,
  method = c("stats", "mode", "diff"),
  group_vars = NULL,
  lags = 1L,
  roll_windows = integer(),
  roll_stats = "mean",
  type = c("Lag", "Lead"),
  n_lag1 = 0L,
  n_lag2 = 1L,
  simple_impute = TRUE,
  time_unit = "days",
  copy_data = TRUE
) {
  method <- match.arg(tolower(as.character(method)[[1L]]), c("stats", "mode", "diff"))
  type <- match.arg(type)
  date_col <- as.character(date_col)[[1L]]
  targets <- as.character(targets)
  group_vars <- if (is.null(group_vars)) NULL else as.character(group_vars)
  lags <- as.integer(lags)
  roll_windows <- as.integer(roll_windows)
  dt <- rodeo_preferred_copy(data, copy_data)
  needed <- c(date_col, targets, group_vars)
  missing_cols <- setdiff(needed, names(dt))
  if (length(missing_cols)) {
    stop(paste("columns not found:", paste(missing_cols, collapse = ", ")),
      call. = FALSE)
  }
  history <- data.table::copy(dt)
  dt <- rodeo_cross_row_apply(dt, method, date_col, targets, group_vars, lags,
    roll_windows, roll_stats, type, n_lag1, n_lag2, simple_impute, time_unit)
  generated <- setdiff(names(dt), names(history))
  out <- list(
    data = dt,
    fitted_state = list(
      method = method, date_col = date_col, targets = targets,
      group_vars = group_vars, lags = lags, roll_windows = roll_windows,
      roll_stats = as.character(roll_stats), type = type, n_lag1 = as.integer(n_lag1),
      n_lag2 = as.integer(n_lag2), simple_impute = isTRUE(simple_impute),
      time_unit = as.character(time_unit)[[1L]], history = history
    ),
    feature_manifest = rodeo_preferred_manifest(generated,
      paste(c(date_col, targets, group_vars), collapse = ","),
      "cross_row", method, TRUE),
    diagnostics = data.table::data.table(
      check = c("history_rows", "generated_features"),
      status = "ok",
      detail = c(nrow(history), paste(generated, collapse = ", "))
    ),
    warnings = character(),
    call = match.call()
  )
  class(out) <- c("rodeo_cross_row_features_fit", "list")
  out
}

rodeo_cross_row_apply <- function(dt, method, date_col, targets, group_vars, lags,
    roll_windows, roll_stats, type, n_lag1, n_lag2, simple_impute, time_unit) {
  if (identical(method, "stats")) {
    mean_windows <- if ("mean" %in% roll_stats) roll_windows else 0
    sd_windows <- if ("sd" %in% roll_stats) roll_windows else 0
    skew_windows <- if ("skew" %in% roll_stats) roll_windows else 0
    kurt_windows <- if ("kurt" %in% roll_stats) roll_windows else 0
    return(DT_GDL_Feature_Engineering(
      dt,
      lags = if (length(lags)) lags else 1L,
      periods = mean_windows,
      SDperiods = sd_windows,
      Skewperiods = skew_windows,
      Kurtperiods = kurt_windows,
      Quantileperiods = 0,
      statsFUNs = roll_stats,
      targets = targets,
      groupingVars = group_vars,
      sortDateName = date_col,
      timeDiffTarget = NULL,
      timeAgg = time_unit,
      WindowingLag = 0,
      ShortName = TRUE,
      Type = type,
      SimpleImpute = simple_impute
    ))
  }
  if (identical(method, "mode")) {
    return(AutoLagRollMode(
      dt,
      Lags = if (length(lags)) lags else 1L,
      ModePeriods = if (length(roll_windows)) roll_windows else 0,
      Targets = targets,
      GroupingVars = group_vars,
      SortDateName = date_col,
      WindowingLag = 0,
      Type = type,
      SimpleImpute = simple_impute,
      Debug = FALSE
    ))
  }
  AutoDiffLagN(
    dt,
    DateVariable = date_col,
    GroupVariables = group_vars,
    DiffVariables = targets,
    DiffDateVariables = NULL,
    DiffGroupVariables = NULL,
    NLag1 = n_lag1,
    NLag2 = n_lag2,
    Type = "lag",
    Sort = TRUE,
    RemoveNA = FALSE
  )
}

#' Apply retained cross-row features to new rows
#' @param object A fitted object returned by [rodeo_cross_row_features()].
#' @param newdata New rows. Training history is concatenated automatically.
#' @param copy_data Whether to copy before generating features.
#' @param ... Reserved for compatible extensions.
#' @return Scoring rows with lag/roll/mode/diff features.
#' @export
predict.rodeo_cross_row_features_fit <- function(object, newdata, copy_data = TRUE, ...) {
  state <- object$fitted_state
  new_dt <- rodeo_preferred_copy(newdata, copy_data)
  new_dt[, .rodeo_src := "score"]
  hist <- data.table::copy(state$history)
  hist[, .rodeo_src := "train"]
  key_cols <- c(state$group_vars, state$date_col)
  if (length(key_cols) && all(key_cols %in% names(hist)) &&
      all(key_cols %in% names(new_dt))) {
    hist <- hist[!new_dt, on = key_cols]
  }
  combined <- data.table::rbindlist(list(hist, new_dt), use.names = TRUE, fill = TRUE)
  scored <- rodeo_cross_row_apply(combined, state$method, state$date_col,
    state$targets, state$group_vars, state$lags, state$roll_windows,
    state$roll_stats, state$type, state$n_lag1, state$n_lag2,
    state$simple_impute, state$time_unit)
  scored[scored$.rodeo_src == "score"][, .rodeo_src := NULL][]
}

#' Fit time-grid completion and rolling fill
#'
#' @description
#' Preferred time-completion entry point. Wraps [TimeSeriesFill()] and
#' [TimeSeriesFillRoll()] and retains the fill specification, group keys, and
#' training calendar range for replay.
#'
#' @param data Training panel.
#' @param date_col Date/time column.
#' @param group_vars Optional panel keys.
#' @param time_unit Calendar step, e.g. `"days"`.
#' @param fill_type Grid-fill range policy passed to [TimeSeriesFill()].
#' @param method `"grid"` or `"roll"`.
#' @param target_col Optional target column for grid fill.
#' @param roll_vars,non_roll_vars Columns for rolling fill.
#' @param roll_direction Rolling-fill direction.
#' @param max_missing_percent Grid-fill missingness threshold.
#' @param simple_impute Whether to impute remaining gaps.
#' @param copy_data Whether to copy before filling.
#' @return A reusable `rodeo_complete_time_fit`. Apply with [predict()].
#' @export
rodeo_complete_time <- function(
  data,
  date_col,
  group_vars = NULL,
  time_unit = "days",
  fill_type = c("maxmax", "minmax", "maxmin", "minmin"),
  method = c("grid", "roll"),
  target_col = NULL,
  roll_vars = NULL,
  non_roll_vars = NULL,
  roll_direction = "backward",
  max_missing_percent = 0.05,
  simple_impute = FALSE,
  copy_data = TRUE
) {
  method <- match.arg(method)
  fill_type <- match.arg(fill_type)
  date_col <- as.character(date_col)[[1L]]
  group_vars <- if (is.null(group_vars)) NULL else as.character(group_vars)
  dt <- rodeo_preferred_copy(data, copy_data)
  missing_cols <- setdiff(c(date_col, group_vars, target_col, roll_vars, non_roll_vars),
    names(dt))
  if (length(missing_cols)) {
    stop(paste("columns not found:", paste(missing_cols, collapse = ", ")),
      call. = FALSE)
  }
  group_keys <- if (length(group_vars)) unique(dt[, group_vars, with = FALSE]) else
    data.table::data.table()
  date_range <- range(dt[[date_col]], na.rm = TRUE)
  filled <- rodeo_complete_time_apply(dt, method, date_col, group_vars, time_unit,
    fill_type, target_col, roll_vars, non_roll_vars, roll_direction,
    max_missing_percent, simple_impute)
  out <- list(
    data = filled,
    fitted_state = list(
      method = method, date_col = date_col, group_vars = group_vars,
      time_unit = as.character(time_unit)[[1L]], fill_type = fill_type,
      target_col = target_col, roll_vars = roll_vars,
      non_roll_vars = non_roll_vars, roll_direction = roll_direction,
      max_missing_percent = max_missing_percent,
      simple_impute = isTRUE(simple_impute), group_keys = group_keys,
      date_range = date_range
    ),
    feature_manifest = rodeo_preferred_manifest(
      date_col, paste(c(date_col, group_vars), collapse = ","),
      "time_complete", method, TRUE),
    diagnostics = data.table::data.table(
      check = c("input_rows", "completed_rows", "groups"),
      status = "ok",
      detail = c(nrow(dt), nrow(filled),
        if (nrow(group_keys)) nrow(group_keys) else 0L)
    ),
    warnings = character(),
    call = match.call()
  )
  class(out) <- c("rodeo_complete_time_fit", "list")
  out
}

rodeo_complete_time_apply <- function(dt, method, date_col, group_vars, time_unit,
    fill_type, target_col, roll_vars, non_roll_vars, roll_direction,
    max_missing_percent, simple_impute) {
  if (identical(method, "roll")) {
    return(TimeSeriesFillRoll(
      data = dt,
      DateColumnName = date_col,
      RollVars = roll_vars,
      NonRollVars = non_roll_vars,
      GroupVariables = group_vars,
      RollDirection = roll_direction,
      TimeUnit = time_unit,
      SimpleImpute = simple_impute
    ))
  }
  TimeSeriesFill(
    data = dt,
    TargetColumn = target_col,
    DateColumnName = date_col,
    GroupVariables = group_vars,
    TimeUnit = time_unit,
    FillType = fill_type,
    MaxMissingPercent = max_missing_percent,
    SimpleImpute = simple_impute
  )
}

#' Apply retained time-grid completion
#' @param object A fitted object returned by [rodeo_complete_time()].
#' @param newdata Panel to complete. Training group keys are unioned in.
#' @param copy_data Whether to copy before filling.
#' @param ... Reserved for compatible extensions.
#' @return Completed time grid.
#' @export
predict.rodeo_complete_time_fit <- function(object, newdata, copy_data = TRUE, ...) {
  state <- object$fitted_state
  dt <- rodeo_preferred_copy(newdata, copy_data)
  if (nrow(state$group_keys) && length(state$group_vars) &&
      all(state$group_vars %in% names(dt))) {
    missing_groups <- state$group_keys[!dt, on = state$group_vars]
    if (nrow(missing_groups)) {
      stub <- data.table::copy(missing_groups)
      stub[, (state$date_col) := state$date_range[[1L]]]
      dt <- data.table::rbindlist(list(dt, stub), use.names = TRUE, fill = TRUE)
    }
  }
  rodeo_complete_time_apply(dt, state$method, state$date_col, state$group_vars,
    state$time_unit, state$fill_type, state$target_col, state$roll_vars,
    state$non_roll_vars, state$roll_direction, state$max_missing_percent,
    state$simple_impute)
}

#' QA the preferred Rodeo expert API
#' @return Compact test table.
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

  encode_fit <- rodeo_encode(train, columns = "category", method = "helmert")
  encode_scored <- predict(encode_fit, train[1:3])
  cred_fit <- rodeo_encode(train, columns = "category", method = "credibility",
    target = "target", ml_type = "classification")
  transform_fit <- rodeo_transform(train, columns = "x", method = "standardize")
  transform_scored <- predict(transform_fit, train[1:3])
  log_fit <- rodeo_transform(train, columns = "x", method = "log")
  log_roundtrip <- predict(log_fit, log_fit$data, inverse = TRUE)
  cross_fit <- rodeo_cross_row_features(train, date_col = "date", targets = "x",
    method = "stats", lags = 1L, roll_windows = 3L, roll_stats = "mean")
  future <- data.table::data.table(
    id = 13L, x = 13, category = "a", target = 0, date = as.Date("2024-01-13"))
  cross_scored <- predict(cross_fit, future)
  gappy <- train[c(1, 2, 4, 7, 12)]
  complete_fit <- rodeo_complete_time(gappy, date_col = "date", time_unit = "days")
  complete_scored <- predict(complete_fit, gappy)

  exported <- tryCatch(getNamespaceExports("Rodeo"), error = function(e) {
    ns <- readLines(system.file("NAMESPACE", package = "Rodeo"), warn = FALSE)
    if (!length(ns) || identical(ns, "")) {
      ns_path <- file.path(getwd(), "NAMESPACE")
      if (file.exists(ns_path)) ns <- readLines(ns_path, warn = FALSE)
    }
    sub("^export\\(([^)]+)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
  })
  unsafe <- intersect(rodeo_preferred_unsafe_exports(), exported)
  lag_cols <- grep("LAG|lag", names(cross_scored), value = TRUE)

  data.table::data.table(
    test = c("tabular_fit", "scoring_state", "supervised_lineage",
      "temporal_fit", "temporal_application", "partition_replay",
      "encode_fit", "encode_scoring_maps", "encode_credibility",
      "transform_fit", "transform_scoring", "transform_inverse",
      "cross_row_fit", "cross_row_history", "complete_time_fit",
      "leakage_unsafe_unexported"),
    passed = c(inherits(feature_fit, "rodeo_features_fit"),
      "category_woe" %in% names(scored),
      any(feature_fit$feature_manifest$family == "supervised_encoding"),
      inherits(temporal_fit, "rodeo_temporal_features_fit"),
      all(c("target_lag_1", "target_roll_mean_3") %in% names(temporal_scored)),
      identical(prep_fit$data$.partition, prep_replay$.partition),
      inherits(encode_fit, "rodeo_encode_fit") && length(encode_fit$fitted_state$encoding_maps) > 0L,
      ncol(encode_scored) > ncol(train[1:3]),
      inherits(cred_fit, "rodeo_encode_fit") && "category_Credibility" %in% names(cred_fit$data),
      inherits(transform_fit, "rodeo_transform_fit") && "x_Standardize" %in% names(transform_fit$data),
      "x_Standardize" %in% names(transform_scored),
      isTRUE(all.equal(as.numeric(log_roundtrip$x), as.numeric(train$x),
        tolerance = 1e-8)),
      inherits(cross_fit, "rodeo_cross_row_features_fit"),
      nrow(cross_scored) == 1L && length(lag_cols) > 0L,
      inherits(complete_fit, "rodeo_complete_time_fit") &&
        nrow(complete_fit$data) >= nrow(gappy) &&
        nrow(complete_scored) >= nrow(gappy),
      !length(unsafe)),
    detail = c("preferred tabular fit", "unseen category uses retained state",
      "target-derived feature is traceable", "preferred temporal fit",
      "temporal state applies without refit", "partition identities are stable",
      "encoding maps retained in memory", "scoring uses retained maps",
      "credibility encoding remains available", "standardize fit retains score table",
      "standardize scoring does not refit", "log inverse recovers training values",
      "cross-row GDL fit", "scoring concatenates retained history",
      "time grid completion is replayable",
      if (length(unsafe)) paste("still exported:", paste(unsafe, collapse = ", "))
        else "leakage-unsafe and internal names are not exported")
  )
}
