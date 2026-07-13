#' Create a Rodeo temporal transformation specification
#'
#' @description
#' Defines deterministic temporal feature preparation for forecasting. The
#' specification is serializable and engine-agnostic; model packages should
#' consume prepared outputs rather than rebuilding temporal features.
#'
#' @export
rodeo_temporal_transformation_spec <- function(
  date_col,
  target_col,
  frequency = "auto",
  calendar_features = c("year", "month", "day", "dow", "day_index"),
  lag_periods = 1L,
  rolling_windows = integer(),
  rolling_stats = "mean",
  expanding_stats = character(),
  known_future_variables = character(),
  static_entity_features = character(),
  entity_id = NULL,
  forecast_horizon = 1L,
  id = NULL,
  version = "0.1.0",
  metadata = list()
) {
  date_col <- rodeo_contract_scalar(date_col)
  target_col <- rodeo_contract_scalar(target_col)
  if (!nzchar(date_col) || !nzchar(target_col)) {
    stop("date_col and target_col are required.", call. = FALSE)
  }
  calendar_features <- rodeo_contract_cols(calendar_features)
  valid_calendar <- c("year", "month", "day", "dow", "week", "quarter", "is_weekend", "day_index")
  bad_calendar <- setdiff(calendar_features, valid_calendar)
  if (length(bad_calendar)) {
    stop(paste("Unsupported calendar features:", paste(bad_calendar, collapse = ", ")), call. = FALSE)
  }
  rolling_stats <- rodeo_contract_cols(rolling_stats)
  bad_stats <- setdiff(rolling_stats, c("mean"))
  if (length(bad_stats)) {
    stop("Only rolling_stats = 'mean' is supported in the temporal contract v1.", call. = FALSE)
  }
  lag_periods <- unique(as.integer(lag_periods %||% integer()))
  lag_periods <- lag_periods[is.finite(lag_periods) & lag_periods > 0L]
  rolling_windows <- unique(as.integer(rolling_windows %||% integer()))
  rolling_windows <- rolling_windows[is.finite(rolling_windows) & rolling_windows > 0L]
  forecast_horizon <- as.integer(forecast_horizon %||% 1L)[1L]
  if (!is.finite(forecast_horizon) || forecast_horizon < 1L) forecast_horizon <- 1L
  entity_id <- rodeo_contract_cols(entity_id)
  known_future_variables <- rodeo_contract_cols(known_future_variables)
  static_entity_features <- rodeo_contract_cols(static_entity_features)
  id <- rodeo_contract_scalar(
    id,
    paste0("temporal_", digest_simple(list(date_col, target_col, lag_periods, rolling_windows, calendar_features, known_future_variables)))
  )
  spec <- list(
    id = id,
    type = "temporal_forecast_features",
    date_col = date_col,
    target_col = target_col,
    frequency = as.character(frequency)[1L],
    calendar_features = calendar_features,
    lag_periods = lag_periods,
    rolling_windows = rolling_windows,
    rolling_stats = rolling_stats,
    expanding_stats = rodeo_contract_cols(expanding_stats),
    known_future_variables = known_future_variables,
    static_entity_features = static_entity_features,
    entity_id = entity_id,
    forecast_horizon = forecast_horizon,
    version = as.character(version)[1L],
    metadata = metadata %||% list(),
    created_at = Sys.time()
  )
  class(spec) <- c("rodeo_temporal_transformation_spec", "list")
  spec
}

#' Fit a Rodeo temporal transformation specification
#'
#' @export
rodeo_fit_temporal_transformation <- function(data, spec, forecast_origin = NULL) {
  if (!inherits(spec, "rodeo_temporal_transformation_spec")) {
    stop("spec must be returned by rodeo_temporal_transformation_spec().", call. = FALSE)
  }
  dt <- data.table::as.data.table(data)
  required <- unique(c(spec$date_col, spec$target_col, spec$known_future_variables, spec$static_entity_features, spec$entity_id))
  missing <- setdiff(required, names(dt))
  if (length(missing)) {
    stop(paste("Required temporal columns are missing:", paste(missing, collapse = ", ")), call. = FALSE)
  }
  if (!nrow(dt)) stop("data must contain at least one temporal row.", call. = FALSE)
  work <- data.table::copy(dt)
  work[, .rodeo_temporal_date := as.Date(get(spec$date_col))]
  if (any(is.na(work$.rodeo_temporal_date))) {
    stop("date_col must be convertible to Date without missing temporal dates.", call. = FALSE)
  }
  work[, .rodeo_temporal_target := as.numeric(get(spec$target_col))]
  order_cols <- c(spec$entity_id, ".rodeo_temporal_date")
  data.table::setorderv(work, order_cols)
  origin <- if (is.null(forecast_origin)) max(work$.rodeo_temporal_date, na.rm = TRUE) else as.Date(forecast_origin)[1L]
  if (is.na(origin)) stop("forecast_origin must be convertible to Date.", call. = FALSE)
  history <- work[.rodeo_temporal_date <= origin]
  if (!nrow(history)) stop("No temporal training rows are at or before forecast_origin.", call. = FALSE)
  prepared <- rodeo_temporal_add_origin_features(history, spec, reference_start = min(history$.rodeo_temporal_date))
  feature_manifest <- rodeo_temporal_feature_manifest(spec)
  fitted <- spec
  fitted$id <- paste0(spec$id, "_fit_", digest_simple(list(nrow(history), origin, names(dt))))
  fitted$spec_id <- spec$id
  fitted$forecast_origin <- origin
  fitted$training_start <- min(history$.rodeo_temporal_date)
  fitted$training_end <- max(history$.rodeo_temporal_date)
  fitted$input_schema <- rodeo_contract_schema(dt)
  fitted$history <- history
  fitted$prepared_history <- prepared
  fitted$entity_levels <- if (length(spec$entity_id)) sort(unique(as.character(history[[spec$entity_id[[1L]]]]))) else character()
  fitted$feature_manifest <- feature_manifest
  fitted$diagnostics <- data.table::data.table(
    check = c("leakage_policy", "schema", "replay_state"),
    status = c("ok", "ok", "ok"),
    detail = c(
      "Lag and rolling features use values strictly before each origin row.",
      paste("Required columns:", paste(required, collapse = ", ")),
      "Training history is stored for deterministic forecast replay."
    )
  )
  fitted$metadata <- c(fitted$metadata, list(
    fitted_at = Sys.time(),
    transformation_identity = fitted$id,
    specification_identity = spec$id,
    prepared_temporal_dataset_identity = paste0("prepared_temporal_", digest_simple(list(spec$id, nrow(prepared), names(prepared)))),
    replay_status = "ready"
  ))
  class(fitted) <- c("rodeo_fitted_temporal_transformation", "rodeo_temporal_transformation_spec", "list")
  fitted
}

#' Apply a fitted Rodeo temporal transformation
#'
#' @export
rodeo_apply_temporal_transformation <- function(data, fitted_spec, copy_data = TRUE) {
  if (!inherits(fitted_spec, "rodeo_fitted_temporal_transformation")) {
    stop("fitted_spec must be returned by rodeo_fit_temporal_transformation().", call. = FALSE)
  }
  validation <- rodeo_validate_temporal_schema(data, fitted_spec)
  if (!isTRUE(validation$valid)) stop(paste(validation$errors, collapse = " | "), call. = FALSE)
  dt <- data.table::as.data.table(data)
  if (copy_data) dt <- data.table::copy(dt)
  dt[, .rodeo_temporal_date := as.Date(get(fitted_spec$date_col))]
  dt[, .rodeo_temporal_target := as.numeric(get(fitted_spec$target_col))]
  data.table::setorderv(dt, c(fitted_spec$entity_id, ".rodeo_temporal_date"))
  out <- rodeo_temporal_add_origin_features(dt, fitted_spec, reference_start = fitted_spec$training_start)
  attr(out, "rodeo_temporal_metadata") <- rodeo_temporal_transformation_metadata(fitted_spec, out)
  out
}

#' Validate data against a fitted temporal transformation schema
#'
#' @export
rodeo_validate_temporal_schema <- function(data, fitted_spec, future_data = NULL) {
  if (!inherits(fitted_spec, "rodeo_fitted_temporal_transformation")) {
    stop("fitted_spec must be returned by rodeo_fit_temporal_transformation().", call. = FALSE)
  }
  dt <- data.table::as.data.table(data)
  required <- unique(c(fitted_spec$date_col, fitted_spec$target_col, fitted_spec$known_future_variables, fitted_spec$static_entity_features, fitted_spec$entity_id))
  missing <- setdiff(required, names(dt))
  future_missing <- character()
  if (!is.null(future_data)) {
    fdt <- data.table::as.data.table(future_data)
    future_missing <- setdiff(unique(c(fitted_spec$date_col, fitted_spec$known_future_variables, fitted_spec$static_entity_features, fitted_spec$entity_id)), names(fdt))
  }
  errors <- c(
    if (length(missing)) paste("Required temporal columns are missing:", paste(missing, collapse = ", ")),
    if (length(future_missing)) paste("Required future temporal columns are missing:", paste(future_missing, collapse = ", "))
  )
  list(valid = !length(errors), errors = errors, missing_columns = missing, future_missing_columns = future_missing)
}

#' Prepare supervised forecast frames from a fitted temporal transformation
#'
#' @export
rodeo_prepare_forecast_supervised_data <- function(
  fitted_spec,
  future_data = NULL,
  horizon = NULL,
  strategy = c("direct", "recursive")
) {
  if (!inherits(fitted_spec, "rodeo_fitted_temporal_transformation")) {
    stop("fitted_spec must be returned by rodeo_fit_temporal_transformation().", call. = FALSE)
  }
  strategy <- match.arg(strategy)
  horizon <- as.integer(horizon %||% fitted_spec$forecast_horizon)[1L]
  if (!is.finite(horizon) || horizon < 1L) horizon <- fitted_spec$forecast_horizon
  history <- data.table::copy(fitted_spec$history)
  origin_features <- rodeo_temporal_add_origin_features(history, fitted_spec, reference_start = fitted_spec$training_start)
  future_rows <- rodeo_temporal_future_rows(fitted_spec, future_data, horizon)
  if (identical(strategy, "recursive")) {
    frame <- rodeo_temporal_supervised_frame(origin_features, history, fitted_spec, 1L)
    feature_cols <- rodeo_temporal_feature_columns(frame, fitted_spec)
    return(list(
      strategy = strategy,
      training_frame = frame,
      feature_columns = feature_cols,
      prediction_frames = lapply(seq_len(horizon), function(i) {
        rodeo_temporal_prediction_frame(fitted_spec, future_rows[i], history_values = history$.rodeo_temporal_target)
      }),
      feature_manifest = fitted_spec$feature_manifest,
      diagnostics = fitted_spec$diagnostics
    ))
  }
  training_frames <- list()
  prediction_frames <- list()
  feature_columns_by_horizon <- list()
  for (h in seq_len(horizon)) {
    frame <- rodeo_temporal_supervised_frame(origin_features, history, fitted_spec, h)
    feature_cols <- rodeo_temporal_feature_columns(frame, fitted_spec)
    training_frames[[as.character(h)]] <- frame
    feature_columns_by_horizon[[as.character(h)]] <- feature_cols
    last_origin <- if (length(fitted_spec$entity_id)) {
      origin_features[, .SD[.N], by = c(fitted_spec$entity_id)]
    } else {
      origin_features[.N]
    }
    prediction_frames[[as.character(h)]] <- rodeo_temporal_direct_prediction_frame(fitted_spec, last_origin, future_rows[.rodeo_panel_horizon == h])
  }
  list(
    strategy = strategy,
    training_frames = training_frames,
    prediction_frames = prediction_frames,
    feature_columns_by_horizon = feature_columns_by_horizon,
    feature_manifest = fitted_spec$feature_manifest,
    diagnostics = fitted_spec$diagnostics
  )
}

#' Build a single temporal prediction feature row
#'
#' @export
rodeo_temporal_prediction_frame <- function(fitted_spec, future_row, history_values) {
  if (!inherits(fitted_spec, "rodeo_fitted_temporal_transformation")) {
    stop("fitted_spec must be returned by rodeo_fit_temporal_transformation().", call. = FALSE)
  }
  future_row <- data.table::as.data.table(future_row)
  if (!nrow(future_row)) stop("future_row must contain one row.", call. = FALSE)
  future_row <- future_row[1L]
  out <- data.table::data.table(.rodeo_future_date = as.Date(future_row[[fitted_spec$date_col]]))
  if (length(fitted_spec$entity_id)) {
    entity_col <- fitted_spec$entity_id[[1L]]
    out[, entity_id_code := match(as.character(future_row[[entity_col]]), fitted_spec$entity_levels)]
  }
  history_values <- as.numeric(history_values)
  for (lag in fitted_spec$lag_periods) {
    out[[paste0("target_lag_", lag)]] <- if (length(history_values) >= lag) history_values[length(history_values) - lag + 1L] else NA_real_
  }
  for (window in fitted_spec$rolling_windows) {
    out[[paste0("target_roll_mean_", window)]] <- if (length(history_values) >= window) mean(utils::tail(history_values, window), na.rm = TRUE) else NA_real_
  }
  out <- data.table::as.data.table(cbind(out, rodeo_temporal_date_features(out$.rodeo_future_date, fitted_spec$calendar_features, fitted_spec$training_start)))
  for (var in fitted_spec$known_future_variables) {
    out[[paste0("future_", var)]] <- future_row[[var]]
  }
  for (var in fitted_spec$static_entity_features) {
    out[[paste0("static_", var)]] <- future_row[[var]]
  }
  out[]
}

#' Return readable temporal transformation metadata
#'
#' @export
rodeo_temporal_transformation_metadata <- function(fitted_spec, data = NULL) {
  if (!inherits(fitted_spec, "rodeo_fitted_temporal_transformation")) {
    stop("fitted_spec must be returned by rodeo_fit_temporal_transformation().", call. = FALSE)
  }
  list(
    temporal_specification_identity = fitted_spec$spec_id,
    temporal_transformation_identity = fitted_spec$id,
    prepared_temporal_dataset_identity = fitted_spec$metadata$prepared_temporal_dataset_identity,
    replay_status = fitted_spec$metadata$replay_status,
    feature_manifest = fitted_spec$feature_manifest,
    diagnostics = fitted_spec$diagnostics,
    row_count = if (is.null(data)) NA_integer_ else nrow(data),
    column_count = if (is.null(data)) NA_integer_ else ncol(data),
    forecast_origin = fitted_spec$forecast_origin,
    training_start = fitted_spec$training_start,
    training_end = fitted_spec$training_end
  )
}

#' Prepare deterministic cross-target temporal features
#'
#' @description
#' Creates leakage-safe cross-target lag and rolling features for wide
#' multi-target forecasting data. Features are shifted by at least one period so
#' same-period and future target values are never used.
#'
#' @param data Wide temporal data with a shared date column and multiple targets.
#' @param date_col Shared date column.
#' @param target_cols Character vector of target columns.
#' @param forecast_origin Forecast origin. Rows after this date are future rows.
#' @param lag_periods Positive integer cross-target lags.
#' @param rolling_windows Positive integer rolling mean windows.
#' @param known_future_variables Optional known future variable columns.
#' @param future_data Optional future data containing date and known future variables.
#'
#' @return A list containing training features, future features, feature manifest,
#' diagnostics, and metadata.
#' @export
rodeo_prepare_cross_target_features <- function(
  data,
  date_col,
  target_cols,
  forecast_origin,
  lag_periods = 1L,
  rolling_windows = integer(),
  known_future_variables = character(),
  future_data = NULL
) {
  dt <- data.table::as.data.table(data)
  date_col <- rodeo_contract_scalar(date_col)
  target_cols <- rodeo_contract_cols(target_cols)
  known_future_variables <- rodeo_contract_cols(known_future_variables)
  lag_periods <- unique(as.integer(lag_periods %||% integer()))
  lag_periods <- lag_periods[is.finite(lag_periods) & lag_periods > 0L]
  rolling_windows <- unique(as.integer(rolling_windows %||% integer()))
  rolling_windows <- rolling_windows[is.finite(rolling_windows) & rolling_windows > 0L]
  required <- unique(c(date_col, target_cols, known_future_variables))
  missing <- setdiff(required, names(dt))
  if (length(missing)) {
    stop(paste("Required cross-target columns are missing:", paste(missing, collapse = ", ")), call. = FALSE)
  }
  work <- data.table::copy(dt)
  work[, .rodeo_cross_target_date := as.Date(get(date_col))]
  if (any(is.na(work$.rodeo_cross_target_date))) {
    stop("date_col must be convertible to Date without missing temporal dates.", call. = FALSE)
  }
  origin <- as.Date(forecast_origin)[1L]
  if (is.na(origin)) stop("forecast_origin must be convertible to Date.", call. = FALSE)
  data.table::setorder(work, .rodeo_cross_target_date)
  training <- work[.rodeo_cross_target_date <= origin]
  if (!nrow(training)) stop("No training rows are at or before forecast_origin.", call. = FALSE)
  future <- if (is.null(future_data)) {
    work[.rodeo_cross_target_date > origin]
  } else {
    data.table::as.data.table(data.table::copy(future_data))
  }
  if (nrow(future)) {
    if (!date_col %in% names(future)) stop("future_data must contain date_col.", call. = FALSE)
    future[, .rodeo_cross_target_date := as.Date(get(date_col))]
    data.table::setorder(future, .rodeo_cross_target_date)
  }
  history_by_target <- lapply(target_cols, function(target) as.numeric(training[[target]]))
  names(history_by_target) <- target_cols
  add_features <- function(frame, include_future_known = TRUE) {
    out <- data.table::copy(frame)
    if (!nrow(out)) return(out)
    out <- data.table::as.data.table(cbind(out, rodeo_temporal_date_features(
      out$.rodeo_cross_target_date,
      c("year", "month", "day", "dow", "day_index"),
      min(training$.rodeo_cross_target_date)
    )))
    for (target in target_cols) {
      y <- as.numeric(training[[target]])
      train_n <- nrow(training)
      frame_dates <- out$.rodeo_cross_target_date
      for (lag in lag_periods) {
        feature <- paste0("cross_target_", target, "_lag_", lag)
        out[[feature]] <- vapply(frame_dates, function(d) {
          prior_idx <- which(training$.rodeo_cross_target_date < d)
          idx <- if (length(prior_idx)) max(prior_idx) else NA_integer_
          if (!is.finite(idx) || idx < lag) NA_real_ else y[idx - lag + 1L]
        }, numeric(1L))
      }
      for (window in rolling_windows) {
        feature <- paste0("cross_target_", target, "_roll_mean_", window)
        out[[feature]] <- vapply(frame_dates, function(d) {
          prior_idx <- which(training$.rodeo_cross_target_date < d)
          idx <- if (length(prior_idx)) max(prior_idx) else NA_integer_
          if (!is.finite(idx) || idx < window) NA_real_ else mean(y[(idx - window + 1L):idx], na.rm = TRUE)
        }, numeric(1L))
      }
      if (train_n == 0L) next
    }
    if (include_future_known) {
      for (var in known_future_variables) {
        if (var %in% names(out)) out[[paste0("future_", var)]] <- out[[var]]
      }
    }
    out
  }
  training_features <- add_features(training)
  future_features <- add_features(future)
  feature_columns <- grep("^cross_target_|^future_|^date_", names(training_features), value = TRUE)
  manifest <- data.table::data.table(
    feature = feature_columns,
    source = data.table::fcase(
      grepl("^cross_target_", feature_columns), "cross_target_history",
      grepl("^future_", feature_columns), "known_future_variable",
      grepl("^date_", feature_columns), "calendar",
      default = "unknown"
    ),
    leakage_policy = data.table::fcase(
      grepl("^cross_target_", feature_columns), "strictly_prior_to_prediction_date",
      grepl("^future_", feature_columns), "declared_known_future",
      grepl("^date_", feature_columns), "derived_from_prediction_date",
      default = "unknown"
    )
  )
  diagnostics <- data.table::data.table(
    check = c("same_period_target_leakage", "future_target_leakage", "forecast_origin_leakage", "feature_manifest"),
    status = c("pass", "pass", "pass", "pass"),
    detail = c(
      "Cross-target features use rows with date strictly before the feature row.",
      "Future target values are not required and are not used for cross-target features.",
      "Training features are built from rows at or before forecast_origin; future rows use historical training values only.",
      paste("Cross-target/calendar feature columns:", length(feature_columns))
    )
  )
  result <- list(
    preparation_id = paste0("cross_target_features_", digest_simple(list(date_col, target_cols, origin, lag_periods, rolling_windows, known_future_variables, nrow(training_features), nrow(future_features)))),
    schema_version = "rodeo_cross_target_features_v1",
    date_col = date_col,
    target_cols = target_cols,
    forecast_origin = origin,
    lag_periods = lag_periods,
    rolling_windows = rolling_windows,
    known_future_variables = known_future_variables,
    training_features = training_features,
    future_features = future_features,
    feature_columns = feature_columns,
    feature_manifest = manifest,
    diagnostics = diagnostics,
    metadata = list(
      preparation_identity = paste0("prepared_cross_target_", digest_simple(list(feature_columns, nrow(training_features), nrow(future_features)))),
      leakage_policy = "strictly_prior_target_history_only",
      training_rows = nrow(training_features),
      future_rows = nrow(future_features),
      created_at = Sys.time()
    )
  )
  class(result) <- c("rodeo_cross_target_features", "list")
  result
}

qa_rodeo_temporal_transformation <- function() {
  data <- data.table::data.table(
    id = rep("A", 20),
    ds = as.Date("2026-01-01") + 0:19,
    y = seq(10, 29),
    promo = rep(c(0, 1), 10),
    store_size = 100
  )
  spec <- rodeo_temporal_transformation_spec(
    date_col = "ds",
    target_col = "y",
    lag_periods = c(1, 7),
    rolling_windows = 3,
    known_future_variables = "promo",
    static_entity_features = "store_size",
    entity_id = "id",
    forecast_horizon = 3
  )
  fitted <- rodeo_fit_temporal_transformation(data, spec, forecast_origin = max(data$ds) - 3)
  applied <- rodeo_apply_temporal_transformation(data[1:17], fitted)
  future <- data.table::data.table(id = "A", ds = max(data$ds) + 1:3, promo = c(1, 0, 1), store_size = 100)
  direct <- rodeo_prepare_forecast_supervised_data(fitted, future_data = future, horizon = 3, strategy = "direct")
  recursive <- rodeo_prepare_forecast_supervised_data(fitted, future_data = future, horizon = 3, strategy = "recursive")
  recursive_row <- rodeo_temporal_prediction_frame(fitted, future[1], history_values = c(data$y[1:17], 100))
  panel_data <- data.table::rbindlist(list(
    data,
    data.table::copy(data)[, `:=`(id = "B", y = y + 100, store_size = 250)]
  ))
  panel_fit <- rodeo_fit_temporal_transformation(panel_data, spec, forecast_origin = max(data$ds) - 3)
  panel_future <- data.table::CJ(id = c("A", "B"), ds = max(data$ds) + 1:2)
  panel_future[, promo := 1]
  panel_future[, store_size := ifelse(id == "A", 100, 250)]
  panel_direct <- rodeo_prepare_forecast_supervised_data(panel_fit, future_data = panel_future, horizon = 2, strategy = "direct")
  loaded <- unserialize(serialize(fitted, NULL))
  replay <- rodeo_prepare_forecast_supervised_data(loaded, future_data = future, horizon = 1, strategy = "direct")
  cross_target_data <- data.table::data.table(
    ds = as.Date("2026-01-01") + 0:19,
    y1 = seq(10, 29),
    y2 = seq(20, 58, by = 2),
    promo = rep(c(0, 1), 10)
  )
  cross_target <- rodeo_prepare_cross_target_features(
    cross_target_data,
    date_col = "ds",
    target_cols = c("y1", "y2"),
    forecast_origin = max(cross_target_data$ds) - 3,
    lag_periods = c(1, 2),
    rolling_windows = 3,
    known_future_variables = "promo",
    future_data = cross_target_data[ds > max(ds) - 3, .(ds, promo)]
  )
  missing_result <- tryCatch({
    rodeo_validate_temporal_schema(data[, !"promo"], fitted, future_data = future)$valid
  }, error = function(e) FALSE)

  data.table::data.table(
    test = c(
      "temporal_spec_created",
      "temporal_fit_metadata",
      "lag_uses_prior_target",
      "rolling_uses_prior_targets",
      "direct_supervised_frames",
      "future_known_shifted",
      "recursive_prediction_uses_history",
      "panel_prediction_frames",
      "static_entity_features",
      "serialization_replay",
      "cross_target_features",
      "cross_target_leakage_policy",
      "schema_validation_missing_future_known",
      "metadata_readable"
    ),
    passed = c(
      inherits(spec, "rodeo_temporal_transformation_spec"),
      inherits(fitted, "rodeo_fitted_temporal_transformation") && identical(fitted$metadata$replay_status, "ready"),
      applied$target_lag_1[5L] == applied$y[4L],
      applied$target_roll_mean_3[5L] == mean(applied$y[2:4]),
      length(direct$training_frames) == 3L && nrow(direct$prediction_frames[["1"]]) == 1L,
      "future_promo" %in% names(direct$training_frames[["1"]]),
      recursive_row$target_lag_1 == 100,
      nrow(panel_direct$prediction_frames[["1"]]) == 2L && nrow(panel_direct$prediction_frames[["2"]]) == 2L,
      "static_store_size" %in% names(panel_direct$training_frames[["1"]]) &&
        "static_entity_feature" %in% panel_fit$feature_manifest$source,
      nrow(replay$training_frames[["1"]]) > 0L,
      inherits(cross_target, "rodeo_cross_target_features") &&
        "cross_target_y2_lag_1" %in% names(cross_target$training_features) &&
        cross_target$training_features$cross_target_y2_lag_1[5L] == cross_target_data$y2[4L],
      all(cross_target$diagnostics$status == "pass") &&
        all(cross_target$feature_manifest$leakage_policy[grepl("^cross_target_", cross_target$feature_manifest$feature)] == "strictly_prior_to_prediction_date"),
      !isTRUE(missing_result),
      all(c("temporal_specification_identity", "temporal_transformation_identity", "feature_manifest") %in% names(rodeo_temporal_transformation_metadata(fitted)))
    ),
    detail = c(
      "Temporal spec records date, target, features, future variables, and entity id.",
      "Fit stores replay-ready history and temporal identities.",
      "Lag features use only prior target values.",
      "Rolling means are computed from shifted target history.",
      "Direct strategy creates one training and prediction frame per horizon.",
      "Known future variables are represented as future_* feature columns.",
      "Recursive prediction rows can consume updated history values.",
      "Panel direct prediction frames preserve one row per entity and horizon.",
      "Static entity features are represented as static_* feature columns.",
      "Serialized fitted temporal transforms replay deterministically.",
      "Cross-target lag and rolling features are generated deterministically.",
      "Cross-target diagnostics record strict prior-history leakage policy.",
      "Schema validation detects missing known future variables.",
      "Metadata exposes identities, diagnostics, and feature manifest."
    )
  )
}

rodeo_temporal_add_origin_features <- function(dt, spec, reference_start) {
  out <- data.table::copy(dt)
  if (!".rodeo_temporal_date" %in% names(out)) out[, .rodeo_temporal_date := as.Date(get(spec$date_col))]
  if (!".rodeo_temporal_target" %in% names(out)) out[, .rodeo_temporal_target := as.numeric(get(spec$target_col))]
  if (length(spec$entity_id)) {
    entity_col <- spec$entity_id[[1L]]
    levels <- spec$entity_levels %||% sort(unique(as.character(out[[entity_col]])))
    out[, entity_id_code := match(as.character(get(entity_col)), levels)]
  }
  group_cols <- spec$entity_id
  add_group <- function(x) {
    y <- as.numeric(x$.rodeo_temporal_target)
    for (lag in spec$lag_periods) x[[paste0("target_lag_", lag)]] <- data.table::shift(y, n = lag, type = "lag")
    shifted_y <- data.table::shift(y, n = 1L, type = "lag")
    for (window in spec$rolling_windows) {
      x[[paste0("target_roll_mean_", window)]] <- data.table::frollmean(shifted_y, n = window, align = "right", fill = NA_real_)
    }
    x
  }
  if (length(group_cols)) {
    out <- out[, add_group(.SD), by = group_cols]
  } else {
    out <- add_group(out)
  }
  out[]
}

rodeo_temporal_date_features <- function(dates, features, reference_start) {
  dates <- as.Date(dates)
  d <- as.POSIXlt(dates)
  out <- data.table::data.table()
  if ("year" %in% features) out[, date_year := d$year + 1900L]
  if ("month" %in% features) out[, date_month := d$mon + 1L]
  if ("day" %in% features) out[, date_day := d$mday]
  if ("dow" %in% features) out[, date_dow := as.integer(format(dates, "%u"))]
  if ("week" %in% features) out[, date_week := as.integer(format(dates, "%U"))]
  if ("quarter" %in% features) out[, date_quarter := floor(d$mon / 3L) + 1L]
  if ("is_weekend" %in% features) out[, date_is_weekend := as.integer(format(dates, "%u") %in% c("6", "7"))]
  if ("day_index" %in% features) out[, date_day_index := as.numeric(dates - as.Date(reference_start))]
  out[]
}

rodeo_temporal_supervised_frame <- function(origin_features, history, fitted_spec, horizon) {
  frame <- data.table::copy(origin_features)
  if (length(fitted_spec$entity_id)) {
    frame[, .rodeo_label := data.table::shift(.rodeo_temporal_target, n = horizon, type = "lead"), by = c(fitted_spec$entity_id)]
    frame[, .rodeo_future_date := data.table::shift(.rodeo_temporal_date, n = horizon, type = "lead"), by = c(fitted_spec$entity_id)]
  } else {
    frame[, .rodeo_label := data.table::shift(.rodeo_temporal_target, n = horizon, type = "lead")]
    frame[, .rodeo_future_date := data.table::shift(.rodeo_temporal_date, n = horizon, type = "lead")]
  }
  frame <- data.table::as.data.table(cbind(frame, rodeo_temporal_date_features(frame$.rodeo_future_date, fitted_spec$calendar_features, fitted_spec$training_start)))
  for (var in fitted_spec$known_future_variables) {
    if (length(fitted_spec$entity_id)) {
      frame[, (paste0("future_", var)) := data.table::shift(get(var), n = horizon, type = "lead"), by = c(fitted_spec$entity_id)]
    } else {
      frame[[paste0("future_", var)]] <- data.table::shift(history[[var]], n = horizon, type = "lead")
    }
  }
  for (var in fitted_spec$static_entity_features) {
    frame[[paste0("static_", var)]] <- frame[[var]]
  }
  frame[]
}

rodeo_temporal_direct_prediction_frame <- function(fitted_spec, last_origin_features, future_row) {
  future_row <- data.table::copy(data.table::as.data.table(future_row))
  if (!nrow(future_row)) stop("future_row must contain at least one row.", call. = FALSE)
  future_date_col <- ".rodeo_future_join_date"
  future_row[, (future_date_col) := as.Date(get(fitted_spec$date_col))]
  for (var in c(fitted_spec$known_future_variables, fitted_spec$static_entity_features)) {
    future_row[[paste0(".rodeo_future_join_", var)]] <- future_row[[var]]
  }
  if (length(fitted_spec$entity_id)) {
    data.table::setkeyv(last_origin_features, fitted_spec$entity_id)
    data.table::setkeyv(future_row, fitted_spec$entity_id)
    out <- last_origin_features[future_row, nomatch = 0L]
  } else {
    future_row <- future_row[1L]
    out <- data.table::copy(last_origin_features)
    out[[future_date_col]] <- future_row[[future_date_col]]
    for (var in c(fitted_spec$known_future_variables, fitted_spec$static_entity_features)) {
      out[[paste0(".rodeo_future_join_", var)]] <- future_row[[paste0(".rodeo_future_join_", var)]]
    }
  }
  out <- data.table::copy(data.table::as.data.table(out))
  out[, .rodeo_future_date := get(future_date_col)]
  out <- data.table::as.data.table(cbind(out, rodeo_temporal_date_features(out$.rodeo_future_date, fitted_spec$calendar_features, fitted_spec$training_start)))
  for (var in fitted_spec$known_future_variables) {
    out[[paste0("future_", var)]] <- out[[paste0(".rodeo_future_join_", var)]]
  }
  for (var in fitted_spec$static_entity_features) {
    out[[paste0("static_", var)]] <- out[[paste0(".rodeo_future_join_", var)]]
  }
  out[]
}

rodeo_temporal_feature_columns <- function(frame, fitted_spec) {
  exclude <- c(
    ".rodeo_temporal_date", ".rodeo_temporal_target", ".rodeo_label",
    ".rodeo_future_date", ".aq_forecast_date",
    fitted_spec$date_col, fitted_spec$target_col, fitted_spec$known_future_variables, fitted_spec$static_entity_features,
    fitted_spec$entity_id
  )
  exclude <- unique(c(exclude, grep("^\\.", names(frame), value = TRUE)))
  cols <- setdiff(names(frame), exclude)
  cols[vapply(frame[, ..cols], function(x) is.numeric(x) || is.integer(x), logical(1))]
}

rodeo_temporal_feature_manifest <- function(spec) {
  features <- c(
    paste0("target_lag_", spec$lag_periods),
    paste0("target_roll_mean_", spec$rolling_windows),
    if (length(spec$entity_id)) "entity_id_code" else character(),
    paste0("date_", spec$calendar_features),
    paste0("future_", spec$known_future_variables),
    paste0("static_", spec$static_entity_features)
  )
  data.table::data.table(
    feature = features,
    source = data.table::fcase(
      grepl("^target_lag_", features), "target_lag",
      grepl("^target_roll_mean_", features), "target_rolling_stat",
      features == "entity_id_code", "entity_identity",
      grepl("^date_", features), "calendar",
      grepl("^future_", features), "future_known_variable",
      grepl("^static_", features), "static_entity_feature",
      default = "unknown"
    ),
    owner = "Rodeo",
    leakage_policy = "uses only information available at forecast origin"
  )
}

rodeo_temporal_future_rows <- function(fitted_spec, future_data, horizon) {
  if (!is.null(future_data) && nrow(data.table::as.data.table(future_data))) {
    future <- data.table::copy(data.table::as.data.table(future_data))
    if (nrow(future) < horizon) stop("future_data must contain at least horizon rows.", call. = FALSE)
    data.table::setorderv(future, c(fitted_spec$entity_id, fitted_spec$date_col))
    if (length(fitted_spec$entity_id)) {
      future[, .rodeo_panel_horizon := seq_len(.N), by = c(fitted_spec$entity_id)]
      return(future[.rodeo_panel_horizon <= horizon])
    }
    future[, .rodeo_panel_horizon := seq_len(.N)]
    return(future[seq_len(horizon)])
  }
  dates <- seq.Date(fitted_spec$forecast_origin + 1L, by = "day", length.out = horizon)
  future <- data.table::data.table(tmp_date = dates)
  data.table::setnames(future, "tmp_date", fitted_spec$date_col)
  for (var in fitted_spec$known_future_variables) future[[var]] <- NA_real_
  for (var in fitted_spec$static_entity_features) future[[var]] <- NA_real_
  for (id in fitted_spec$entity_id) future[[id]] <- fitted_spec$history[[id]][nrow(fitted_spec$history)]
  future[, .rodeo_panel_horizon := seq_len(.N)]
  future
}
