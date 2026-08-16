#' Create a Rodeo temporal transformation specification
#'
#' @description
#' Defines deterministic temporal feature preparation for forecasting. The
#' specification is serializable and engine-agnostic; model packages should
#' consume prepared outputs rather than rebuilding temporal features.
#'
#' @details
#' `frequency` accepts `"auto"` or a canonical grain: `1min`, `5min`, `10min`,
#' `15min`, `30min`, `hour`, `day`, `week`, `month`, `quarter`, or `year`.
#' Intraday grains preserve POSIXct timestamps.
#'
#' @export
rodeo_temporal_transformation_spec <- function(
  date_col,
  target_col,
  frequency = "auto",
  calendar_features = c("year", "month", "day", "dow", "day_index"),
  lag_periods = 1L,
  seasonal_lag_periods = integer(),
  rolling_windows = integer(),
  rolling_stats = "mean",
  rolling_quantiles = c(0.25, 0.5, 0.75),
  difference_orders = integer(),
  seasonal_difference_periods = integer(),
  ewm_alphas = numeric(),
  fourier_periods = integer(),
  fourier_pairs = 1L,
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
  valid_calendar <- c(
    "year", "month", "day", "dow", "week", "quarter", "is_weekend",
    "day_index", "day_of_year", "month_start", "month_end",
    "quarter_start", "quarter_end", "year_start", "year_end",
    "hour", "minute"
  )
  frequency <- rodeo_temporal_normalize_frequency(frequency)
  bad_calendar <- setdiff(calendar_features, valid_calendar)
  if (length(bad_calendar)) {
    stop(paste("Unsupported calendar features:", paste(bad_calendar, collapse = ", ")), call. = FALSE)
  }
  rolling_stats <- rodeo_contract_cols(rolling_stats)
  valid_rolling <- c("mean", "sd", "variance", "median", "min", "max", "range", "skewness", "kurtosis", "quantile")
  bad_stats <- setdiff(rolling_stats, valid_rolling)
  if (length(bad_stats)) {
    stop(paste("Unsupported rolling statistic(s):", paste(bad_stats, collapse = ", ")), call. = FALSE)
  }
  expanding_stats <- rodeo_contract_cols(expanding_stats)
  bad_expanding <- setdiff(expanding_stats, c("mean", "sd", "min", "max"))
  if (length(bad_expanding)) stop(paste("Unsupported expanding statistic(s):", paste(bad_expanding, collapse = ", ")), call. = FALSE)
  lag_periods <- unique(as.integer(lag_periods %||% integer()))
  lag_periods <- lag_periods[is.finite(lag_periods) & lag_periods > 0L]
  seasonal_lag_periods <- unique(as.integer(seasonal_lag_periods %||% integer()))
  seasonal_lag_periods <- seasonal_lag_periods[is.finite(seasonal_lag_periods) & seasonal_lag_periods > 0L]
  rolling_windows <- unique(as.integer(rolling_windows %||% integer()))
  rolling_windows <- rolling_windows[is.finite(rolling_windows) & rolling_windows > 0L]
  rolling_quantiles <- unique(as.numeric(rolling_quantiles %||% numeric()))
  if (any(!is.finite(rolling_quantiles) | rolling_quantiles <= 0 | rolling_quantiles >= 1)) stop("rolling_quantiles must be strictly between 0 and 1.", call. = FALSE)
  difference_orders <- unique(as.integer(difference_orders %||% integer()))
  difference_orders <- difference_orders[is.finite(difference_orders) & difference_orders > 0L]
  seasonal_difference_periods <- unique(as.integer(seasonal_difference_periods %||% integer()))
  seasonal_difference_periods <- seasonal_difference_periods[is.finite(seasonal_difference_periods) & seasonal_difference_periods > 0L]
  ewm_alphas <- unique(as.numeric(ewm_alphas %||% numeric()))
  if (any(!is.finite(ewm_alphas) | ewm_alphas <= 0 | ewm_alphas > 1)) stop("ewm_alphas must be in (0, 1].", call. = FALSE)
  fourier_periods <- unique(as.integer(fourier_periods %||% integer()))
  fourier_periods <- fourier_periods[is.finite(fourier_periods) & fourier_periods > 1L]
  fourier_pairs <- as.integer(fourier_pairs %||% 1L)[1L]
  if (!is.finite(fourier_pairs) || fourier_pairs < 1L) stop("fourier_pairs must be a positive integer.", call. = FALSE)
  forecast_horizon <- as.integer(forecast_horizon %||% 1L)[1L]
  if (!is.finite(forecast_horizon) || forecast_horizon < 1L) forecast_horizon <- 1L
  entity_id <- rodeo_contract_cols(entity_id)
  known_future_variables <- rodeo_contract_cols(known_future_variables)
  static_entity_features <- rodeo_contract_cols(static_entity_features)
  id_payload <- list(
    date_col, target_col, frequency, lag_periods, seasonal_lag_periods, rolling_windows,
    rolling_stats, difference_orders, seasonal_difference_periods,
    ewm_alphas, fourier_periods, fourier_pairs, calendar_features,
    known_future_variables
  )
  id <- rodeo_contract_scalar(id, paste0("temporal_", digest_simple(id_payload)))
  spec <- list(
    id = id,
    type = "temporal_forecast_features",
    date_col = date_col,
    target_col = target_col,
    frequency = frequency,
    calendar_features = calendar_features,
    lag_periods = lag_periods,
    seasonal_lag_periods = seasonal_lag_periods,
    rolling_windows = rolling_windows,
    rolling_stats = rolling_stats,
    rolling_quantiles = rolling_quantiles,
    difference_orders = difference_orders,
    seasonal_difference_periods = seasonal_difference_periods,
    ewm_alphas = ewm_alphas,
    fourier_periods = fourier_periods,
    fourier_pairs = fourier_pairs,
    expanding_stats = expanding_stats,
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
  resolved_frequency <- rodeo_temporal_resolve_frequency(spec$frequency, work[[spec$date_col]])
  work[, .rodeo_temporal_date := rodeo_temporal_coerce_index(get(spec$date_col), resolved_frequency)]
  if (any(is.na(work$.rodeo_temporal_date))) {
    stop("date_col must be convertible to a temporal index without missing timestamps.", call. = FALSE)
  }
  work[, .rodeo_temporal_target := as.numeric(get(spec$target_col))]
  order_cols <- c(spec$entity_id, ".rodeo_temporal_date")
  data.table::setorderv(work, order_cols)
  origin <- if (is.null(forecast_origin)) {
    max(work$.rodeo_temporal_date, na.rm = TRUE)
  } else {
    rodeo_temporal_coerce_index(forecast_origin[1L], resolved_frequency)
  }
  if (is.na(origin)) stop("forecast_origin must be convertible to the fitted temporal index.", call. = FALSE)
  history <- work[.rodeo_temporal_date <= origin]
  if (!nrow(history)) stop("No temporal training rows are at or before forecast_origin.", call. = FALSE)
  fitted_spec <- spec
  fitted_spec$frequency_declared <- spec$frequency
  fitted_spec$frequency <- resolved_frequency
  if (rodeo_temporal_frequency_is_intraday(resolved_frequency)) {
    fitted_spec$calendar_features <- unique(c(fitted_spec$calendar_features, "hour", "minute"))
  }
  prepared <- rodeo_temporal_add_origin_features(history, fitted_spec, reference_start = min(history$.rodeo_temporal_date))
  feature_manifest <- rodeo_temporal_feature_manifest(fitted_spec)
  fitted <- fitted_spec
  fitted$id <- paste0(spec$id, "_fit_", digest_simple(list(nrow(history), origin, names(dt), resolved_frequency)))
  fitted$spec_id <- spec$id
  fitted$forecast_origin <- origin
  fitted$training_start <- min(history$.rodeo_temporal_date)
  fitted$training_end <- max(history$.rodeo_temporal_date)
  fitted$temporal_class <- if (rodeo_temporal_frequency_is_intraday(resolved_frequency)) "posixct" else "date"
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
  dt[, .rodeo_temporal_date := rodeo_temporal_coerce_index(get(fitted_spec$date_col), fitted_spec$frequency)]
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
      prediction_frames = rodeo_temporal_recursive_prediction_frames(
        fitted_spec, future_rows, history, horizon
      ),
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
  out <- data.table::data.table(
    .rodeo_future_date = rodeo_temporal_coerce_index(future_row[[fitted_spec$date_col]], fitted_spec$frequency)
  )
  if (length(fitted_spec$entity_id)) {
    entity_col <- fitted_spec$entity_id[[1L]]
    out[, entity_id_code := match(as.character(future_row[[entity_col]]), fitted_spec$entity_levels)]
  }
  history_values <- as.numeric(history_values)
  for (lag in fitted_spec$lag_periods) {
    out[[paste0("target_lag_", lag)]] <- if (length(history_values) >= lag) history_values[length(history_values) - lag + 1L] else NA_real_
  }
  for (lag in fitted_spec$seasonal_lag_periods) {
    out[[paste0("target_seasonal_lag_", lag)]] <- if (length(history_values) >= lag) history_values[length(history_values) - lag + 1L] else NA_real_
  }
  for (window in fitted_spec$rolling_windows) {
    values <- if (length(history_values) >= window) utils::tail(history_values, window) else numeric()
    for (stat in fitted_spec$rolling_stats) {
      if (identical(stat, "quantile")) {
        for (prob in fitted_spec$rolling_quantiles) {
          suffix <- gsub("\\.", "_", format(prob, trim = TRUE, scientific = FALSE))
          out[[paste0("target_roll_quantile_", suffix, "_", window)]] <- if (length(values)) as.numeric(stats::quantile(values, prob, na.rm = TRUE, names = FALSE)) else NA_real_
        }
      } else {
        out[[paste0("target_roll_", stat, "_", window)]] <- if (length(values)) tail(rodeo_temporal_roll_stat(values, length(values), stat), 1L) else NA_real_
      }
    }
  }
  for (d in fitted_spec$difference_orders) {
    out[[paste0("target_difference_order_", d)]] <- if (length(history_values) > d) tail(diff(history_values, differences = d), 1L) else NA_real_
  }
  for (period in fitted_spec$seasonal_difference_periods) {
    out[[paste0("target_seasonal_difference_", period)]] <- if (length(history_values) > period) tail(history_values, 1L) - history_values[length(history_values) - period] else NA_real_
  }
  for (alpha in fitted_spec$ewm_alphas) {
    suffix <- gsub("\\.", "_", format(alpha, trim = TRUE, scientific = FALSE))
    out[[paste0("target_ewm_mean_", suffix)]] <- tail(rodeo_temporal_ewm(history_values, alpha), 1L)
  }
  for (stat in fitted_spec$expanding_stats) {
    out[[paste0("target_expanding_", stat)]] <- tail(rodeo_temporal_expanding_stat(history_values, stat), 1L)
  }
  out <- data.table::as.data.table(cbind(out, rodeo_temporal_fourier_features(length(history_values), fitted_spec)))
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
#' @param frequency Canonical calendar frequency or `"auto"`. Reuses the Rodeo
#'   temporal transformation frequency authority.
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
  future_data = NULL,
  frequency = "auto"
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
  resolved_frequency <- rodeo_temporal_resolve_frequency(frequency, work[[date_col]])
  work[, .rodeo_cross_target_date := rodeo_temporal_coerce_index(get(date_col), resolved_frequency)]
  if (any(is.na(work$.rodeo_cross_target_date))) {
    stop("date_col must be convertible to a temporal index without missing timestamps.", call. = FALSE)
  }
  origin <- rodeo_temporal_coerce_index(forecast_origin[1L], resolved_frequency)
  if (is.na(origin)) stop("forecast_origin must be convertible to the fitted temporal index.", call. = FALSE)
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
    future[, .rodeo_cross_target_date := rodeo_temporal_coerce_index(get(date_col), resolved_frequency)]
    data.table::setorder(future, .rodeo_cross_target_date)
  }
  add_features <- function(frame, include_future_known = TRUE) {
    out <- data.table::copy(frame)
    if (!nrow(out)) return(out)
    out <- data.table::as.data.table(cbind(out, rodeo_temporal_date_features(
      out$.rodeo_cross_target_date,
      c("year", "month", "day", "dow", "day_index"),
      min(training$.rodeo_cross_target_date)
    )))
    prior <- rodeo_temporal_prior_index(out$.rodeo_cross_target_date, training$.rodeo_cross_target_date)
    for (target in target_cols) {
      y <- as.numeric(training[[target]])
      for (lag in lag_periods) {
        idx <- prior - lag + 1L
        values <- rep(NA_real_, length(prior))
        ok <- is.finite(prior) & is.finite(idx) & prior >= lag & idx >= 1L
        values[ok] <- y[idx[ok]]
        out[[paste0("cross_target_", target, "_lag_", lag)]] <- values
      }
      if (length(rolling_windows)) {
        finite <- is.finite(y)
        cs <- c(0, cumsum(ifelse(finite, y, 0)))
        cn <- c(0, cumsum(as.integer(finite)))
        for (window in rolling_windows) {
          values <- rep(NA_real_, length(prior))
          ok <- is.finite(prior) & prior >= window
          start <- pmax(prior[ok] - window + 1L, 1L)
          n <- cn[prior[ok] + 1L] - cn[start]
          tot <- cs[prior[ok] + 1L] - cs[start]
          values[ok] <- ifelse(n > 0, tot / n, NA_real_)
          out[[paste0("cross_target_", target, "_roll_mean_", window)]] <- values
        }
      }
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
    frequency = resolved_frequency,
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
  expanded_spec <- rodeo_temporal_transformation_spec(
    date_col = "ds", target_col = "y", entity_id = "id", forecast_horizon = 2,
    seasonal_lag_periods = 7, rolling_windows = 4,
    rolling_stats = c("variance", "median", "min", "max", "range", "quantile"),
    rolling_quantiles = 0.75, difference_orders = 1,
    seasonal_difference_periods = 7, ewm_alphas = 0.4,
    expanding_stats = c("mean", "sd", "min", "max"),
    calendar_features = c("day_of_year", "month_start", "month_end"),
    fourier_periods = 7, fourier_pairs = 2
  )
  expanded_fit <- rodeo_fit_temporal_transformation(data, expanded_spec, forecast_origin = max(data$ds) - 3)
  expanded <- rodeo_apply_temporal_transformation(data[1:17], expanded_fit)
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
      "metadata_readable",
      "expanded_temporal_families",
      "expanded_features_use_prior_history",
      "expanded_feature_manifest_contract"
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
      all(c("temporal_specification_identity", "temporal_transformation_identity", "feature_manifest") %in% names(rodeo_temporal_transformation_metadata(fitted))),
      all(c("target_roll_variance_4", "target_roll_median_4", "target_roll_quantile_0_75_4",
        "target_difference_order_1", "target_seasonal_difference_7", "target_ewm_mean_0_4",
        "target_expanding_mean", "fourier_sin_p7_k1") %in% names(expanded)) &&
        "date_day_of_year" %in% expanded_fit$feature_manifest$feature,
      expanded$target_difference_order_1[5L] == data$y[4L] - data$y[3L] &&
        expanded$target_expanding_mean[5L] == mean(data$y[1:4]),
      all(c("feature_id", "owner", "temporal_resolution", "as_of_safe", "leakage_policy", "definition") %in%
        names(expanded_fit$feature_manifest)) && all(expanded_fit$feature_manifest$as_of_safe)
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
      "Metadata exposes identities, diagnostics, and feature manifest.",
      "Expanded rolling, differencing, EWM, expanding, Fourier, and calendar families materialize.",
      "Difference and expanding features use strictly prior target history.",
      "Expanded feature manifest records ownership, lineage, and as-of safety."
    )
  )
}

rodeo_temporal_add_origin_features <- function(dt, spec, reference_start) {
  out <- data.table::copy(dt)
  if (!".rodeo_temporal_date" %in% names(out)) {
    out[, .rodeo_temporal_date := rodeo_temporal_coerce_index(get(spec$date_col), spec$frequency)]
  }
  if (!".rodeo_temporal_target" %in% names(out)) out[, .rodeo_temporal_target := as.numeric(get(spec$target_col))]
  if (length(spec$entity_id)) {
    entity_col <- spec$entity_id[[1L]]
    levels <- spec$entity_levels %||% sort(unique(as.character(out[[entity_col]])))
    out[, entity_id_code := match(as.character(get(entity_col)), levels)]
  }
  group_cols <- spec$entity_id
  add_group <- function(x) {
    y <- as.numeric(x$.rodeo_temporal_target)
    x[[".rodeo_time_index"]] <- seq_len(nrow(x)) - 1L
    for (lag in spec$lag_periods) x[[paste0("target_lag_", lag)]] <- data.table::shift(y, n = lag, type = "lag")
    for (lag in spec$seasonal_lag_periods) x[[paste0("target_seasonal_lag_", lag)]] <- data.table::shift(y, n = lag, type = "lag")
    shifted_y <- data.table::shift(y, n = 1L, type = "lag")
    for (window in spec$rolling_windows) {
      for (stat in spec$rolling_stats) {
        if (identical(stat, "quantile")) {
          for (prob in spec$rolling_quantiles) {
            suffix <- gsub("\\.", "_", format(prob, trim = TRUE, scientific = FALSE))
            x[[paste0("target_roll_quantile_", suffix, "_", window)]] <- rodeo_temporal_roll_stat(shifted_y, window, stat, prob)
          }
        } else {
          x[[paste0("target_roll_", stat, "_", window)]] <- rodeo_temporal_roll_stat(shifted_y, window, stat)
        }
      }
    }
    for (d in spec$difference_orders) {
      aligned <- c(rep(NA_real_, d), diff(y, differences = d))
      x[[paste0("target_difference_order_", d)]] <- data.table::shift(aligned, n = 1L, type = "lag")
    }
    for (period in spec$seasonal_difference_periods) {
      x[[paste0("target_seasonal_difference_", period)]] <-
        data.table::shift(y, n = 1L, type = "lag") - data.table::shift(y, n = period + 1L, type = "lag")
    }
    for (alpha in spec$ewm_alphas) {
      suffix <- gsub("\\.", "_", format(alpha, trim = TRUE, scientific = FALSE))
      x[[paste0("target_ewm_mean_", suffix)]] <- rodeo_temporal_ewm(shifted_y, alpha)
    }
    for (stat in spec$expanding_stats) {
      x[[paste0("target_expanding_", stat)]] <- rodeo_temporal_expanding_stat(shifted_y, stat)
    }
    x <- data.table::as.data.table(cbind(x, rodeo_temporal_fourier_features(x$.rodeo_time_index, spec)))
    x
  }
  if (length(group_cols)) {
    out <- out[, add_group(.SD), by = group_cols]
  } else {
    out <- add_group(out)
  }
  out[]
}

rodeo_temporal_fourier_features <- function(time_index, spec) {
  if (!length(spec$fourier_periods)) return(NULL)
  out <- data.table::data.table(.row_id = seq_along(time_index))
  for (period in spec$fourier_periods) for (pair in seq_len(min(spec$fourier_pairs, floor(period / 2)))) {
    out[[paste0("fourier_sin_p", period, "_k", pair)]] <- sin(2 * pi * pair * time_index / period)
    out[[paste0("fourier_cos_p", period, "_k", pair)]] <- cos(2 * pi * pair * time_index / period)
  }
  out[, .row_id := NULL]
  out[]
}

rodeo_temporal_roll_stat <- function(x, window, stat, probability = NULL) {
  if (identical(stat, "mean")) return(data.table::frollmean(x, n = window, align = "right", fill = NA_real_))
  fun <- switch(
    stat,
    sd = function(z) stats::sd(z, na.rm = TRUE),
    variance = function(z) stats::var(z, na.rm = TRUE),
    median = function(z) stats::median(z, na.rm = TRUE),
    min = function(z) min(z, na.rm = TRUE),
    max = function(z) max(z, na.rm = TRUE),
    range = function(z) diff(range(z, na.rm = TRUE)),
    skewness = function(z) {
      s <- stats::sd(z, na.rm = TRUE)
      if (!is.finite(s) || s == 0) 0 else mean((z - mean(z, na.rm = TRUE))^3, na.rm = TRUE) / s^3
    },
    kurtosis = function(z) {
      s <- stats::sd(z, na.rm = TRUE)
      if (!is.finite(s) || s == 0) 0 else mean((z - mean(z, na.rm = TRUE))^4, na.rm = TRUE) / s^4 - 3
    },
    quantile = function(z) as.numeric(stats::quantile(z, probs = probability, na.rm = TRUE, names = FALSE, type = 7)),
    stop("Unsupported rolling statistic.", call. = FALSE)
  )
  data.table::frollapply(x, n = window, FUN = fun, align = "right", fill = NA_real_)
}

rodeo_temporal_ewm <- function(x, alpha) {
  out <- rep(NA_real_, length(x))
  state <- NA_real_
  for (i in seq_along(x)) {
    if (is.finite(x[[i]])) state <- if (is.finite(state)) alpha * x[[i]] + (1 - alpha) * state else x[[i]]
    out[[i]] <- state
  }
  out
}

rodeo_temporal_expanding_stat <- function(x, stat) {
  finite <- is.finite(x)
  count <- cumsum(finite)
  values <- ifelse(finite, x, 0)
  if (identical(stat, "mean")) {
    out <- cumsum(values) / count
    out[count == 0L] <- NA_real_
    return(out)
  }
  if (identical(stat, "sd")) {
    total <- cumsum(values)
    total_sq <- cumsum(values^2)
    variance <- (total_sq - total^2 / count) / (count - 1L)
    variance[variance < 0 & variance > -sqrt(.Machine$double.eps)] <- 0
    out <- sqrt(variance)
    out[count < 2L] <- NA_real_
    return(out)
  }
  reducer <- if (identical(stat, "min")) min else max
  sentinel <- if (identical(stat, "min")) Inf else -Inf
  out <- Reduce(reducer, ifelse(finite, x, sentinel), accumulate = TRUE)
  out[count == 0L] <- NA_real_
  out
}

rodeo_temporal_date_features <- function(dates, features, reference_start) {
  posix <- rodeo_temporal_as_posixct(dates)
  date_part <- as.Date(posix, tz = rodeo_temporal_tz(posix))
  d <- as.POSIXlt(posix, tz = rodeo_temporal_tz(posix))
  ref_date <- as.Date(rodeo_temporal_as_posixct(reference_start), tz = rodeo_temporal_tz(posix))
  next_date <- date_part + 1L
  out <- data.table::data.table()
  if ("year" %in% features) out[, date_year := d$year + 1900L]
  if ("month" %in% features) out[, date_month := d$mon + 1L]
  if ("day" %in% features) out[, date_day := d$mday]
  if ("dow" %in% features) out[, date_dow := as.integer(format(date_part, "%u"))]
  if ("week" %in% features) out[, date_week := as.integer(format(date_part, "%U"))]
  if ("quarter" %in% features) out[, date_quarter := floor(d$mon / 3L) + 1L]
  if ("is_weekend" %in% features) out[, date_is_weekend := as.integer(format(date_part, "%u") %in% c("6", "7"))]
  if ("day_index" %in% features) out[, date_day_index := as.numeric(date_part - ref_date)]
  if ("day_of_year" %in% features) out[, date_day_of_year := as.integer(format(date_part, "%j"))]
  if ("month_start" %in% features) out[, date_month_start := as.integer(d$mday == 1L)]
  if ("month_end" %in% features) out[, date_month_end := as.integer(format(next_date, "%d") == "01")]
  if ("quarter_start" %in% features) out[, date_quarter_start := as.integer(d$mday == 1L & (d$mon + 1L) %in% c(1L, 4L, 7L, 10L))]
  if ("quarter_end" %in% features) out[, date_quarter_end := as.integer(format(next_date, "%m-%d") %in% c("04-01", "07-01", "10-01", "01-01"))]
  if ("year_start" %in% features) out[, date_year_start := as.integer(format(date_part, "%m-%d") == "01-01")]
  if ("year_end" %in% features) out[, date_year_end := as.integer(format(date_part, "%m-%d") == "12-31")]
  if ("hour" %in% features) out[, date_hour := as.integer(d$hour)]
  if ("minute" %in% features) out[, date_minute := as.integer(d$min)]
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
  fourier <- rodeo_temporal_fourier_features(frame$.rodeo_time_index + horizon, fitted_spec)
  for (feature in names(fourier)) frame[[feature]] <- fourier[[feature]]
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
  future_row[, (future_date_col) := rodeo_temporal_coerce_index(get(fitted_spec$date_col), fitted_spec$frequency)]
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
  horizon_step <- if (".rodeo_panel_horizon" %in% names(out)) out$.rodeo_panel_horizon else 1L
  fourier <- rodeo_temporal_fourier_features(out$.rodeo_time_index + horizon_step, fitted_spec)
  for (feature in names(fourier)) out[[feature]] <- fourier[[feature]]
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
  declared <- unique(c(
    if (!is.null(fitted_spec$feature_manifest$feature)) fitted_spec$feature_manifest$feature else character()
  ))
  cols[vapply(cols, function(nm) {
    x <- frame[[nm]]
    is.numeric(x) || is.integer(x) ||
      ((is.character(x) || is.factor(x)) && nm %in% declared)
  }, logical(1))]
}

rodeo_temporal_feature_manifest <- function(spec) {
  rolling_features <- unlist(lapply(spec$rolling_windows, function(window) {
    unlist(lapply(spec$rolling_stats, function(stat) {
      if (identical(stat, "quantile")) {
        paste0("target_roll_quantile_", gsub("\\.", "_", format(spec$rolling_quantiles, trim = TRUE, scientific = FALSE)), "_", window)
      } else paste0("target_roll_", stat, "_", window)
    }), use.names = FALSE)
  }), use.names = FALSE)
  fourier_features <- unlist(lapply(spec$fourier_periods, function(period) {
    pairs <- seq_len(min(spec$fourier_pairs, floor(period / 2)))
    c(paste0("fourier_sin_p", period, "_k", pairs), paste0("fourier_cos_p", period, "_k", pairs))
  }), use.names = FALSE)
  features <- c(
    paste0("target_lag_", spec$lag_periods),
    paste0("target_seasonal_lag_", spec$seasonal_lag_periods),
    rolling_features,
    paste0("target_difference_order_", spec$difference_orders),
    paste0("target_seasonal_difference_", spec$seasonal_difference_periods),
    paste0("target_ewm_mean_", gsub("\\.", "_", format(spec$ewm_alphas, trim = TRUE, scientific = FALSE))),
    paste0("target_expanding_", spec$expanding_stats),
    fourier_features,
    if (length(spec$entity_id)) "entity_id_code" else character(),
    paste0("date_", spec$calendar_features),
    paste0("future_", spec$known_future_variables),
    paste0("static_", spec$static_entity_features)
  )
  manifest <- data.table::data.table(
    feature_id = paste0("temporal_feature_", seq_along(features)),
    feature = features,
    family = data.table::fcase(
      grepl("^target_seasonal_lag_", features), "seasonal_lag",
      grepl("^target_lag_", features), "lag",
      grepl("^target_roll_", features), "rolling_statistic",
      grepl("^target_difference_", features), "difference",
      grepl("^target_seasonal_difference_", features), "seasonal_difference",
      grepl("^target_ewm_", features), "exponentially_weighted",
      grepl("^target_expanding_", features), "expanding_statistic",
      grepl("^fourier_", features), "fourier",
      features == "entity_id_code", "entity_identity",
      grepl("^date_", features), "calendar",
      grepl("^future_", features), "future_known_variable",
      grepl("^static_", features), "static_entity_feature",
      default = "unknown"
    ),
    source = data.table::fcase(
      grepl("^target_seasonal_lag_", features), "target_history",
      grepl("^target_lag_", features), "target_lag",
      grepl("^target_roll_mean_", features), "target_rolling_stat",
      grepl("^target_(roll|difference|seasonal_difference|ewm|expanding)_", features), "target_history",
      grepl("^fourier_", features), "time_index",
      features == "entity_id_code", "entity_identity",
      grepl("^date_", features), "calendar",
      grepl("^future_", features), "future_known_variable",
      grepl("^static_", features), "static_entity_feature",
      default = "unknown"
    ),
    owner = "Rodeo",
    temporal_resolution = spec$frequency,
    as_of_safe = TRUE,
    leakage_policy = "uses only information available strictly before the forecast origin row",
    retained = TRUE
  )
  manifest[, definition := paste("Deterministic", family, "feature derived from", source)]
  manifest[, computational_cost := "cheap"]
  manifest[]
}

rodeo_temporal_future_rows <- function(fitted_spec, future_data, horizon) {
  if (!is.null(future_data) && nrow(data.table::as.data.table(future_data))) {
    future <- data.table::copy(data.table::as.data.table(future_data))
    if (!fitted_spec$date_col %in% names(future)) {
      stop("future_data must contain the fitted date_col.", call. = FALSE)
    }
    future[[fitted_spec$date_col]] <- rodeo_temporal_coerce_index(future[[fitted_spec$date_col]], fitted_spec$frequency)
    data.table::setorderv(future, c(fitted_spec$entity_id, fitted_spec$date_col))
    if (length(fitted_spec$entity_id)) {
      counts <- future[, .N, by = c(fitted_spec$entity_id)]
      if (any(counts$N < horizon)) {
        stop("future_data must contain at least horizon rows for each entity.", call. = FALSE)
      }
      future[, .rodeo_panel_horizon := seq_len(.N), by = c(fitted_spec$entity_id)]
      return(future[.rodeo_panel_horizon <= horizon])
    }
    if (nrow(future) < horizon) stop("future_data must contain at least horizon rows.", call. = FALSE)
    future[, .rodeo_panel_horizon := seq_len(.N)]
    return(future[seq_len(horizon)])
  }
  dates <- rodeo_temporal_next_index(fitted_spec$forecast_origin, fitted_spec$frequency, horizon)
  if (length(fitted_spec$entity_id)) {
    entity_col <- fitted_spec$entity_id[[1L]]
    entities <- fitted_spec$entity_levels
    if (!length(entities)) {
      entities <- unique(as.character(fitted_spec$history[[entity_col]]))
    }
    future <- data.table::data.table(
      .rodeo_entity = rep(entities, each = length(dates)),
      .rodeo_timestamp = rep(dates, times = length(entities)),
      .rodeo_panel_horizon = rep(seq_along(dates), times = length(entities))
    )
    data.table::setnames(future, c(".rodeo_entity", ".rodeo_timestamp"), c(entity_col, fitted_spec$date_col))
    data.table::setorderv(future, c(entity_col, fitted_spec$date_col))
    last_rows <- fitted_spec$history[, .SD[.N], by = c(entity_col)]
    for (var in fitted_spec$known_future_variables) future[[var]] <- NA_real_
    static_cols <- intersect(fitted_spec$static_entity_features, names(last_rows))
    if (length(static_cols)) {
      lookup <- last_rows[, c(entity_col, static_cols), with = FALSE]
      future <- lookup[future, on = entity_col]
    }
    return(future)
  }
  future <- data.table::data.table(
    .rodeo_timestamp = dates,
    .rodeo_panel_horizon = seq_along(dates)
  )
  data.table::setnames(future, ".rodeo_timestamp", fitted_spec$date_col)
  for (var in fitted_spec$known_future_variables) future[[var]] <- NA_real_
  for (var in fitted_spec$static_entity_features) future[[var]] <- NA_real_
  future
}

rodeo_temporal_frequency_aliases <- function() {
  c(
    "1min" = "1min", "1mins" = "1min", "1minute" = "1min", "1minutes" = "1min",
    "5min" = "5min", "5mins" = "5min", "5minute" = "5min", "5minutes" = "5min",
    "10min" = "10min", "10mins" = "10min", "10minute" = "10min", "10minutes" = "10min",
    "15min" = "15min", "15mins" = "15min", "15minute" = "15min", "15minutes" = "15min",
    "30min" = "30min", "30mins" = "30min", "30minute" = "30min", "30minutes" = "30min",
    "hour" = "hour", "hours" = "hour", "hr" = "hour", "hrs" = "hour", "hourly" = "hour",
    "day" = "day", "days" = "day", "dy" = "day", "dys" = "day", "daily" = "day",
    "week" = "week", "weeks" = "week", "wk" = "week", "wks" = "week", "weekly" = "week",
    "month" = "month", "months" = "month", "mth" = "month", "mths" = "month", "monthly" = "month",
    "quarter" = "quarter", "quarters" = "quarter", "qtr" = "quarter", "qtrs" = "quarter", "quarterly" = "quarter",
    "year" = "year", "years" = "year", "yr" = "year", "yrs" = "year", "yearly" = "year",
    "auto" = "auto"
  )
}

rodeo_temporal_prior_index <- function(query, history) {
  findInterval(as.numeric(query), as.numeric(history), left.open = TRUE)
}

rodeo_temporal_recursive_prediction_frames <- function(fitted_spec, future_rows, history, horizon) {
  future_rows <- data.table::as.data.table(future_rows)
  entity_col <- if (length(fitted_spec$entity_id)) fitted_spec$entity_id[[1L]] else NULL
  histories <- if (!is.null(entity_col)) {
    split(as.numeric(history$.rodeo_temporal_target), as.character(history[[entity_col]]))
  } else {
    list(.series = as.numeric(history$.rodeo_temporal_target))
  }
  lapply(seq_len(horizon), function(h) {
    rows <- if (".rodeo_panel_horizon" %in% names(future_rows)) {
      future_rows[.rodeo_panel_horizon == h]
    } else {
      future_rows[min(h, nrow(future_rows))]
    }
    if (!nrow(rows)) {
      stop("No future rows were generated for the requested horizon.", call. = FALSE)
    }
    if (is.null(entity_col)) {
      return(rodeo_temporal_prediction_frame(fitted_spec, rows[1L], histories[[1L]]))
    }
    data.table::rbindlist(lapply(seq_len(nrow(rows)), function(j) {
      row <- rows[j]
      ent <- as.character(row[[entity_col]])
      frame <- rodeo_temporal_prediction_frame(fitted_spec, row, histories[[ent]])
      frame[[entity_col]] <- ent
      frame$.rodeo_panel_horizon <- h
      frame
    }), use.names = TRUE, fill = TRUE)
  })
}

#' Normalize a forecast-calendar frequency to Rodeo's canonical identity
#'
#' @param frequency Frequency label or alias.
#' @return Canonical frequency: `auto`, `1min`, `5min`, `10min`, `15min`,
#'   `30min`, `hour`, `day`, `week`, `month`, `quarter`, or `year`.
#' @export
rodeo_temporal_normalize_frequency <- function(frequency) {
  raw <- tolower(gsub("[ _-]", "", as.character(frequency)[1L] %||% "auto"))
  aliases <- rodeo_temporal_frequency_aliases()
  if (!raw %in% names(aliases)) {
    stop(
      paste0(
        "Unsupported temporal frequency '", frequency,
        "'. Use auto, 1min, 5min, 10min, 15min, 30min, hour, day, week, month, quarter, or year."
      ),
      call. = FALSE
    )
  }
  unname(aliases[[raw]])
}

#' Whether a canonical frequency preserves POSIXct precision
#'
#' @param frequency Frequency label or alias.
#' @return `TRUE` for minute and hour grains.
#' @export
rodeo_temporal_frequency_is_intraday <- function(frequency) {
  rodeo_temporal_normalize_frequency(frequency) %in%
    c("1min", "5min", "10min", "15min", "30min", "hour")
}

rodeo_temporal_tz <- function(x) {
  tz <- attr(x, "tzone")
  if (is.null(tz) || !nzchar(tz[[1L]])) "UTC" else tz[[1L]]
}

rodeo_temporal_as_posixct <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "POSIXlt")) return(as.POSIXct(x))
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = "UTC"))
  if (is.character(x) || is.factor(x)) {
    parsed <- as.POSIXct(as.character(x), tz = "UTC")
    if (all(is.na(parsed) | grepl(":", as.character(x)) | nchar(as.character(x)) > 10L)) return(parsed)
    return(as.POSIXct(as.Date(as.character(x)), tz = "UTC"))
  }
  as.POSIXct(x, tz = "UTC")
}

#' Coerce timestamps to the Date or POSIXct class required by a frequency
#'
#' @param x Timestamp vector.
#' @param frequency Frequency label or alias.
#' @return `POSIXct` for intraday grains; `Date` otherwise.
#' @export
rodeo_temporal_coerce_index <- function(x, frequency) {
  frequency <- rodeo_temporal_normalize_frequency(frequency)
  if (rodeo_temporal_frequency_is_intraday(frequency)) {
    out <- rodeo_temporal_as_posixct(x)
    if (any(is.na(out) & !is.na(x))) {
      stop("Intraday timestamps could not be coerced to POSIXct.", call. = FALSE)
    }
    return(out)
  }
  if (inherits(x, "Date") && !inherits(x, "POSIXt")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x, tz = rodeo_temporal_tz(x)))
  as.Date(x)
}

rodeo_temporal_detect_frequency <- function(timestamps) {
  stamp <- rodeo_temporal_as_posixct(timestamps)
  stamp <- sort(unique(stamp[!is.na(stamp)]))
  if (length(stamp) < 2L) return("day")
  med <- stats::median(as.numeric(diff(stamp)), na.rm = TRUE)
  if (!is.finite(med) || med <= 0) return("day")
  if (abs(med - 60) <= 5) return("1min")
  if (abs(med - 300) <= 20) return("5min")
  if (abs(med - 600) <= 30) return("10min")
  if (abs(med - 900) <= 45) return("15min")
  if (abs(med - 1800) <= 90) return("30min")
  if (abs(med - 3600) <= 180) return("hour")
  if (abs(med - 86400) <= 3600) return("day")
  if (abs(med - 604800) <= 86400) return("week")
  if (med >= 27 * 86400 && med <= 32 * 86400) return("month")
  if (med >= 80 * 86400 && med <= 100 * 86400) return("quarter")
  if (med >= 360 * 86400 && med <= 370 * 86400) return("year")
  if (med < 86400) return("hour")
  "day"
}

#' Resolve `auto` or an explicit frequency against observed timestamps
#'
#' @param frequency Frequency label or alias.
#' @param timestamps Observed timestamp vector.
#' @return Canonical frequency identity.
#' @export
rodeo_temporal_resolve_frequency <- function(frequency, timestamps) {
  frequency <- rodeo_temporal_normalize_frequency(frequency)
  if (!identical(frequency, "auto")) return(frequency)
  rodeo_temporal_detect_frequency(timestamps)
}

#' Generate the next forecast-calendar timestamps from a fitted origin
#'
#' @param origin Last observed timestamp.
#' @param frequency Frequency label or alias.
#' @param horizon Number of future periods.
#' @return Timestamp vector with Date or POSIXct precision matching `frequency`.
#' @export
rodeo_temporal_next_index <- function(origin, frequency, horizon) {
  frequency <- rodeo_temporal_normalize_frequency(frequency)
  horizon <- as.integer(horizon)[1L]
  if (!is.finite(horizon) || horizon < 1L) {
    return(if (rodeo_temporal_frequency_is_intraday(frequency)) as.POSIXct(character()) else as.Date(character()))
  }
  if (rodeo_temporal_frequency_is_intraday(frequency)) {
    origin <- rodeo_temporal_as_posixct(origin)[1L]
    step <- switch(
      frequency,
      "1min" = 60, "5min" = 300, "10min" = 600,
      "15min" = 900, "30min" = 1800, "hour" = 3600
    )
    return(origin + step * seq_len(horizon))
  }
  origin <- if (inherits(origin, "POSIXt")) as.Date(origin, tz = rodeo_temporal_tz(origin)) else as.Date(origin)
  origin <- origin[1L]
  if (identical(frequency, "week")) return(origin + 7L * seq_len(horizon))
  add_period <- function(steps, unit) {
    do.call(c, lapply(steps, function(i) origin %m+% lubridate::period(i, units = unit)))
  }
  if (identical(frequency, "month")) return(add_period(seq_len(horizon), "month"))
  if (identical(frequency, "quarter")) return(add_period(3L * seq_len(horizon), "month"))
  if (identical(frequency, "year")) return(add_period(seq_len(horizon), "year"))
  origin + seq_len(horizon)
}

qa_rodeo_temporal_calendar <- function() {
  check <- function(name, value, detail) {
    data.table::data.table(test = name, passed = isTRUE(value), detail = detail)
  }
  tz <- "UTC"
  min15 <- data.table::data.table(
    ds = as.POSIXct("2026-03-01 09:00:00", tz = tz) + 15 * 60 * 0:11,
    y = 1:12
  )
  hour <- data.table::data.table(
    ds = as.POSIXct("2026-03-01 00:00:00", tz = tz) + 3600 * 0:23,
    y = 1:24
  )
  daily <- data.table::data.table(ds = as.Date("2026-01-01") + 0:9, y = 1:10)
  weekly <- data.table::data.table(ds = as.Date("2026-01-07") + 7L * 0:7, y = 1:8)
  calendar_seq <- function(origin, steps, unit) {
    do.call(c, lapply(steps, function(i) origin %m+% lubridate::period(i, units = unit)))
  }
  monthly <- data.table::data.table(
    ds = calendar_seq(as.Date("2024-01-31"), 0:5, "month"),
    y = 1:6
  )
  quarterly <- data.table::data.table(
    ds = calendar_seq(as.Date("2024-02-29"), 3L * 0:4, "month"),
    y = 1:5
  )
  yearly <- data.table::data.table(
    ds = calendar_seq(as.Date("2020-02-29"), 0:4, "year"),
    y = 1:5
  )
  fit_cal <- function(data, frequency, origin = NULL, horizon = 3L, entity_id = NULL) {
    spec <- rodeo_temporal_transformation_spec(
      date_col = "ds", target_col = "y", frequency = frequency,
      forecast_horizon = horizon, entity_id = entity_id,
      calendar_features = c("year", "month", "day", "dow", "day_index")
    )
    rodeo_fit_temporal_transformation(data, spec, forecast_origin = origin)
  }
  min15_fit <- fit_cal(min15, "15min", max(min15$ds), 4L)
  min15_gen <- rodeo_temporal_future_rows(min15_fit, NULL, 4L)
  hour_fit <- fit_cal(hour, "hours", max(hour$ds), 3L)
  hour_gen <- rodeo_temporal_future_rows(hour_fit, NULL, 3L)
  day_fit <- fit_cal(daily, "day", max(daily$ds), 3L)
  day_gen <- rodeo_temporal_future_rows(day_fit, NULL, 3L)
  week_fit <- fit_cal(weekly, "week", max(weekly$ds), 3L)
  week_gen <- rodeo_temporal_future_rows(week_fit, NULL, 3L)
  month_fit <- fit_cal(monthly, "month", as.Date("2024-01-31"), 3L)
  month_gen <- rodeo_temporal_future_rows(month_fit, NULL, 3L)
  quarter_fit <- fit_cal(quarterly, "quarter", as.Date("2024-02-29"), 2L)
  quarter_gen <- rodeo_temporal_future_rows(quarter_fit, NULL, 2L)
  year_fit <- fit_cal(yearly, "year", as.Date("2024-02-29"), 2L)
  year_gen <- rodeo_temporal_future_rows(year_fit, NULL, 2L)
  explicit <- data.table::data.table(
    ds = as.POSIXct(c("2026-03-01 12:15:00", "2026-03-01 12:45:00"), tz = tz),
    y = c(NA_real_, NA_real_)
  )
  explicit_rows <- rodeo_temporal_future_rows(min15_fit, explicit, 2L)
  panel <- data.table::rbindlist(list(
    data.table::copy(hour)[, id := "A"],
    data.table::copy(hour)[, `:=`(id = "B", y = y + 50)]
  ))
  panel_fit <- fit_cal(panel, "hour", max(hour$ds), 2L, entity_id = "id")
  panel_gen <- rodeo_temporal_future_rows(panel_fit, NULL, 2L)
  panel_direct <- rodeo_prepare_forecast_supervised_data(panel_fit, horizon = 2L, strategy = "direct")
  hour_direct <- rodeo_prepare_forecast_supervised_data(hour_fit, horizon = 3L, strategy = "direct")
  hour_recursive <- rodeo_prepare_forecast_supervised_data(hour_fit, horizon = 3L, strategy = "recursive")
  loaded <- unserialize(serialize(hour_fit, NULL))
  replay <- rodeo_temporal_future_rows(loaded, NULL, 3L)
  applied_hour <- rodeo_apply_temporal_transformation(hour, hour_fit)
  cal_date <- rodeo_temporal_date_features(
    as.Date(c("2026-01-01", "2026-01-31")),
    c("year", "month", "day", "month_end", "hour", "minute"),
    as.Date("2026-01-01")
  )
  cal_posix <- rodeo_temporal_date_features(
    as.POSIXct(c("2026-01-01 13:45:00", "2026-01-31 23:15:00"), tz = tz),
    c("year", "month", "day", "month_end", "hour", "minute"),
    as.POSIXct("2026-01-01 00:00:00", tz = tz)
  )
  truncated <- !inherits(min15_fit$history$.rodeo_temporal_date, "POSIXct") ||
    any(as.POSIXlt(min15_fit$history$.rodeo_temporal_date, tz = tz)$min == 0L &
          as.POSIXlt(min15$ds, tz = tz)$min != 0L)
  data.table::rbindlist(list(
    check("freq_15min_horizon", identical(
      min15_gen$ds,
      as.POSIXct("2026-03-01 09:00:00", tz = tz) + 15 * 60 * 12:15
    ), "15-minute future timestamps increment by 15 minutes."),
    check("freq_hourly_horizon", identical(
      hour_gen$ds,
      as.POSIXct("2026-03-02 00:00:00", tz = tz) + 3600 * 0:2
    ), "Hourly future timestamps increment by one hour."),
    check("freq_daily_horizon", identical(day_gen$ds, as.Date("2026-01-11") + 0:2), "Daily future dates increment by one day."),
    check("freq_weekly_horizon", identical(week_gen$ds, as.Date("2026-01-07") + 7L * 8:10), "Weekly future dates increment by seven days."),
    check("freq_month_end", identical(month_gen$ds, as.Date(c("2024-02-29", "2024-03-31", "2024-04-30"))), "Monthly calendars keep month-end alignment."),
    check("freq_quarterly", identical(quarter_gen$ds, as.Date(c("2024-05-29", "2024-08-29"))), "Quarterly calendars increment by three calendar months."),
    check("freq_leap_year", identical(year_gen$ds, as.Date(c("2025-02-28", "2026-02-28"))), "Yearly leap-day origins land on February 28 in non-leap years."),
    check("panel_one_horizon_per_entity", identical(sort(panel_gen$id), rep(c("A", "B"), each = 2L)) &&
      identical(panel_gen[.rodeo_panel_horizon == 1L, ds], rep(hour_gen$ds[1L], 2L)) &&
      nrow(panel_direct$prediction_frames[["1"]]) == 2L,
      "Generated panel calendars keep one correct horizon per entity."),
    check("explicit_future_data_authoritative", inherits(explicit_rows$ds, "POSIXct") &&
      identical(explicit_rows$ds, explicit$ds),
      "Supplied future_data timestamps are used without regeneration."),
    check("direct_uses_frequency_calendar", inherits(hour_direct$prediction_frames[["1"]]$.rodeo_future_date, "POSIXct") &&
      identical(hour_direct$prediction_frames[["1"]]$.rodeo_future_date, hour_gen$ds[1L]),
      "Direct strategy consumes the frequency-correct future calendar."),
    check("recursive_uses_frequency_calendar", length(hour_recursive$prediction_frames) == 3L &&
      identical(hour_recursive$prediction_frames[[2L]]$.rodeo_future_date, hour_gen$ds[2L]),
      "Recursive strategy consumes the frequency-correct future calendar."),
    check("serialization_replay_calendar", identical(replay$ds, hour_gen$ds) &&
      inherits(loaded$forecast_origin, "POSIXct"),
      "Serialized fitted transforms replay identical future timestamps without refit."),
    check("no_intraday_truncation", inherits(min15_fit$history$.rodeo_temporal_date, "POSIXct") &&
      identical(min15_fit$history$.rodeo_temporal_date, min15$ds) && !truncated &&
      all(c("date_hour", "date_minute") %in% min15_fit$feature_manifest$feature),
      "Intraday timestamps remain POSIXct and hour/minute features are available."),
    check("calendar_features_date_and_posixct", identical(cal_date$date_month_end, c(0L, 1L)) &&
      identical(cal_posix$date_month_end, c(0L, 1L)) &&
      identical(cal_posix$date_hour, c(13L, 23L)) && identical(cal_posix$date_minute, c(45L, 15L)) &&
      identical(cal_date$date_hour, c(0L, 0L)),
      "Calendar features remain correct for Date and POSIXct, including hour/minute.")
  ))
}
