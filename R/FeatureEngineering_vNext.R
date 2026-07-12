#' Create a Rodeo vNext feature engineering plan
#'
#' @description
#' Creates a scoring-safe feature engineering plan without changing legacy Rodeo
#' APIs. Model-based features are intentionally out of scope for this vNext layer.
#'
#' @export
rodeo_feature_plan <- function(
  numeric = list(columns = character(), transforms = c("log1p", "sqrt", "standardize", "winsorize"),
                 winsorize_probs = c(0.01, 0.99)),
  categorical = list(columns = character(), top_n = 10L, rare_level = "__RARE__",
                     unseen_level = "__UNSEEN__", one_hot = TRUE, keep_original = TRUE),
  calendar = list(columns = character(), features = c("year", "month", "day", "wday", "week", "quarter", "is_weekend")),
  text = list(columns = character(), features = c("char_count", "word_count", "digit_count", "punct_count", "upper_ratio", "blank")),
  missingness = list(columns = character(), suffix = "_is_missing"),
  interactions = list(numeric_pairs = list(), categorical_numeric = list(), categorical_pairs = list(), max_features = 50L),
  cross_row = list(enabled = FALSE),
  metadata = list()
) {
  plan <- list(
    numeric = numeric,
    categorical = categorical,
    calendar = calendar,
    text = text,
    missingness = missingness,
    interactions = interactions,
    cross_row = cross_row,
    metadata = metadata,
    created_at = Sys.time()
  )
  class(plan) <- c("rodeo_feature_plan", "list")
  plan
}

rodeo_vnext_cols <- function(x) {
  if (is.null(x)) character() else as.character(x)
}

rodeo_vnext_keep_cols <- function(cols, data) {
  intersect(rodeo_vnext_cols(cols), names(data))
}

rodeo_vnext_suffix <- function(x, fallback) {
  if (is.null(x) || !nzchar(as.character(x)[1L])) fallback else as.character(x)[1L]
}

rodeo_vnext_feature_manifest <- function() {
  data.table::data.table(
    feature = character(),
    source_column = character(),
    family = character(),
    transform = character(),
    scoring_safe = logical()
  )
}

rodeo_vnext_manifest_row <- function(feature, source_column, family, transform, scoring_safe = TRUE) {
  list(
    feature = feature,
    source_column = source_column,
    family = family,
    transform = transform,
    scoring_safe = scoring_safe
  )
}

rodeo_vnext_manifest_dt <- function(rows) {
  if (!length(rows)) {
    return(rodeo_vnext_feature_manifest())
  }
  data.table::rbindlist(rows, use.names = TRUE)
}

rodeo_vnext_batch_assign <- function(dt, column_names, values) {
  if (!length(column_names)) return(invisible(dt))
  data.table::setalloccol(dt, ncol(dt) + length(column_names))
  dt[, (column_names) := values]
  invisible(dt)
}

rodeo_vnext_add_manifest <- function(manifest, feature, source_column, family, transform, scoring_safe = TRUE) {
  data.table::rbindlist(list(
    manifest,
    data.table::data.table(
      feature = feature,
      source_column = source_column,
      family = family,
      transform = transform,
      scoring_safe = scoring_safe
    )
  ), use.names = TRUE)
}

#' Fit a Rodeo vNext feature engineering plan
#'
#' @export
rodeo_fit_feature_plan <- function(data, plan = rodeo_feature_plan()) {
  dt <- data.table::as.data.table(data)
  warnings <- character()
  col_names <- names(dt)
  manifest_rows <- list()
  manifest_n <- 0L

  num_cols <- rodeo_vnext_keep_cols(plan$numeric$columns, dt)
  cat_cols <- rodeo_vnext_keep_cols(plan$categorical$columns, dt)
  cal_cols <- rodeo_vnext_keep_cols(plan$calendar$columns, dt)
  txt_cols <- rodeo_vnext_keep_cols(plan$text$columns, dt)
  miss_cols <- rodeo_vnext_keep_cols(plan$missingness$columns, dt)

  missing_requested <- setdiff(unique(c(
    rodeo_vnext_cols(plan$numeric$columns), rodeo_vnext_cols(plan$categorical$columns),
    rodeo_vnext_cols(plan$calendar$columns), rodeo_vnext_cols(plan$text$columns),
    rodeo_vnext_cols(plan$missingness$columns)
  )), col_names)
  if (length(missing_requested)) {
    warnings <- c(warnings, paste("Requested columns not found:", paste(missing_requested, collapse = ", ")))
  }

  numeric_specs <- vector("list", length(num_cols))
  names(numeric_specs) <- num_cols
  transforms <- rodeo_vnext_cols(plan$numeric$transforms)
  for (col in num_cols) {
    x <- dt[[col]]
    probs <- plan$numeric$winsorize_probs
    lower <- suppressWarnings(stats::quantile(x, probs = probs[1L], na.rm = TRUE, names = FALSE))
    upper <- suppressWarnings(stats::quantile(x, probs = probs[2L], na.rm = TRUE, names = FALSE))
    numeric_specs[[col]] <- list(
      mean = mean(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      lower = lower,
      upper = upper,
      transforms = transforms
    )
    for (tr in transforms) {
      manifest_n <- manifest_n + 1L
      manifest_rows[[manifest_n]] <- rodeo_vnext_manifest_row(paste0(col, "_", tr), col, "numeric", tr)
    }
  }

  categorical_specs <- vector("list", length(cat_cols))
  names(categorical_specs) <- cat_cols
  top_n <- as.integer(plan$categorical$top_n %||% 10L)
  rare_level <- rodeo_vnext_suffix(plan$categorical$rare_level, "__RARE__")
  unseen_level <- rodeo_vnext_suffix(plan$categorical$unseen_level, "__UNSEEN__")
  for (col in cat_cols) {
    tab <- sort(table(as.character(dt[[col]]), useNA = "no"), decreasing = TRUE)
    levels <- names(tab)[seq_len(min(length(tab), top_n))]
    levels <- unique(c(levels, rare_level, unseen_level))
    encoded_names <- paste0(col, "__", make.names(levels))
    categorical_specs[[col]] <- list(
      levels = levels,
      encoded_names = encoded_names,
      rare_level = rare_level,
      unseen_level = unseen_level
    )
    for (idx in seq_along(levels)) {
      manifest_n <- manifest_n + 1L
      manifest_rows[[manifest_n]] <- rodeo_vnext_manifest_row(encoded_names[idx], col, "categorical", "one_hot")
    }
  }

  calendar_features <- rodeo_vnext_cols(plan$calendar$features)
  for (col in cal_cols) {
    for (feature in calendar_features) {
      manifest_n <- manifest_n + 1L
      manifest_rows[[manifest_n]] <- rodeo_vnext_manifest_row(paste0(col, "_", feature), col, "calendar", feature)
    }
  }

  text_features <- rodeo_vnext_cols(plan$text$features)
  for (col in txt_cols) {
    for (feature in text_features) {
      manifest_n <- manifest_n + 1L
      manifest_rows[[manifest_n]] <- rodeo_vnext_manifest_row(paste0(col, "_", feature), col, "text", feature)
    }
  }

  miss_suffix <- rodeo_vnext_suffix(plan$missingness$suffix, "_is_missing")
  for (col in miss_cols) {
    manifest_n <- manifest_n + 1L
    manifest_rows[[manifest_n]] <- rodeo_vnext_manifest_row(paste0(col, miss_suffix), col, "missingness", "is_missing")
  }

  interaction_specs <- rodeo_vnext_prepare_interactions(plan$interactions, dt, col_names)
  if (length(interaction_specs$manifest_rows)) manifest_rows <- c(manifest_rows, interaction_specs$manifest_rows)
  warnings <- c(warnings, interaction_specs$warnings)

  manifest <- rodeo_vnext_manifest_dt(manifest_rows)
  diagnostics <- data.table::rbindlist(list(
    data.table::data.table(check = "input_rows", status = "ok", detail = as.character(nrow(dt))),
    data.table::data.table(check = "generated_features", status = "ok", detail = as.character(nrow(manifest))),
    data.table::data.table(check = "cross_row", status = if (isTRUE(plan$cross_row$enabled)) "deferred" else "skipped",
                           detail = "Cross-row vNext wrappers are architecture-scoped for a later optimization pass.")
  ), use.names = TRUE)

  fitted <- list(
    plan = plan,
    numeric_specs = numeric_specs,
    categorical_specs = categorical_specs,
    calendar_columns = cal_cols,
    text_columns = txt_cols,
    missingness_columns = miss_cols,
    interaction_specs = interaction_specs$specs,
    feature_manifest = manifest,
    diagnostics = diagnostics,
    warnings = unique(warnings),
    fitted_at = Sys.time()
  )
  class(fitted) <- c("rodeo_fitted_feature_plan", "list")
  fitted
}

rodeo_vnext_prepare_interactions <- function(interactions, dt, col_names = names(dt)) {
  max_features <- as.integer(interactions$max_features %||% 50L)
  manifest_rows <- list()
  manifest_n <- 0L
  specs <- list(numeric_pairs = list(), categorical_numeric = list(), categorical_pairs = list())
  warnings <- character()

  add_if_room <- function(current_n) current_n < max_features
  n <- 0L
  for (pair in interactions$numeric_pairs %||% list()) {
    pair <- rodeo_vnext_cols(pair)
    if (length(pair) == 2L && all(pair %in% col_names) && add_if_room(n)) {
      feature <- paste0(pair[1L], "_x_", pair[2L])
      specs$numeric_pairs[[length(specs$numeric_pairs) + 1L]] <- pair
      manifest_n <- manifest_n + 1L
      manifest_rows[[manifest_n]] <- rodeo_vnext_manifest_row(feature, paste(pair, collapse = ","), "interaction", "numeric_x_numeric")
      n <- n + 1L
    }
  }
  for (item in interactions$categorical_numeric %||% list()) {
    cat_col <- item$categorical %||% item[[1L]]
    num_col <- item$numeric %||% item[[2L]]
    if (length(cat_col) && length(num_col) && cat_col %in% col_names && num_col %in% col_names) {
      levels <- unique(as.character(dt[[cat_col]]))
      for (lvl in levels) {
        if (!add_if_room(n)) break
        feature <- paste0(cat_col, "__", make.names(lvl), "_x_", num_col)
        specs$categorical_numeric[[length(specs$categorical_numeric) + 1L]] <- list(
          categorical = cat_col,
          numeric = num_col,
          level = lvl,
          feature_name = feature
        )
        manifest_n <- manifest_n + 1L
        manifest_rows[[manifest_n]] <- rodeo_vnext_manifest_row(feature, paste(cat_col, num_col, sep = ","), "interaction", "categorical_x_numeric")
        n <- n + 1L
      }
    }
  }
  for (pair in interactions$categorical_pairs %||% list()) {
    pair <- rodeo_vnext_cols(pair)
    if (length(pair) == 2L && all(pair %in% col_names) && add_if_room(n)) {
      feature <- paste0(pair[1L], "_x_", pair[2L])
      specs$categorical_pairs[[length(specs$categorical_pairs) + 1L]] <- list(columns = pair, feature_name = feature)
      manifest_n <- manifest_n + 1L
      manifest_rows[[manifest_n]] <- rodeo_vnext_manifest_row(feature, paste(pair, collapse = ","), "interaction", "categorical_x_categorical")
      n <- n + 1L
    }
  }
  if (n >= max_features) warnings <- c(warnings, paste("Interaction feature cap reached:", max_features))
  list(specs = specs, manifest_rows = manifest_rows, warnings = warnings)
}

#' Transform data with a fitted Rodeo vNext feature plan
#'
#' @export
rodeo_transform_feature_plan <- function(data, fitted_plan, copy_data = TRUE) {
  if (!inherits(fitted_plan, "rodeo_fitted_feature_plan")) {
    stop("fitted_plan must be returned by rodeo_fit_feature_plan().", call. = FALSE)
  }
  dt <- data.table::as.data.table(data)
  if (copy_data) dt <- data.table::copy(dt)
  dt_names <- names(dt)

  new_names <- character()
  new_values <- list()
  for (col in names(fitted_plan$numeric_specs)) {
    if (!col %in% dt_names) next
    spec <- fitted_plan$numeric_specs[[col]]
    x <- dt[[col]]
    if ("log1p" %in% spec$transforms) {
      ok <- !is.na(x) & x > -1
      if (all(ok)) {
        value <- log1p(x)
      } else {
        value <- rep(NA_real_, length(x))
        value[ok] <- log1p(x[ok])
      }
      new_names <- c(new_names, paste0(col, "_log1p"))
      new_values[[length(new_values) + 1L]] <- value
    }
    if ("sqrt" %in% spec$transforms) {
      ok <- !is.na(x) & x >= 0
      if (all(ok)) {
        value <- sqrt(x)
      } else {
        value <- rep(NA_real_, length(x))
        value[ok] <- sqrt(x[ok])
      }
      new_names <- c(new_names, paste0(col, "_sqrt"))
      new_values[[length(new_values) + 1L]] <- value
    }
    if ("standardize" %in% spec$transforms) {
      sd_value <- if (is.na(spec$sd) || spec$sd == 0) 1 else spec$sd
      new_names <- c(new_names, paste0(col, "_standardize"))
      new_values[[length(new_values) + 1L]] <- (x - spec$mean) / sd_value
    }
    if ("winsorize" %in% spec$transforms) {
      new_names <- c(new_names, paste0(col, "_winsorize"))
      new_values[[length(new_values) + 1L]] <- pmin(pmax(x, spec$lower), spec$upper)
    }
  }
  rodeo_vnext_batch_assign(dt, new_names, new_values)

  new_names <- character()
  new_values <- list()
  for (col in names(fitted_plan$categorical_specs)) {
    if (!col %in% dt_names) next
    spec <- fitted_plan$categorical_specs[[col]]
    vals <- as.character(dt[[col]])
    vals[is.na(vals)] <- spec$rare_level
    vals[!vals %chin% spec$levels] <- spec$unseen_level
    for (idx in seq_along(spec$levels)) {
      new_names <- c(new_names, spec$encoded_names[idx])
      new_values[[length(new_values) + 1L]] <- as.integer(vals == spec$levels[idx])
    }
  }
  rodeo_vnext_batch_assign(dt, new_names, new_values)

  calendar_features <- fitted_plan$plan$calendar$features
  calendar_has_year <- "year" %in% calendar_features
  calendar_has_month <- "month" %in% calendar_features
  calendar_has_day <- "day" %in% calendar_features
  calendar_has_wday <- "wday" %in% calendar_features
  calendar_has_week <- "week" %in% calendar_features
  calendar_has_quarter <- "quarter" %in% calendar_features
  calendar_has_weekend <- "is_weekend" %in% calendar_features
  new_names <- character()
  new_values <- list()
  for (col in fitted_plan$calendar_columns) {
    if (!col %in% dt_names) next
    date_value <- as.Date(dt[[col]])
    d <- as.POSIXlt(date_value)
    if (calendar_has_year) {
      new_names <- c(new_names, paste0(col, "_year"))
      new_values[[length(new_values) + 1L]] <- d$year + 1900L
    }
    if (calendar_has_month) {
      new_names <- c(new_names, paste0(col, "_month"))
      new_values[[length(new_values) + 1L]] <- d$mon + 1L
    }
    if (calendar_has_day) {
      new_names <- c(new_names, paste0(col, "_day"))
      new_values[[length(new_values) + 1L]] <- d$mday
    }
    if (calendar_has_wday) {
      new_names <- c(new_names, paste0(col, "_wday"))
      new_values[[length(new_values) + 1L]] <- d$wday + 1L
    }
    if (calendar_has_week) {
      new_names <- c(new_names, paste0(col, "_week"))
      new_values[[length(new_values) + 1L]] <- as.integer(format(date_value, "%U"))
    }
    if (calendar_has_quarter) {
      new_names <- c(new_names, paste0(col, "_quarter"))
      new_values[[length(new_values) + 1L]] <- ((d$mon) %/% 3L) + 1L
    }
    if (calendar_has_weekend) {
      new_names <- c(new_names, paste0(col, "_is_weekend"))
      new_values[[length(new_values) + 1L]] <- as.integer((d$wday + 1L) %in% c(1L, 7L))
    }
  }
  rodeo_vnext_batch_assign(dt, new_names, new_values)

  text_features <- fitted_plan$plan$text$features
  text_has_char_count <- "char_count" %in% text_features
  text_has_word_count <- "word_count" %in% text_features
  text_has_digit_count <- "digit_count" %in% text_features
  text_has_punct_count <- "punct_count" %in% text_features
  text_has_upper_ratio <- "upper_ratio" %in% text_features
  text_has_blank <- "blank" %in% text_features
  new_names <- character()
  new_values <- list()
  for (col in fitted_plan$text_columns) {
    if (!col %in% dt_names) next
    x <- as.character(dt[[col]])
    x[is.na(x)] <- ""
    x_nchar <- nchar(x)
    if (text_has_char_count) {
      new_names <- c(new_names, paste0(col, "_char_count"))
      new_values[[length(new_values) + 1L]] <- x_nchar
    }
    if (text_has_word_count) {
      new_names <- c(new_names, paste0(col, "_word_count"))
      new_values[[length(new_values) + 1L]] <- lengths(regmatches(x, gregexpr("\\S+", x)))
    }
    if (text_has_digit_count) {
      new_names <- c(new_names, paste0(col, "_digit_count"))
      new_values[[length(new_values) + 1L]] <- nchar(gsub("\\D", "", x))
    }
    if (text_has_punct_count) {
      new_names <- c(new_names, paste0(col, "_punct_count"))
      new_values[[length(new_values) + 1L]] <- nchar(gsub("[^[:punct:]]", "", x))
    }
    if (text_has_upper_ratio) {
      upper <- nchar(gsub("[^A-Z]", "", x))
      new_names <- c(new_names, paste0(col, "_upper_ratio"))
      new_values[[length(new_values) + 1L]] <- ifelse(x_nchar == 0, 0, upper / x_nchar)
    }
    if (text_has_blank) {
      new_names <- c(new_names, paste0(col, "_blank"))
      new_values[[length(new_values) + 1L]] <- as.integer(!nzchar(trimws(x)))
    }
  }
  rodeo_vnext_batch_assign(dt, new_names, new_values)

  miss_suffix <- rodeo_vnext_suffix(fitted_plan$plan$missingness$suffix, "_is_missing")
  new_names <- character()
  new_values <- list()
  for (col in fitted_plan$missingness_columns) {
    if (!col %in% dt_names) next
    new_names <- c(new_names, paste0(col, miss_suffix))
    new_values[[length(new_values) + 1L]] <- as.integer(is.na(dt[[col]]))
  }
  rodeo_vnext_batch_assign(dt, new_names, new_values)

  new_names <- character()
  new_values <- list()
  for (pair in fitted_plan$interaction_specs$numeric_pairs) {
    new_names <- c(new_names, paste0(pair[1L], "_x_", pair[2L]))
    new_values[[length(new_values) + 1L]] <- dt[[pair[1L]]] * dt[[pair[2L]]]
  }
  for (item in fitted_plan$interaction_specs$categorical_numeric) {
    new_names <- c(new_names, item$feature_name)
    new_values[[length(new_values) + 1L]] <- as.integer(as.character(dt[[item$categorical]]) == item$level) * dt[[item$numeric]]
  }
  for (pair in fitted_plan$interaction_specs$categorical_pairs) {
    cols <- pair$columns
    new_names <- c(new_names, pair$feature_name)
    new_values[[length(new_values) + 1L]] <- paste(dt[[cols[1L]]], dt[[cols[2L]]], sep = "__")
  }
  rodeo_vnext_batch_assign(dt, new_names, new_values)

  dt
}

#' Fit and transform a Rodeo vNext feature plan
#'
#' @export
rodeo_fit_transform_feature_plan <- function(data, plan = rodeo_feature_plan(), copy_data = TRUE) {
  fitted_plan <- rodeo_fit_feature_plan(data = data, plan = plan)
  engineered_data <- rodeo_transform_feature_plan(data = data, fitted_plan = fitted_plan, copy_data = copy_data)
  list(engineered_data = engineered_data, fitted_plan = fitted_plan)
}

#' Generate feature engineering artifacts from a Rodeo vNext run
#'
#' @export
generate_rodeo_feature_engineering_artifacts <- function(data, plan = NULL, fitted_plan = NULL, benchmark_summary = NULL) {
  if (is.null(fitted_plan)) {
    if (is.null(plan)) plan <- rodeo_feature_plan()
    ft <- rodeo_fit_transform_feature_plan(data = data, plan = plan)
    fitted_plan <- ft$fitted_plan
    engineered_data <- ft$engineered_data
  } else {
    engineered_data <- rodeo_transform_feature_plan(data = data, fitted_plan = fitted_plan)
  }
  summary <- data.table::data.table(
    metric = c("input_rows", "input_columns", "engineered_columns", "generated_features"),
    value = c(nrow(data), ncol(data), ncol(engineered_data), nrow(fitted_plan$feature_manifest))
  )
  artifacts <- list(
    overview_text = "Rodeo vNext feature engineering run completed.",
    config_table = data.table::data.table(
      family = c("numeric", "categorical", "calendar", "text", "missingness", "interactions", "cross_row"),
      enabled = c(
        length(fitted_plan$numeric_specs) > 0L,
        length(fitted_plan$categorical_specs) > 0L,
        length(fitted_plan$calendar_columns) > 0L,
        length(fitted_plan$text_columns) > 0L,
        length(fitted_plan$missingness_columns) > 0L,
        length(unlist(fitted_plan$interaction_specs, recursive = FALSE)) > 0L,
        isTRUE(fitted_plan$plan$cross_row$enabled)
      )
    ),
    feature_manifest = fitted_plan$feature_manifest,
    diagnostics = fitted_plan$diagnostics,
    engineered_data_summary = summary,
    benchmark_summary = benchmark_summary
  )
  list(
    artifacts = artifacts,
    metadata = list(generator = "generate_rodeo_feature_engineering_artifacts", generated_at = Sys.time()),
    warnings = fitted_plan$warnings,
    diagnostics = fitted_plan$diagnostics,
    value = list(
      engineered_data = engineered_data,
      fitted_plan = fitted_plan,
      feature_manifest = fitted_plan$feature_manifest,
      diagnostics = fitted_plan$diagnostics,
      warnings = fitted_plan$warnings
    )
  )
}

rodeo_vnext_fixture <- function() {
  data.table::data.table(
    id = 1:8,
    x = c(1, 2, 3, NA, 5, 100, 7, 8),
    y = c(2, 4, 6, 8, 10, 12, 14, 16),
    cat = c("A", "A", "B", "B", "C", "C", "D", NA),
    cat2 = c("K", "L", "K", "L", "K", "L", "K", "L"),
    date = as.Date("2024-01-01") + 0:7,
    text = c("Hello WORLD", "two words", "", NA, "ABC123!", "small", "More text.", "LAST")
  )
}

rodeo_vnext_test_plan <- function() {
  rodeo_feature_plan(
    numeric = list(columns = c("x", "y"), transforms = c("log1p", "sqrt", "standardize", "winsorize"), winsorize_probs = c(0.1, 0.9)),
    categorical = list(columns = "cat", top_n = 2L, rare_level = "__RARE__", unseen_level = "__UNSEEN__", one_hot = TRUE, keep_original = TRUE),
    calendar = list(columns = "date", features = c("year", "month", "day", "wday", "week", "quarter", "is_weekend")),
    text = list(columns = "text", features = c("char_count", "word_count", "digit_count", "punct_count", "upper_ratio", "blank")),
    missingness = list(columns = c("x", "cat", "text"), suffix = "_is_missing"),
    interactions = list(
      numeric_pairs = list(c("x", "y")),
      categorical_numeric = list(list(categorical = "cat", numeric = "y")),
      categorical_pairs = list(c("cat", "cat2")),
      max_features = 20L
    )
  )
}

rodeo_vnext_qa_result <- function(test, passed, detail = "") {
  data.table::data.table(test = test, passed = isTRUE(passed), detail = as.character(detail))
}

qa_rodeo_vnext_numeric <- function() {
  out <- rodeo_fit_transform_feature_plan(rodeo_vnext_fixture(), rodeo_vnext_test_plan())
  cols <- c("x_log1p", "x_sqrt", "x_standardize", "x_winsorize")
  rodeo_vnext_qa_result("numeric", all(cols %in% names(out$engineered_data)), paste(cols, collapse = ", "))
}

qa_rodeo_vnext_categorical <- function() {
  out <- rodeo_fit_transform_feature_plan(rodeo_vnext_fixture(), rodeo_vnext_test_plan())
  score <- data.table::copy(rodeo_vnext_fixture())
  score$cat[1L] <- "NEW"
  scored <- rodeo_transform_feature_plan(score, out$fitted_plan)
  unseen_col <- paste0("cat__", make.names("__UNSEEN__"))
  rodeo_vnext_qa_result("categorical", unseen_col %in% names(scored) && scored[[unseen_col]][1L] == 1L, "unseen category handled")
}

qa_rodeo_vnext_calendar <- function() {
  out <- rodeo_fit_transform_feature_plan(rodeo_vnext_fixture(), rodeo_vnext_test_plan())
  rodeo_vnext_qa_result("calendar", all(c("date_year", "date_month", "date_is_weekend") %in% names(out$engineered_data)), "calendar columns exist")
}

qa_rodeo_vnext_text <- function() {
  out <- rodeo_fit_transform_feature_plan(rodeo_vnext_fixture(), rodeo_vnext_test_plan())
  rodeo_vnext_qa_result("text", all(c("text_char_count", "text_word_count", "text_blank") %in% names(out$engineered_data)), "text columns exist")
}

qa_rodeo_vnext_interactions <- function() {
  out <- rodeo_fit_transform_feature_plan(rodeo_vnext_fixture(), rodeo_vnext_test_plan())
  rodeo_vnext_qa_result("interactions", all(c("x_x_y", "cat_x_cat2") %in% names(out$engineered_data)), "interaction columns exist")
}

qa_rodeo_vnext_fit_transform <- function() {
  data <- rodeo_vnext_fixture()
  plan <- rodeo_vnext_test_plan()
  fitted <- rodeo_fit_feature_plan(data, plan)
  transformed <- rodeo_transform_feature_plan(data, fitted)
  rodeo_vnext_qa_result("fit_transform", inherits(fitted, "rodeo_fitted_feature_plan") && nrow(transformed) == nrow(data), "fit/transform reusable")
}

qa_generate_rodeo_feature_engineering_artifacts <- function() {
  out <- generate_rodeo_feature_engineering_artifacts(rodeo_vnext_fixture(), rodeo_vnext_test_plan())
  rodeo_vnext_qa_result("artifact_generator", all(c("artifacts", "metadata", "warnings", "diagnostics", "value") %in% names(out)), "structured output")
}

qa_rodeo_vnext <- function() {
  data.table::rbindlist(list(
    qa_rodeo_vnext_numeric(),
    qa_rodeo_vnext_categorical(),
    qa_rodeo_vnext_calendar(),
    qa_rodeo_vnext_text(),
    qa_rodeo_vnext_interactions(),
    qa_rodeo_vnext_fit_transform(),
    qa_generate_rodeo_feature_engineering_artifacts()
  ), use.names = TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
