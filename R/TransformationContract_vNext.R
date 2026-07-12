#' Create a structured Rodeo transformation specification
#'
#' @description
#' Creates a deterministic, serializable transformation specification. The spec
#' stores metadata and parameters only; it never stores executable code.
#'
#' @export
rodeo_transformation_spec <- function(
  type = c("missing_impute", "constant_remove", "near_zero_variance_remove", "factor_levels", "date_features"),
  id = NULL,
  input_columns = character(),
  output_columns = NULL,
  parameters = list(),
  version = "0.1.0",
  metadata = list()
) {
  type <- match.arg(type)
  input_columns <- rodeo_contract_cols(input_columns)
  id <- rodeo_contract_scalar(id, paste0(type, "_", digest_simple(c(type, input_columns, names(parameters)))))
  spec <- list(
    id = id,
    type = type,
    input_columns = input_columns,
    output_columns = if (is.null(output_columns)) NULL else rodeo_contract_cols(output_columns, unique_values = FALSE),
    parameters = parameters %||% list(),
    learned_state = list(),
    schema_metadata = list(),
    version = as.character(version)[1L],
    warnings = character(),
    diagnostics = data.table::data.table(check = character(), status = character(), detail = character()),
    metadata = metadata %||% list(),
    created_at = Sys.time()
  )
  class(spec) <- c("rodeo_transformation_spec", "list")
  spec
}

#' Fit a structured Rodeo transformation specification
#'
#' @description
#' Fits a transformation on training data and returns a fitted specification.
#' Apply must use the fitted learned state and must not recompute it.
#'
#' @export
rodeo_fit_transformation <- function(data, spec) {
  if (!inherits(spec, "rodeo_transformation_spec")) {
    stop("spec must be returned by rodeo_transformation_spec().", call. = FALSE)
  }
  dt <- data.table::as.data.table(data)
  if (!ncol(dt)) {
    stop("data must contain at least one column.", call. = FALSE)
  }
  input_columns <- rodeo_contract_resolve_columns(spec$input_columns, dt)
  missing_columns <- setdiff(input_columns, names(dt))
  if (length(missing_columns)) {
    stop(paste("Required input columns are missing:", paste(missing_columns, collapse = ", ")), call. = FALSE)
  }

  input_schema <- rodeo_contract_schema(dt)
  warnings <- character()
  diagnostics <- list()
  columns_added <- character()
  columns_removed <- character()
  learned_state <- list()
  output_columns <- spec$output_columns

  add_diag <- function(check, status, detail = "") {
    diagnostics[[length(diagnostics) + 1L]] <<- list(check = check, status = status, detail = as.character(detail))
  }

  if (identical(spec$type, "missing_impute")) {
    method <- rodeo_contract_scalar(spec$parameters$method, "median_mode")
    constant_values <- spec$parameters$constant_values %||% list()
    replacements <- list()
    skipped <- character()
    for (col in input_columns) {
      x <- dt[[col]]
      replacement <- NULL
      if (identical(method, "median_mode")) {
        if (is.numeric(x) || is.integer(x)) {
          replacement <- stats::median(x, na.rm = TRUE)
        } else {
          replacement <- rodeo_contract_mode(x)
        }
      } else if (identical(method, "zero_unknown")) {
        replacement <- if (is.numeric(x) || is.integer(x)) 0 else "Unknown"
      } else if (identical(method, "constant")) {
        replacement <- constant_values[[col]]
      } else {
        stop("missing_impute method must be one of median_mode, zero_unknown, or constant.", call. = FALSE)
      }
      if (is.null(replacement) || length(replacement) < 1L || all(is.na(replacement))) {
        skipped <- c(skipped, col)
      } else {
        replacements[[col]] <- replacement[[1L]]
      }
    }
    learned_state <- list(method = method, replacements = replacements, skipped_columns = skipped)
    warnings <- c(warnings, if (length(skipped)) paste("No imputation value learned for:", paste(skipped, collapse = ", ")))
    add_diag("learned_replacements", "ok", paste(names(replacements), collapse = ", "))
    if (length(skipped)) add_diag("skipped_columns", "warning", paste(skipped, collapse = ", "))
  } else if (identical(spec$type, "constant_remove")) {
    removed <- input_columns[vapply(input_columns, function(col) data.table::uniqueN(dt[[col]], na.rm = FALSE) <= 1L, logical(1))]
    protected <- rodeo_contract_cols(spec$parameters$protected_columns)
    removed <- setdiff(removed, protected)
    columns_removed <- removed
    learned_state <- list(removed_columns = removed, protected_columns = protected)
    add_diag("removed_columns", "ok", paste(removed, collapse = ", "))
  } else if (identical(spec$type, "near_zero_variance_remove")) {
    threshold <- suppressWarnings(as.numeric(spec$parameters$threshold %||% 0.95))
    if (is.na(threshold) || threshold <= 0 || threshold > 1) {
      stop("near_zero_variance_remove threshold must be greater than 0 and less than or equal to 1.", call. = FALSE)
    }
    protected <- rodeo_contract_cols(spec$parameters$protected_columns)
    removed <- character()
    skipped <- character()
    for (col in input_columns) {
      x <- dt[[col]]
      if (!is.numeric(x) && !is.integer(x)) {
        skipped <- c(skipped, col)
        next
      }
      usable <- x[!is.na(x)]
      if (!length(usable)) {
        removed <- c(removed, col)
        next
      }
      tab <- sort(table(usable), decreasing = TRUE)
      if (as.numeric(tab[[1L]]) / length(usable) >= threshold) removed <- c(removed, col)
    }
    removed <- setdiff(removed, protected)
    columns_removed <- removed
    learned_state <- list(removed_columns = removed, skipped_columns = skipped, threshold = threshold, protected_columns = protected)
    add_diag("removed_columns", "ok", paste(removed, collapse = ", "))
    if (length(skipped)) add_diag("skipped_columns", "warning", paste(skipped, collapse = ", "))
  } else if (identical(spec$type, "factor_levels")) {
    unseen_level <- rodeo_contract_scalar(spec$parameters$unseen_level, "__UNSEEN__")
    include_missing <- isTRUE(spec$parameters$include_missing_level)
    levels <- list()
    skipped <- character()
    for (col in input_columns) {
      x <- dt[[col]]
      if (!is.character(x) && !is.factor(x)) {
        skipped <- c(skipped, col)
        next
      }
      vals <- sort(unique(as.character(x[!is.na(x)])))
      if (include_missing) vals <- unique(c(vals, "__MISSING__"))
      levels[[col]] <- unique(c(vals, unseen_level))
    }
    learned_state <- list(levels = levels, unseen_level = unseen_level, include_missing_level = include_missing, skipped_columns = skipped)
    add_diag("learned_levels", "ok", paste(names(levels), collapse = ", "))
    if (length(skipped)) add_diag("skipped_columns", "warning", paste(skipped, collapse = ", "))
  } else if (identical(spec$type, "date_features")) {
    features <- rodeo_contract_cols(spec$parameters$features %||% c("year", "month", "dow"))
    valid_features <- c("year", "month", "day", "dow", "week", "quarter", "is_weekend")
    bad_features <- setdiff(features, valid_features)
    if (length(bad_features)) {
      stop(paste("Unsupported date features:", paste(bad_features, collapse = ", ")), call. = FALSE)
    }
    generated <- unlist(lapply(input_columns, function(col) paste0(col, "_", features)), use.names = FALSE)
    if (!is.null(output_columns) && length(output_columns) != length(generated)) {
      stop("date_features output_columns must match the number of generated feature columns.", call. = FALSE)
    }
    output_columns <- output_columns %||% generated
    columns_added <- output_columns
    invalid <- input_columns[!vapply(input_columns, function(col) inherits(dt[[col]], c("Date", "POSIXct", "POSIXlt")) || !all(is.na(suppressWarnings(as.Date(dt[[col]])))), logical(1))]
    learned_state <- list(features = features, generated_columns = output_columns, invalid_columns = invalid)
    warnings <- c(warnings, if (length(invalid)) paste("Date conversion failed for:", paste(invalid, collapse = ", ")))
    add_diag("generated_columns", "ok", paste(output_columns, collapse = ", "))
    if (length(invalid)) add_diag("incompatible_columns", "warning", paste(invalid, collapse = ", "))
  }

  fitted <- spec
  fitted$input_columns <- input_columns
  fitted$output_columns <- output_columns
  fitted$learned_state <- learned_state
  fitted$schema_metadata <- list(input_schema = input_schema)
  fitted$warnings <- unique(warnings[!is.na(warnings) & nzchar(warnings)])
  fitted$diagnostics <- rodeo_contract_diagnostics(diagnostics)
  fitted$metadata <- c(fitted$metadata, list(
    fitted_at = Sys.time(),
    columns_added = columns_added,
    columns_removed = columns_removed,
    transformation_summary = rodeo_contract_summary(spec$type, columns_added, columns_removed)
  ))
  class(fitted) <- c("rodeo_fitted_transformation", "rodeo_transformation_spec", "list")
  fitted
}

#' Apply a fitted Rodeo transformation specification
#'
#' @export
rodeo_apply_transformation <- function(data, fitted_spec, copy_data = TRUE) {
  if (!inherits(fitted_spec, "rodeo_fitted_transformation")) {
    stop("fitted_spec must be returned by rodeo_fit_transformation().", call. = FALSE)
  }
  validation <- rodeo_validate_transformation_schema(data, fitted_spec)
  if (!isTRUE(validation$valid)) {
    stop(paste(validation$errors, collapse = " | "), call. = FALSE)
  }
  dt <- data.table::as.data.table(data)
  if (copy_data) dt <- data.table::copy(dt)

  if (identical(fitted_spec$type, "missing_impute")) {
    for (col in names(fitted_spec$learned_state$replacements)) {
      missing <- is.na(dt[[col]])
      if (any(missing)) data.table::set(dt, which(missing), col, fitted_spec$learned_state$replacements[[col]])
    }
  } else if (identical(fitted_spec$type, "constant_remove")) {
    remove <- intersect(fitted_spec$learned_state$removed_columns, names(dt))
    if (length(remove)) dt[, (remove) := NULL]
  } else if (identical(fitted_spec$type, "near_zero_variance_remove")) {
    remove <- intersect(fitted_spec$learned_state$removed_columns, names(dt))
    if (length(remove)) dt[, (remove) := NULL]
  } else if (identical(fitted_spec$type, "factor_levels")) {
    unseen <- fitted_spec$learned_state$unseen_level
    include_missing <- isTRUE(fitted_spec$learned_state$include_missing_level)
    for (col in names(fitted_spec$learned_state$levels)) {
      vals <- as.character(dt[[col]])
      if (include_missing) vals[is.na(vals)] <- "__MISSING__"
      vals[!is.na(vals) & !vals %in% fitted_spec$learned_state$levels[[col]]] <- unseen
      dt[[col]] <- factor(vals, levels = fitted_spec$learned_state$levels[[col]])
    }
  } else if (identical(fitted_spec$type, "date_features")) {
    new_names <- character()
    new_values <- list()
    features <- fitted_spec$learned_state$features
    out_idx <- 0L
    for (col in fitted_spec$input_columns) {
      date_value <- as.Date(dt[[col]])
      d <- as.POSIXlt(date_value)
      for (feature in features) {
        out_idx <- out_idx + 1L
        new_names <- c(new_names, fitted_spec$output_columns[[out_idx]])
        new_values[[length(new_values) + 1L]] <- switch(
          feature,
          year = d$year + 1900L,
          month = d$mon + 1L,
          day = d$mday,
          dow = as.integer(format(date_value, "%u")),
          week = as.integer(format(date_value, "%U")),
          quarter = ((d$mon) %/% 3L) + 1L,
          is_weekend = as.integer(format(date_value, "%u") %in% c("6", "7"))
        )
      }
    }
    if (length(new_names)) dt[, (new_names) := new_values]
  }

  attr(dt, "rodeo_transformation_metadata") <- rodeo_transformation_metadata(fitted_spec, data = dt)
  dt
}

#' Fit and apply a structured Rodeo transformation
#'
#' @export
rodeo_fit_apply_transformation <- function(data, spec, copy_data = TRUE) {
  fitted <- rodeo_fit_transformation(data, spec)
  transformed <- rodeo_apply_transformation(data, fitted, copy_data = copy_data)
  list(transformed_data = transformed, fitted_spec = fitted)
}

#' Validate data against a fitted transformation schema
#'
#' @export
rodeo_validate_transformation_schema <- function(data, fitted_spec) {
  if (!inherits(fitted_spec, "rodeo_fitted_transformation")) {
    stop("fitted_spec must be returned by rodeo_fit_transformation().", call. = FALSE)
  }
  dt <- data.table::as.data.table(data)
  errors <- character()
  warnings <- character()
  missing_columns <- setdiff(fitted_spec$input_columns, names(dt))
  if (length(missing_columns)) errors <- c(errors, paste("Required input columns are missing:", paste(missing_columns, collapse = ", ")))

  if (identical(fitted_spec$type, "date_features")) {
    output_columns <- fitted_spec$output_columns %||% character()
    duplicate_outputs <- output_columns[duplicated(output_columns)]
    if (length(duplicate_outputs)) errors <- c(errors, paste("Duplicated output columns:", paste(unique(duplicate_outputs), collapse = ", ")))
    existing_outputs <- intersect(output_columns, names(dt))
    if (length(existing_outputs)) errors <- c(errors, paste("Output columns already exist:", paste(existing_outputs, collapse = ", ")))
  }

  input_schema <- fitted_spec$schema_metadata$input_schema
  if (is.data.frame(input_schema) && nrow(input_schema)) {
    for (col in intersect(fitted_spec$input_columns, names(dt))) {
      expected <- input_schema$class[input_schema$column == col][1L]
      actual <- paste(class(dt[[col]]), collapse = "/")
      if (!is.na(expected) && nzchar(expected) && !identical(expected, actual)) {
        warnings <- c(warnings, paste("Column type changed for", col, "from", expected, "to", actual))
      }
    }
  }

  list(
    valid = !length(errors),
    errors = errors,
    warnings = warnings,
    diagnostics = data.table::data.table(
      check = c("required_columns", "output_columns", "column_types"),
      status = c(if (length(missing_columns)) "error" else "ok", if (length(errors) && !length(missing_columns)) "error" else "ok", if (length(warnings)) "warning" else "ok"),
      detail = c(paste(missing_columns, collapse = ", "), paste(fitted_spec$output_columns %||% character(), collapse = ", "), paste(warnings, collapse = " | "))
    )
  )
}

#' Save a fitted Rodeo transformation specification
#'
#' @export
rodeo_save_transformation <- function(fitted_spec, path) {
  if (!inherits(fitted_spec, "rodeo_fitted_transformation")) {
    stop("fitted_spec must be returned by rodeo_fit_transformation().", call. = FALSE)
  }
  saveRDS(fitted_spec, file = path, version = 3)
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

#' Load a fitted Rodeo transformation specification
#'
#' @export
rodeo_load_transformation <- function(path) {
  fitted <- readRDS(path)
  if (!inherits(fitted, "rodeo_fitted_transformation")) {
    stop("Serialized object is not a fitted Rodeo transformation.", call. = FALSE)
  }
  fitted
}

#' Read fitted Rodeo transformation metadata
#'
#' @export
rodeo_transformation_metadata <- function(fitted_spec, data = NULL) {
  if (!inherits(fitted_spec, "rodeo_fitted_transformation")) {
    stop("fitted_spec must be returned by rodeo_fit_transformation().", call. = FALSE)
  }
  output_schema <- if (is.null(data)) data.table::data.table() else rodeo_contract_schema(data.table::as.data.table(data))
  list(
    id = fitted_spec$id,
    type = fitted_spec$type,
    version = fitted_spec$version,
    input_columns = fitted_spec$input_columns,
    output_columns = fitted_spec$output_columns %||% character(),
    columns_added = fitted_spec$metadata$columns_added %||% character(),
    columns_removed = fitted_spec$metadata$columns_removed %||% character(),
    parameters = fitted_spec$parameters,
    learned_state = fitted_spec$learned_state,
    input_schema = fitted_spec$schema_metadata$input_schema,
    output_schema = output_schema,
    warnings = fitted_spec$warnings,
    diagnostics = fitted_spec$diagnostics,
    transformation_summary = fitted_spec$metadata$transformation_summary %||% ""
  )
}

#' QA for the structured Rodeo transformation contract
#'
qa_rodeo_transformation_contract <- function() {
  data <- data.table::data.table(
    id = 1:8,
    x = c(1, 1, 1, 1, 1, 1, 2, NA),
    y = c(10, 11, NA, 13, 14, 15, 16, 17),
    z = 1,
    cat = c("A", "A", "B", NA, "B", "C", "C", "C"),
    event_date = as.Date("2026-01-01") + 0:7
  )
  spec_impute <- rodeo_transformation_spec("missing_impute", input_columns = c("x", "y", "cat"), parameters = list(method = "median_mode"))
  fitted_impute <- rodeo_fit_transformation(data, spec_impute)
  applied_impute <- rodeo_apply_transformation(data, fitted_impute)
  repeated_impute <- rodeo_apply_transformation(data, fitted_impute)

  spec_const <- rodeo_transformation_spec("constant_remove", input_columns = names(data), parameters = list(protected_columns = "id"))
  fitted_const <- rodeo_fit_transformation(data, spec_const)
  applied_const <- rodeo_apply_transformation(data, fitted_const)

  spec_nzv <- rodeo_transformation_spec("near_zero_variance_remove", input_columns = c("x", "y", "cat"), parameters = list(threshold = 0.75))
  fitted_nzv <- rodeo_fit_transformation(data, spec_nzv)

  spec_factor <- rodeo_transformation_spec("factor_levels", input_columns = "cat", parameters = list(unseen_level = "__UNSEEN__", include_missing_level = TRUE))
  fitted_factor <- rodeo_fit_transformation(data, spec_factor)
  score_factor <- data.table::copy(data)
  score_factor$cat[1L] <- "NEW"
  applied_factor <- rodeo_apply_transformation(score_factor, fitted_factor)

  spec_date <- rodeo_transformation_spec("date_features", input_columns = "event_date", parameters = list(features = c("year", "month", "dow")))
  fitted_date <- rodeo_fit_transformation(data, spec_date)
  applied_date <- rodeo_apply_transformation(data, fitted_date)

  tmp <- tempfile(fileext = ".rds")
  rodeo_save_transformation(fitted_impute, tmp)
  loaded_impute <- rodeo_load_transformation(tmp)
  replay_impute <- rodeo_apply_transformation(data, loaded_impute)

  missing_col_result <- tryCatch({
    rodeo_apply_transformation(data[, !"y"], fitted_impute)
    FALSE
  }, error = function(e) grepl("Required input columns are missing", conditionMessage(e), fixed = TRUE))

  duplicate_output_spec <- rodeo_transformation_spec("date_features", input_columns = "event_date", output_columns = rep("dup", 3L), parameters = list(features = c("year", "month", "dow")))
  duplicate_output_fit <- rodeo_fit_transformation(data, duplicate_output_spec)
  duplicate_output_result <- tryCatch({
    rodeo_apply_transformation(data, duplicate_output_fit)
    FALSE
  }, error = function(e) grepl("Duplicated output columns", conditionMessage(e), fixed = TRUE))

  empty_result <- tryCatch({
    rodeo_fit_transformation(data.table::data.table(), spec_impute)
    FALSE
  }, error = function(e) grepl("at least one column", conditionMessage(e), fixed = TRUE))

  unsupported_type <- any(fitted_nzv$learned_state$skipped_columns == "cat")
  metadata <- rodeo_transformation_metadata(fitted_date, applied_date)

  data.table::data.table(
    test = c(
      "fit_apply_separation",
      "missing_imputation",
      "constant_remove",
      "near_zero_variance_remove",
      "factor_unseen_management",
      "date_feature_extraction",
      "serialization_replay",
      "schema_missing_columns",
      "schema_duplicate_outputs",
      "metadata_readable",
      "deterministic_repeated_apply",
      "empty_dataset_rejected",
      "unsupported_types_diagnostic"
    ),
    passed = c(
      inherits(fitted_impute, "rodeo_fitted_transformation") && !is.null(fitted_impute$learned_state$replacements),
      sum(is.na(applied_impute$y)) == 0L && sum(is.na(applied_impute$cat)) == 0L,
      !"z" %in% names(applied_const),
      "x" %in% fitted_nzv$learned_state$removed_columns,
      as.character(applied_factor$cat[1L]) == "__UNSEEN__",
      all(c("event_date_year", "event_date_month", "event_date_dow") %in% names(applied_date)),
      isTRUE(all.equal(applied_impute, replay_impute, check.attributes = FALSE)),
      isTRUE(missing_col_result),
      isTRUE(duplicate_output_result),
      is.list(metadata) && all(c("input_schema", "output_schema", "learned_state") %in% names(metadata)),
      isTRUE(all.equal(applied_impute, repeated_impute, check.attributes = FALSE)),
      isTRUE(empty_result),
      isTRUE(unsupported_type)
    ),
    detail = c(
      "Fitted spec stores learned state before apply.",
      "Apply uses fitted imputation values.",
      "Constant fitted removal drops only learned columns.",
      "Near-zero variance removal records learned removals.",
      "Unseen scoring values map to fitted unseen level.",
      "Date features are generated from fitted output schema.",
      "Loaded fitted spec replays identical apply output.",
      "Apply rejects missing required columns.",
      "Apply rejects duplicated output names.",
      "Metadata is readable without re-fitting.",
      "Repeated apply returns identical output.",
      "Empty training data is rejected.",
      "Unsupported non-numeric NZV columns are diagnostic skips."
    )
  )
}

rodeo_contract_cols <- function(x, unique_values = TRUE) {
  x <- as.character(x %||% character())
  x <- x[!is.na(x) & nzchar(x)]
  if (isTRUE(unique_values)) unique(x) else x
}

rodeo_contract_scalar <- function(x, default = "") {
  x <- rodeo_contract_cols(x)
  if (length(x)) x[[1L]] else default
}

rodeo_contract_resolve_columns <- function(cols, data) {
  cols <- rodeo_contract_cols(cols)
  if (length(cols)) cols else names(data)
}

rodeo_contract_schema <- function(dt) {
  data.table::data.table(
    column = names(dt),
    class = vapply(dt, function(x) paste(class(x), collapse = "/"), character(1)),
    missing = vapply(dt, function(x) sum(is.na(x)), integer(1)),
    unique_values = vapply(dt, function(x) data.table::uniqueN(x, na.rm = TRUE), integer(1))
  )
}

rodeo_contract_mode <- function(x) {
  values <- x[!is.na(x)]
  if (!length(values)) return(NA)
  tab <- sort(table(values), decreasing = TRUE)
  names(tab)[[1L]]
}

rodeo_contract_diagnostics <- function(rows) {
  if (!length(rows)) {
    return(data.table::data.table(check = character(), status = character(), detail = character()))
  }
  data.table::rbindlist(rows, use.names = TRUE, fill = TRUE)
}

rodeo_contract_summary <- function(type, columns_added, columns_removed) {
  paste0(
    type,
    ": added ", length(columns_added), " column(s); removed ",
    length(columns_removed), " column(s)."
  )
}

digest_simple <- function(x) {
  raw <- paste(utils::capture.output(str(x)), collapse = "|")
  sum(utf8ToInt(raw)) %% 1000000L
}
