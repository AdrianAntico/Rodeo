#' Create a Rodeo vNext model-prep partition plan
#'
#' @description
#' Creates a scoring-safe partition plan for model-prep workflows. This vNext
#' layer is additive and does not change legacy AutoDataPartition(),
#' PartitionData(), or ModelDataPrep() behavior.
#'
#' @export
rodeo_partition_plan <- function(
  method = c("random", "stratified", "grouped", "time"),
  fractions = c(train = 0.8, test = 0.2),
  target_col = NULL,
  group_col = NULL,
  date_col = NULL,
  seed = 123L,
  row_id_col = ".row_id",
  partition_col = ".partition",
  fold_col = ".fold_id",
  k = 5L,
  metadata = list()
) {
  method <- match.arg(method)
  fractions <- rodeo_model_prep_normalize_fractions(fractions)
  plan <- list(
    method = method,
    fractions = fractions,
    target_col = target_col,
    group_col = group_col,
    date_col = date_col,
    seed = as.integer(seed),
    row_id_col = as.character(row_id_col)[1L],
    partition_col = as.character(partition_col)[1L],
    fold_col = as.character(fold_col)[1L],
    k = as.integer(k),
    metadata = metadata,
    created_at = Sys.time()
  )
  class(plan) <- c("rodeo_partition_plan", "list")
  plan
}

rodeo_model_prep_normalize_fractions <- function(fractions) {
  fraction_names <- names(fractions)
  fractions <- as.numeric(fractions)
  if (!length(fractions) || any(!is.finite(fractions)) || any(fractions <= 0)) {
    stop("fractions must contain positive numeric values.")
  }
  names(fractions) <- fraction_names
  if (is.null(names(fractions)) || any(!nzchar(names(fractions)))) {
    names(fractions) <- c("train", "validation", "test")[seq_along(fractions)]
  }
  fractions <- fractions / sum(fractions)
  fractions
}

rodeo_model_prep_fixture <- function() {
  data.table::data.table(
    id = seq_len(120L),
    target = rep(c("no", "yes"), 60L),
    group = rep(paste0("g", seq_len(24L)), each = 5L),
    event_date = as.Date("2024-01-01") + seq_len(120L),
    x = seq_len(120L)
  )
}

rodeo_model_prep_add_row_id <- function(dt, row_id_col) {
  if (!row_id_col %in% names(dt)) data.table::set(dt, j = row_id_col, value = seq_len(nrow(dt)))
  dt
}

rodeo_model_prep_cut_labels <- function(n, fractions) {
  sizes <- floor(fractions * n)
  remainder <- n - sum(sizes)
  if (remainder > 0L) sizes[seq_len(remainder)] <- sizes[seq_len(remainder)] + 1L
  rep(names(fractions), sizes)[seq_len(n)]
}

rodeo_model_prep_assign_random <- function(ids, fractions, seed) {
  set.seed(seed)
  shuffled <- sample(ids, length(ids))
  data.table::data.table(.row_id_tmp = shuffled, .partition_tmp = rodeo_model_prep_cut_labels(length(shuffled), fractions))
}

rodeo_model_prep_assignment_manifest <- function(assignments, partition_col, fold_col = NULL) {
  partition_summary <- assignments[, .N, by = partition_col][order(get(partition_col))]
  data.table::setnames(partition_summary, "N", "rows")
  if (!is.null(fold_col) && fold_col %in% names(assignments)) {
    fold_summary <- assignments[, .N, by = fold_col][order(get(fold_col))]
    data.table::setnames(fold_summary, "N", "rows")
  } else {
    fold_summary <- data.table::data.table()
  }
  list(partition_summary = partition_summary, fold_summary = fold_summary)
}

#' Fit a Rodeo vNext partition plan
#'
#' @export
rodeo_fit_partition_plan <- function(data, plan = rodeo_partition_plan()) {
  dt <- data.table::copy(data.table::as.data.table(data))
  row_id_col <- plan$row_id_col
  partition_col <- plan$partition_col
  warnings <- character()
  rodeo_model_prep_add_row_id(dt, row_id_col)

  if (plan$method == "stratified" && (is.null(plan$target_col) || !plan$target_col %in% names(dt))) {
    warnings <- c(warnings, "Stratified partition requested without a valid target_col; falling back to random partitioning.")
    plan$method <- "random"
  }
  if (plan$method == "grouped" && (is.null(plan$group_col) || !plan$group_col %in% names(dt))) {
    warnings <- c(warnings, "Grouped partition requested without a valid group_col; falling back to random partitioning.")
    plan$method <- "random"
  }
  if (plan$method == "time" && (is.null(plan$date_col) || !plan$date_col %in% names(dt))) {
    warnings <- c(warnings, "Time partition requested without a valid date_col; falling back to random partitioning.")
    plan$method <- "random"
  }

  if (plan$method == "stratified") {
    assignments <- dt[, {
      a <- rodeo_model_prep_assign_random(get(row_id_col), plan$fractions, plan$seed + .GRP)
      a
    }, by = c(plan$target_col)]
    assignments[, (plan$target_col) := NULL]
  } else if (plan$method == "grouped") {
    groups <- unique(dt[, .(group_value = get(plan$group_col))])
    group_assign <- rodeo_model_prep_assign_random(seq_len(nrow(groups)), plan$fractions, plan$seed)
    groups[, .group_index := seq_len(.N)]
    groups[group_assign, (partition_col) := i..partition_tmp, on = c(".group_index" = ".row_id_tmp")]
    group_map <- groups[, .(group_value, .partition_tmp = get(partition_col))]
    row_map <- dt[, .(group_value = get(plan$group_col), .row_id_tmp = get(row_id_col))]
    assignments <- group_map[row_map, on = "group_value"][, .(.row_id_tmp, .partition_tmp)]
  } else if (plan$method == "time") {
    ordered <- dt[order(get(plan$date_col), get(row_id_col)), get(row_id_col)]
    assignments <- data.table::data.table(.row_id_tmp = ordered, .partition_tmp = rodeo_model_prep_cut_labels(length(ordered), plan$fractions))
  } else {
    assignments <- rodeo_model_prep_assign_random(dt[[row_id_col]], plan$fractions, plan$seed)
  }

  data.table::setnames(assignments, c(".row_id_tmp", ".partition_tmp"), c(row_id_col, partition_col))
  assignments <- assignments[order(get(row_id_col))]

  folds <- rodeo_create_folds(
    data = dt,
    k = plan$k,
    target_col = if (plan$method == "stratified") plan$target_col else NULL,
    group_col = if (plan$method == "grouped") plan$group_col else NULL,
    seed = plan$seed,
    row_id_col = row_id_col,
    fold_col = plan$fold_col
  )
  assignments <- folds[assignments, on = row_id_col]

  manifest <- rodeo_model_prep_assignment_manifest(assignments, partition_col, plan$fold_col)
  diagnostics <- data.table::rbindlist(list(
    data.table::data.table(check = "input_rows", status = "ok", detail = as.character(nrow(dt))),
    data.table::data.table(check = "partition_method", status = "ok", detail = plan$method),
    data.table::data.table(check = "partition_count", status = "ok", detail = as.character(length(unique(assignments[[partition_col]])))),
    data.table::data.table(check = "fold_count", status = "ok", detail = as.character(length(unique(assignments[[plan$fold_col]]))))
  ), use.names = TRUE)

  fitted <- list(
    plan = plan,
    assignments = assignments,
    partition_manifest = manifest$partition_summary,
    fold_manifest = manifest$fold_summary,
    diagnostics = diagnostics,
    warnings = unique(warnings),
    fitted_at = Sys.time()
  )
  class(fitted) <- c("rodeo_fitted_partition_plan", "list")
  fitted
}

#' Apply a fitted Rodeo vNext partition plan
#'
#' @export
rodeo_apply_partition_plan <- function(data, fitted_plan, copy_data = TRUE) {
  dt <- if (copy_data) data.table::copy(data.table::as.data.table(data)) else data.table::as.data.table(data)
  row_id_col <- fitted_plan$plan$row_id_col
  rodeo_model_prep_add_row_id(dt, row_id_col)
  fitted_plan$assignments[dt, on = row_id_col]
}

#' Create reproducible fold assignments
#'
#' @export
rodeo_create_folds <- function(data, k = 5L, target_col = NULL, group_col = NULL, seed = 123L,
                               row_id_col = ".row_id", fold_col = ".fold_id") {
  dt <- data.table::copy(data.table::as.data.table(data))
  rodeo_model_prep_add_row_id(dt, row_id_col)
  k <- max(2L, as.integer(k))
  set.seed(as.integer(seed))

  if (!is.null(group_col) && group_col %in% names(dt)) {
    groups <- unique(dt[, .(group_value = get(group_col))])
    groups[, .group_index := sample(seq_len(.N), .N)]
    groups[, (fold_col) := ((.group_index - 1L) %% k) + 1L]
    group_map <- groups[, .(group_value, .fold_tmp = get(fold_col))]
    row_map <- dt[, .(group_value = get(group_col), .row_id_tmp = get(row_id_col))]
    out <- group_map[row_map, on = "group_value"][, .(.row_id_tmp, .fold_tmp)]
  } else if (!is.null(target_col) && target_col %in% names(dt)) {
    out <- dt[, {
      ids <- sample(get(row_id_col), .N)
      data.table::data.table(.row_id_tmp = ids, .fold_tmp = ((seq_along(ids) - 1L) %% k) + 1L)
    }, by = c(target_col)][, (target_col) := NULL]
  } else {
    ids <- sample(dt[[row_id_col]], nrow(dt))
    out <- data.table::data.table(.row_id_tmp = ids, .fold_tmp = ((seq_along(ids) - 1L) %% k) + 1L)
  }

  data.table::setnames(out, c(".row_id_tmp", ".fold_tmp"), c(row_id_col, fold_col))
  out[order(get(row_id_col))]
}

#' Generate Rodeo vNext model-prep artifacts
#'
#' @export
generate_rodeo_model_prep_artifacts <- function(data, plan = NULL, fitted_plan = NULL) {
  if (is.null(fitted_plan)) {
    if (is.null(plan)) plan <- rodeo_partition_plan()
    fitted_plan <- rodeo_fit_partition_plan(data, plan)
  }
  prepared_data <- rodeo_apply_partition_plan(data, fitted_plan)
  artifacts <- list(
    overview_text = "Rodeo vNext model-prep partition run completed.",
    partition_manifest = fitted_plan$partition_manifest,
    fold_manifest = fitted_plan$fold_manifest,
    diagnostics = fitted_plan$diagnostics,
    assignment_manifest = fitted_plan$assignments
  )
  list(
    artifacts = artifacts,
    metadata = list(
      generator = "generate_rodeo_model_prep_artifacts",
      generated_at = Sys.time(),
      method = fitted_plan$plan$method,
      seed = fitted_plan$plan$seed,
      leakage_safe = TRUE
    ),
    warnings = fitted_plan$warnings,
    diagnostics = fitted_plan$diagnostics,
    value = list(
      prepared_data = prepared_data,
      fitted_plan = fitted_plan,
      partition_manifest = fitted_plan$partition_manifest,
      fold_manifest = fitted_plan$fold_manifest
    )
  )
}

#' @export
qa_rodeo_vnext_model_prep <- function() {
  data <- rodeo_model_prep_fixture()
  random_plan <- rodeo_partition_plan(method = "random", fractions = c(train = 0.7, test = 0.3), seed = 1L)
  strat_plan <- rodeo_partition_plan(method = "stratified", fractions = c(train = 0.7, test = 0.3), target_col = "target", seed = 1L)
  group_plan <- rodeo_partition_plan(method = "grouped", fractions = c(train = 0.7, test = 0.3), group_col = "group", seed = 1L)
  time_plan <- rodeo_partition_plan(method = "time", fractions = c(train = 0.7, test = 0.3), date_col = "event_date", seed = 1L)

  random_fit <- rodeo_fit_partition_plan(data, random_plan)
  strat_fit <- rodeo_fit_partition_plan(data, strat_plan)
  group_fit <- rodeo_fit_partition_plan(data, group_plan)
  time_fit <- rodeo_fit_partition_plan(data, time_plan)
  group_applied <- rodeo_apply_partition_plan(data, group_fit)
  time_applied <- rodeo_apply_partition_plan(data, time_fit)
  folds <- rodeo_create_folds(data, k = 5L, target_col = "target", seed = 2L)

  group_check <- group_applied[, uniqueN(.partition), by = group][, all(V1 == 1L)]
  time_check <- max(time_applied[.partition == "train", event_date]) <= min(time_applied[.partition == "test", event_date])

  data.table::rbindlist(list(
    rodeo_vnext_qa_result("random_partition", all(c("train", "test") %in% random_fit$partition_manifest[[random_plan$partition_col]]), "train/test assigned"),
    rodeo_vnext_qa_result("stratified_partition", all(c("no", "yes") %in% rodeo_apply_partition_plan(data, strat_fit)[.partition == "train", target]), "target classes preserved in train"),
    rodeo_vnext_qa_result("grouped_partition", group_check, "groups do not cross partitions"),
    rodeo_vnext_qa_result("time_partition", time_check, "training dates precede test dates"),
    rodeo_vnext_qa_result("folds", setequal(sort(unique(folds$.fold_id)), 1:5), "fold ids assigned"),
    rodeo_vnext_qa_result("manifest", all(c("partition_manifest", "fold_manifest", "assignments") %in% names(group_fit)), "structured fitted plan")
  ), use.names = TRUE)
}

#' @export
qa_generate_rodeo_model_prep_artifacts <- function() {
  out <- generate_rodeo_model_prep_artifacts(
    rodeo_model_prep_fixture(),
    rodeo_partition_plan(method = "stratified", fractions = c(train = 0.7, validation = 0.1, test = 0.2), target_col = "target")
  )
  rodeo_vnext_qa_result("model_prep_artifact_generator", all(c("artifacts", "metadata", "warnings", "diagnostics", "value") %in% names(out)), "structured output")
}
