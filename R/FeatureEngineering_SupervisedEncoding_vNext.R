rodeo_vnext_supervised_method <- function(x) {
  match.arg(tolower(as.character(x %||% "none")[[1L]]),
    c("none", "target_mean", "woe"))
}

rodeo_vnext_encoding_name <- function(column, method, suffix = NULL) {
  suffix <- suffix %||% switch(method, target_mean = "_target_mean", woe = "_woe")
  paste0(column, suffix)
}

rodeo_vnext_target_numeric <- function(target, method, positive_class = NULL) {
  if (identical(method, "woe")) {
    levels <- unique(stats::na.omit(as.character(target)))
    if (length(levels) != 2L) stop("WOE encoding requires a binary target.", call. = FALSE)
    positive <- as.character(positive_class %||% levels[[2L]])
    return(list(value = as.numeric(as.character(target) == positive),
      positive_class = positive))
  }
  if (!is.numeric(target)) stop("Target-mean encoding requires a numeric target.",
    call. = FALSE)
  list(value = as.numeric(target), positive_class = NULL)
}

rodeo_vnext_encoding_map <- function(category, target, prior, smoothing, method) {
  work <- data.table::data.table(level = as.character(category), target = target)
  work <- work[!is.na(level) & is.finite(target),
    .(n = .N, sum_target = sum(target)), by = level]
  work[, encoded := (sum_target + smoothing * prior) / (n + smoothing)]
  if (identical(method, "woe")) {
    eps <- 1e-8
    global_odds <- (prior + eps) / (1 - prior + eps)
    work[, encoded := log(((encoded + eps) / (1 - encoded + eps)) / global_odds)]
  }
  work[, .(level, encoded, n)]
}

rodeo_vnext_fit_supervised_encoding <- function(dt, config) {
  method <- rodeo_vnext_supervised_method(config$method)
  if (identical(method, "none")) return(list(specs = list(),
    manifest_rows = list(), warnings = character()))
  columns <- intersect(as.character(config$columns %||% character()), names(dt))
  target_name <- as.character(config$target %||% "")[[1L]]
  if (!nzchar(target_name) || !target_name %in% names(dt)) {
    stop("supervised_encoding$target must name a target column.", call. = FALSE)
  }
  if (!length(columns)) stop("supervised_encoding requires categorical columns.",
    call. = FALSE)
  target <- rodeo_vnext_target_numeric(dt[[target_name]], method,
    config$positive_class)
  prior <- mean(target$value, na.rm = TRUE)
  if (!is.finite(prior)) stop("Supervised encoding target has no finite values.",
    call. = FALSE)
  smoothing <- max(0, as.numeric(config$smoothing %||% 20))
  specs <- lapply(columns, function(column) list(
    column = column,
    feature = rodeo_vnext_encoding_name(column, method, config$suffix),
    method = method,
    map = rodeo_vnext_encoding_map(dt[[column]], target$value, prior,
      smoothing, method),
    prior = if (identical(method, "woe")) 0 else prior,
    raw_prior = prior,
    smoothing = smoothing,
    target = target_name,
    positive_class = target$positive_class,
    folds = max(2L, as.integer(config$folds %||% 5L)),
    seed = as.integer(config$seed %||% 1L)))
  names(specs) <- columns
  manifest <- lapply(specs, function(spec) rodeo_vnext_manifest_row(
    spec$feature, paste(spec$column, target_name, sep = ","),
    "supervised_encoding", method, TRUE))
  list(specs = specs, manifest_rows = manifest, warnings = character())
}

rodeo_vnext_apply_supervised_encoding <- function(dt, specs) {
  if (!length(specs)) return(dt)
  names <- vapply(specs, `[[`, character(1L), "feature")
  values <- lapply(specs, function(spec) {
    idx <- match(as.character(dt[[spec$column]]), spec$map$level)
    value <- spec$map$encoded[idx]
    value[is.na(value)] <- spec$prior
    value
  })
  rodeo_vnext_batch_assign(dt, names, values)
  dt
}

rodeo_vnext_fold_ids <- function(target, folds, seed) {
  set.seed(seed)
  if (length(unique(stats::na.omit(target))) > max(20L, folds * 2L)) {
    return(sample(rep_len(seq_len(folds), length(target))))
  }
  fold <- integer(length(target))
  strata <- split(seq_along(target), as.character(target), drop = TRUE)
  for (idx in strata) fold[idx] <- sample(rep_len(seq_len(folds), length(idx)))
  fold
}

rodeo_vnext_apply_oof_supervised_encoding <- function(engineered, data,
    fitted_plan) {
  specs <- fitted_plan$supervised_encoding_specs
  if (!length(specs)) return(engineered)
  source <- data.table::as.data.table(data)
  target_name <- specs[[1L]]$target
  target <- rodeo_vnext_target_numeric(source[[target_name]],
    specs[[1L]]$method, specs[[1L]]$positive_class)$value
  folds <- min(specs[[1L]]$folds, max(2L, nrow(source)))
  fold_id <- rodeo_vnext_fold_ids(target, folds, specs[[1L]]$seed)
  for (spec in specs) {
    value <- rep(spec$prior, nrow(source))
    for (fold in seq_len(folds)) {
      train <- fold_id != fold
      prior <- mean(target[train], na.rm = TRUE)
      map <- rodeo_vnext_encoding_map(source[[spec$column]][train],
        target[train], prior, spec$smoothing, spec$method)
      idx <- match(as.character(source[[spec$column]][!train]), map$level)
      encoded <- map$encoded[idx]
      fallback <- if (identical(spec$method, "woe")) 0 else prior
      encoded[is.na(encoded)] <- fallback
      value[!train] <- encoded
    }
    data.table::set(engineered, j = spec$feature, value = value)
  }
  attr(engineered, "rodeo_supervised_encoding") <- list(
    training_values = "out_of_fold", folds = folds,
    scoring_values = "full_training_map")
  engineered
}

#' QA leakage-safe supervised encoding
#' @export
qa_rodeo_vnext_supervised_encoding <- function() {
  data <- data.table::data.table(
    category = paste0("unique_", seq_len(120L)),
    binary_group = rep(c("a", "b"), each = 60L),
    target = rep(c(0, 1), 60L))
  plan <- rodeo_feature_plan(
    supervised_encoding = list(columns = c("category", "binary_group"),
      target = "target", method = "target_mean", smoothing = 0,
      folds = 5L, seed = 11L))
  result <- rodeo_fit_transform_feature_plan(data, plan)
  score <- data.table::data.table(category = c("unique_1", "new"),
    binary_group = c("a", "new"), target = c(0, 0))
  scored <- rodeo_transform_feature_plan(score, result$fitted_plan)
  oof <- result$engineered_data$category_target_mean
  woe_plan <- rodeo_feature_plan(supervised_encoding = list(
    columns = "binary_group", target = "target", method = "woe",
    smoothing = 2, folds = 5L, seed = 11L, positive_class = "1"))
  woe <- rodeo_fit_transform_feature_plan(data, woe_plan)
  data.table::data.table(
    test = c("unique_levels_do_not_leak", "scoring_map_reused",
      "unseen_uses_prior", "oof_contract_recorded", "woe_is_finite",
      "manifest_marks_supervised"),
    passed = c(!isTRUE(all.equal(oof, data$target)),
      isTRUE(all.equal(scored$category_target_mean[[1L]], 0)),
      isTRUE(all.equal(scored$category_target_mean[[2L]], 0.5)),
      identical(attr(result$engineered_data, "rodeo_supervised_encoding")$training_values,
        "out_of_fold"), all(is.finite(woe$engineered_data$binary_group_woe)),
      any(result$fitted_plan$feature_manifest$family == "supervised_encoding")),
    detail = c("cross-fitted values differ from row targets",
      "known levels use fitted full-data map", "new levels use fitted prior",
      "training/scoring semantics are explicit", "smoothed WOE remains finite",
      "artifact lineage identifies target-derived features"))
}
