namespace <- readLines("NAMESPACE", warn = FALSE)
exports <- sort(sub("^export\\(([^)]+)\\)$", "\\1", grep("^export\\(", namespace, value = TRUE)))

core <- c(
  "rodeo_features", "rodeo_encode", "rodeo_transform", "rodeo_temporal_features",
  "rodeo_cross_row_features", "rodeo_complete_time", "rodeo_model_prep"
)
specialized <- c(
  "Apply_Asin", "Apply_Asinh", "Apply_BoxCox", "Apply_Log", "Apply_LogPlus1",
  "Apply_Logit", "Apply_Sqrt", "Apply_YeoJohnson", "AutoClustering",
  "AutoClusteringScoring", "AutoDiffLagN", "AutoInteraction", "AutoLagRollMode",
  "AutoLagRollStats", "AutoLagRollStatsScoring", "AutoTransformationCreate",
  "AutoTransformationScore", "AutoWord2VecModeler", "AutoWord2VecScoring",
  "CategoricalEncoding", "CreateCalendarVariables", "CreateHolidayVariables",
  "DT_GDL_Feature_Engineering", "Estimate_BoxCox_Lambda",
  "Estimate_YeoJohnson_Lambda", "H2OAutoencoder", "H2OAutoencoderScoring",
  "H2OIsolationForest", "H2OIsolationForestScoring", "InvApply_Asin",
  "InvApply_Asinh", "InvApply_BoxCox", "InvApply_Log", "InvApply_LogPlus1",
  "InvApply_Logit", "InvApply_Sqrt", "InvApply_YeoJohnson",
  "Partial_DT_GDL_Feature_Engineering", "PercRank", "PercRankScoring",
  "Standardize", "StandardizeScoring", "TimeSeriesFill", "TimeSeriesFillRoll",
  "generate_rodeo_feature_engineering_artifacts",
  "generate_rodeo_model_prep_artifacts", "rodeo_apply_partition_plan",
  "rodeo_apply_temporal_transformation", "rodeo_apply_transformation",
  "rodeo_create_folds", "rodeo_feature_plan", "rodeo_fit_apply_transformation",
  "rodeo_fit_feature_plan", "rodeo_fit_partition_plan",
  "rodeo_fit_temporal_transformation", "rodeo_fit_transform_feature_plan",
  "rodeo_fit_transformation", "rodeo_load_transformation",
  "rodeo_partition_plan", "rodeo_prepare_cross_target_features",
  "rodeo_prepare_forecast_supervised_data", "rodeo_save_transformation",
  "rodeo_temporal_transformation_metadata",
  "rodeo_temporal_transformation_spec", "rodeo_transform_feature_plan",
  "rodeo_transformation_metadata", "rodeo_transformation_spec"
)

classify <- function(x) {
  if (x %in% core) return("CORE_EXPERT")
  if (x %in% specialized) return("SPECIALIZED_EXPERT")
  "UNEXPECTED_EXPORT"
}

workflow <- function(x) {
  if (grepl("Categor|Dumm|Encoding|Character|rodeo_encode", x)) return("categorical encoding")
  if (grepl("Transformation|Apply_|Test_|Standard|PercRank|rodeo_transform$|Estimate_", x)) {
    return("numeric transformation/scaling")
  }
  if (grepl("complete_time|Lag|Roll|Diff|TimeSeries|Calendar|Holiday|weekdays|LB|cross_row|temporal_features", x)) {
    return("temporal/cross-row")
  }
  if (grepl("Interaction|Interact", x)) return("interactions")
  if (grepl("Partition|model_prep|fold", x, ignore.case = TRUE)) return("model preparation")
  if (grepl("Word2Vec|Autoencoder|Isolation|Clustering", x, ignore.case = TRUE)) {
    return("model-based features")
  }
  if (grepl("artifact", x, ignore.case = TRUE)) return("artifacts")
  if (grepl("^rodeo_features$|^rodeo_feature_plan$|fit_feature|transform_feature", x)) {
    return("general feature engineering")
  }
  "general feature engineering"
}

audit <- data.frame(
  export = exports,
  classification = vapply(exports, classify, character(1)),
  workflow = vapply(exports, workflow, character(1)),
  stringsAsFactors = FALSE
)
stopifnot(nrow(audit) == length(exports), !anyNA(audit$classification))
if (any(audit$classification == "UNEXPECTED_EXPORT")) {
  stop("Unexpected public exports: ",
    paste(audit$export[audit$classification == "UNEXPECTED_EXPORT"], collapse = ", "))
}
dir.create("docs", showWarnings = FALSE)
write.csv(audit, "docs/rodeo_public_api_audit.csv", row.names = FALSE, na = "")

classification_levels <- c(
  "CORE_EXPERT", "SPECIALIZED_EXPERT", "COMPATIBILITY",
  "INTERNAL_SHOULD_NOT_BE_PUBLIC", "DEPRECATED", "DUPLICATE"
)
counts <- as.data.frame(table(factor(audit$classification, levels = classification_levels)),
  stringsAsFactors = FALSE)
names(counts) <- c("classification", "count")
write.csv(counts, "docs/rodeo_public_api_audit_counts.csv", row.names = FALSE)
