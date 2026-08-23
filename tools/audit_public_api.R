namespace <- readLines("NAMESPACE", warn = FALSE)
exports <- sort(sub("^export\\(([^)]+)\\)$", "\\1", grep("^export\\(", namespace, value = TRUE)))

core <- c("rodeo_features", "rodeo_temporal_features", "rodeo_model_prep")
missing <- c(
  "CategoricalEncoding", "Standardize", "StandardizeScoring", "PercRank",
  "PercRankScoring", "AutoTransformationCreate", "AutoTransformationScore",
  "AutoLagRollMode", "AutoLagRollStats", "AutoLagRollStatsScoring",
  "TimeSeriesFill", "TimeSeriesFillRoll", "AutoWord2VecModeler",
  "AutoWord2VecScoring", "H2OAutoencoder", "H2OAutoencoderScoring",
  "H2OIsolationForest", "H2OIsolationForestScoring", "AutoClustering",
  "AutoClusteringScoring", "rodeo_transformation_spec",
  "rodeo_fit_transformation", "rodeo_apply_transformation",
  "rodeo_fit_apply_transformation", "rodeo_save_transformation",
  "rodeo_load_transformation", "rodeo_transformation_metadata"
)
compatibility <- c(
  "Apply_Asin", "Apply_Asinh", "Apply_BoxCox", "Apply_Log", "Apply_LogPlus1",
  "Apply_Logit", "Apply_Sqrt", "Apply_YeoJohnson", "InvApply_Asin",
  "InvApply_Asinh", "InvApply_BoxCox", "InvApply_Log", "InvApply_LogPlus1",
  "InvApply_Logit", "InvApply_Sqrt", "InvApply_YeoJohnson", "Test_Asin",
  "Test_Asinh", "Test_BoxCox", "Test_Identity", "Test_Log", "Test_LogPlus1",
  "Test_Logit", "Test_Sqrt", "Test_YeoJohnson", "Estimate_BoxCox_Lambda",
  "Estimate_YeoJohnson_Lambda", "DummifyDT", "CreateCalendarVariables",
  "CreateHolidayVariables", "AutoInteraction", "Interact", "CreateInteractions",
  "AutoDiffLagN", "DiffDT", "DiffLagN", "DT_GDL_Feature_Engineering",
  "Partial_DT_GDL_Feature_Engineering", "Partial_DT_GDL_Feature_Engineering2",
  "AutoDataPartition", "ModelDataPrep", "PartitionData"
)
duplicates <- c(
  "DummyVariables", "EncodeCharacterVariables", "Encoding", "CalendarVariables",
  "HolidayVariables", "TimeSeriesFeatures", "Word2Vec_H2O", "AutoEncoder_H2O",
  "IsolationForest_H2O", "Clustering_H2O"
)
internal <- c(
  "BenchmarkData", "BuildBinary", "FakeDataGenerator", "Install", "UpdateDocs",
  "LB", "MEOW", "Mode", "weekdays_in_month", "qa_rodeo_package",
  "qa_rodeo_preferred_api", "qa_rodeo_vnext_supervised_encoding",
  "rodeo_temporal_coerce_index", "rodeo_temporal_frequency_is_intraday",
  "rodeo_temporal_next_index", "rodeo_temporal_normalize_frequency",
  "rodeo_temporal_prediction_frame", "rodeo_temporal_resolve_frequency",
  "rodeo_validate_temporal_schema", "rodeo_validate_transformation_schema"
)
specialized <- c(
  "generate_rodeo_feature_engineering_artifacts", "generate_rodeo_model_prep_artifacts",
  "rodeo_apply_partition_plan", "rodeo_create_folds", "rodeo_feature_plan",
  "rodeo_fit_feature_plan", "rodeo_fit_partition_plan",
  "rodeo_fit_temporal_transformation", "rodeo_fit_transform_feature_plan",
  "rodeo_partition_plan", "rodeo_prepare_cross_target_features",
  "rodeo_prepare_forecast_supervised_data", "rodeo_temporal_transformation_metadata",
  "rodeo_temporal_transformation_spec", "rodeo_transform_feature_plan"
)

classify <- function(x) {
  if (x %in% core) return("CORE_EXPERT_ENTRY_POINT")
  if (x %in% missing) return("MISSING_FROM_PREFERRED_SURFACE")
  if (x %in% compatibility) return("COMPATIBILITY_ALIAS")
  if (x %in% duplicates) return("DUPLICATE")
  if (x %in% internal) return("INTERNAL/SHOULD_NOT_BE_PUBLIC")
  if (x %in% specialized) return("SPECIALIZED_EXPERT_FUNCTION")
  "SPECIALIZED_EXPERT_FUNCTION"
}

workflow <- function(x) {
  if (grepl("Categor|Dumm|Encoding|Character", x)) return("categorical encoding")
  if (grepl("Transformation|Apply_|Test_|Standard|PercRank", x)) return("numeric transformation/scaling")
  if (grepl("Temporal|Lag|Roll|Diff|TimeSeries|Calendar|Holiday|weekdays|LB", x)) return("temporal/cross-row")
  if (grepl("Interaction|Interact|MEOW", x)) return("interactions")
  if (grepl("Partition|ModelPrep|fold", x, ignore.case = TRUE)) return("model preparation")
  if (grepl("Word2Vec|Autoencoder|Isolation|Clustering", x, ignore.case = TRUE)) return("model-based features")
  if (grepl("artifact", x, ignore.case = TRUE)) return("artifacts")
  if (grepl("qa_|Benchmark|Fake|Install|Update|BuildBinary|Mode", x)) return("package utility/QA")
  "general feature engineering"
}

audit <- data.frame(
  export = exports,
  classification = vapply(exports, classify, character(1)),
  workflow = vapply(exports, workflow, character(1)),
  stringsAsFactors = FALSE
)
stopifnot(nrow(audit) == length(exports), !anyNA(audit$classification))
dir.create("docs", showWarnings = FALSE)
write.csv(audit, "docs/rodeo_public_api_audit.csv", row.names = FALSE, na = "")

classification_levels <- c(
  "CORE_EXPERT_ENTRY_POINT", "SPECIALIZED_EXPERT_FUNCTION",
  "COMPATIBILITY_ALIAS", "INTERNAL/SHOULD_NOT_BE_PUBLIC", "DUPLICATE",
  "DEPRECATED", "MISSING_FROM_PREFERRED_SURFACE"
)
counts <- as.data.frame(table(factor(audit$classification, levels = classification_levels)), stringsAsFactors = FALSE)
names(counts) <- c("classification", "count")
write.csv(counts, "docs/rodeo_public_api_audit_counts.csv", row.names = FALSE)
