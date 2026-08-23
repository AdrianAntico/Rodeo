# Rodeo Expert API Reconstruction 2.0

## Decision

The target is minimum memorization without capability loss. Distinct expert
workflows keep distinct verbs. Duplicates, leakage-unsafe paths, QA helpers,
and package utilities are unexported. Preferred fitted verbs wrap real
implementations and return reusable state plus `predict()`.

| Metric | Value |
|---|---|
| TOTAL_EXPORTS_BEFORE | 118 |
| AFTER | 74 |
| CORE | 7 |
| SPECIALIZED | 67 |
| COMPATIBILITY | 0 |
| UNEXPORTED_INTERNALS | 48 |
| CAPABILITY LOST? | no |

S3 `predict()` methods for the seven fitted classes are registered in NAMESPACE
and are not counted in the 74 `export()` symbols.

NAMESPACE is hand-curated. DESCRIPTION sets
`Roxygen: list(roclets = c("collate", "rd"))` so roxygen2 cannot regenerate
exports.

## CORE_EXPERT

Memorable fitted workflows. Each returns reusable state and supports `predict()`.

| Export | Workflow |
|---|---|
| `rodeo_features` | Static tabular generation, one-hot, interactions, leakage-safe target/WOE |
| `rodeo_encode` | Credibility, target, WOE, polynomial, backward-difference, Helmert maps |
| `rodeo_transform` | Auto/invertible transforms, grouped standardize, percentile ranks |
| `rodeo_temporal_features` | Forecast-safe lags, rolling, differences, Fourier/calendar |
| `rodeo_cross_row_features` | Grouped lag/roll/mode/diff outside forecast framing |
| `rodeo_complete_time` | Time-grid completion and rolling fill |
| `rodeo_model_prep` | Random, stratified, grouped, chronological partitions and folds |

`rodeo_model_features` was not added. Word2Vec, autoencoder, isolation forest,
and clustering remain specialized fit/score pairs so method-specific controls
are not collapsed.

`rodeo_recipe` was not added. Persistable imputation, constant/NZV removal,
factor-level, and date-feature state already live on
`rodeo_transformation_spec()`.

## SPECIALIZED_EXPERT

Distinct expert methods kept public. Preferred verbs wrap these; they are not
deleted.

**Encoding.** `CategoricalEncoding`

**Calendar.** `CreateCalendarVariables`, `CreateHolidayVariables`

**Cross-row / temporal engines.** `AutoLagRollStats`, `AutoLagRollStatsScoring`,
`AutoLagRollMode`, `AutoDiffLagN`, `TimeSeriesFill`, `TimeSeriesFillRoll`,
`DT_GDL_Feature_Engineering`, `Partial_DT_GDL_Feature_Engineering`

**Numeric transforms.** `AutoTransformationCreate`, `AutoTransformationScore`,
`Standardize`, `StandardizeScoring`, `PercRank`, `PercRankScoring`,
`Apply_Asin`, `Apply_Asinh`, `Apply_BoxCox`, `Apply_Log`, `Apply_LogPlus1`,
`Apply_Logit`, `Apply_Sqrt`, `Apply_YeoJohnson`, `InvApply_Asin`,
`InvApply_Asinh`, `InvApply_BoxCox`, `InvApply_Log`, `InvApply_LogPlus1`,
`InvApply_Logit`, `InvApply_Sqrt`, `InvApply_YeoJohnson`,
`Estimate_BoxCox_Lambda`, `Estimate_YeoJohnson_Lambda`

**Interactions.** `AutoInteraction` (canonical name)

**Model-based features.** `AutoWord2VecModeler`, `AutoWord2VecScoring`,
`H2OAutoencoder`, `H2OAutoencoderScoring`, `H2OIsolationForest`,
`H2OIsolationForestScoring`, `AutoClustering`, `AutoClusteringScoring`

**vNext plans, artifacts, lineage.** `rodeo_feature_plan`,
`rodeo_fit_feature_plan`, `rodeo_transform_feature_plan`,
`rodeo_fit_transform_feature_plan`, `rodeo_partition_plan`,
`rodeo_fit_partition_plan`, `rodeo_apply_partition_plan`, `rodeo_create_folds`,
`rodeo_transformation_spec`, `rodeo_fit_transformation`,
`rodeo_apply_transformation`, `rodeo_fit_apply_transformation`,
`rodeo_save_transformation`, `rodeo_load_transformation`,
`rodeo_transformation_metadata`, `rodeo_temporal_transformation_spec`,
`rodeo_fit_temporal_transformation`, `rodeo_apply_temporal_transformation`,
`rodeo_temporal_transformation_metadata`,
`rodeo_prepare_forecast_supervised_data`,
`rodeo_prepare_cross_target_features`,
`generate_rodeo_feature_engineering_artifacts`,
`generate_rodeo_model_prep_artifacts`

## COMPATIBILITY

None. Partitioning is covered by `rodeo_partition_plan` / `rodeo_model_prep`.
Interaction ArgsList wrappers and low-level difference helpers were unexported
instead of kept as aliases.

## UNEXPORTED

Leakage-unsafe or duplicate encoding: `MEOW`, `DummifyDT`, `DummyVariables`,
`Encoding`, `EncodeCharacterVariables`.

Duplicate calendar/time wrappers: `CalendarVariables`, `HolidayVariables`,
`TimeSeriesFeatures`.

Duplicate H2O ArgsList wrappers: `AutoEncoder_H2O`, `Clustering_H2O`,
`IsolationForest_H2O`, `Word2Vec_H2O`.

Package utilities: `FakeDataGenerator`, `BenchmarkData`, `BuildBinary`,
`Install`, `UpdateDocs`, `Mode`, `LB`.

QA: `qa_rodeo_package`, `qa_rodeo_preferred_api`,
`qa_rodeo_vnext_supervised_encoding`. `qa_rodeo_preferred_api` remains callable
via `Rodeo:::qa_rodeo_preferred_api()` and asserts leakage-unsafe names are not
exported.

Temporal/schema internals: `rodeo_temporal_coerce_index`,
`rodeo_temporal_frequency_is_intraday`, `rodeo_temporal_next_index`,
`rodeo_temporal_normalize_frequency`, `rodeo_temporal_prediction_frame`,
`rodeo_temporal_resolve_frequency`, `rodeo_validate_temporal_schema`,
`rodeo_validate_transformation_schema`, `weekdays_in_month`.

Transform validators: `Test_Asin`, `Test_Asinh`, `Test_BoxCox`,
`Test_Identity`, `Test_Log`, `Test_LogPlus1`, `Test_Logit`, `Test_Sqrt`,
`Test_YeoJohnson`.

Covered or wrapper-only: `AutoDataPartition`, `PartitionData`, `ModelDataPrep`,
`CreateInteractions`, `Interact`, `DiffDT`, `DiffLagN`,
`Partial_DT_GDL_Feature_Engineering2`.

## Why capability is not lost

- Supervised target/WOE remains on `rodeo_features()` with out-of-fold training
  values.
- Credibility and contrast encodings remain on `CategoricalEncoding()` and the
  new `rodeo_encode()` fitted wrapper.
- Invertible transforms, grouped scale, and percentile ranks remain as
  specialized verbs and as `rodeo_transform()`.
- Forecast temporal features remain on `rodeo_temporal_features()`.
- General grouped lag/roll/mode/diff remain as specialized engines and as
  `rodeo_cross_row_features()`.
- Time-grid completion remains as `TimeSeriesFill` / `TimeSeriesFillRoll` and
  as `rodeo_complete_time()`.
- Model-based features keep separate fit/score pairs.
- Persistable recipes remain on the transformation-spec family.
- Scoring counterparts (`*Scoring`, `Partial_DT_GDL_Feature_Engineering`,
  `InvApply_*`) stay public where they are required for OOS apply.
