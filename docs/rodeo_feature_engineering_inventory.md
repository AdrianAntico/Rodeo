# Rodeo Feature Engineering Inventory

Rodeo is a data.table-oriented R feature engineering package. The existing package philosophy is to expose optimized, task-specific functions for numeric transforms, categorical encodings, calendar/date features, cross-row operations, interactions, model prep, and model-based feature generation. Legacy functions remain performance and behavior baselines.

Model-Based Features are explicitly deferred from the vNext optimization pass.

| Function family | Exported functions | Primary files | Purpose | Train/scoring safety | Mutation behavior | Internal approaches | vNext disposition |
|---|---|---|---|---|---|---|---|
| Numeric transforms | `Apply_Asin`, `Apply_Asinh`, `Apply_BoxCox`, `Apply_Log`, `Apply_LogPlus1`, `Apply_Logit`, `Apply_Sqrt`, `Apply_YeoJohnson`, inverse `InvApply_*`, tests `Test_*`, `Estimate_BoxCox_Lambda`, `Estimate_YeoJohnson_Lambda`, `Standardize`, `StandardizeScoring`, `PercRank`, `PercRankScoring`, `AutoTransformationCreate`, `AutoTransformationScore` | `R/FeatureEngineering_NumericTypes.R` | Transform numeric columns, estimate transform parameters, reuse scoring parameters. | Mixed. Some functions have explicit scoring variants, some mutate a supplied table directly. | Often by reference. | data.table, base vector math, loops, some distribution tests. | Keep legacy; wrap concepts in vNext plan/spec; benchmark transform implementations. |
| Categorical/text-like columns | `DummifyDT`, `DummyVariables`, `CategoricalEncoding`, `EncodeCharacterVariables`, `Encoding` | `R/FeatureEngineering_CharacterTypes.R` | Dummy variables, target/frequency-style encoding, factor-level reuse. | Mixed. `DummifyDT` supports imported factor levels; vNext standardizes rare/unseen handling. | Often by reference. | data.table column creation, loops over columns and levels. | Keep legacy; vNext provides one-hot/top-N/rare/unseen spec. |
| Calendar/date | `LB`, `CreateCalendarVariables`, `CreateHolidayVariables`, `weekdays_in_month`, `CalendarVariables`, `HolidayVariables`, `TimeSeriesFeatures`, `TimeSeriesFill`, `TimeSeriesFillRoll` | `R/FeatureEngineering_CalendarTypes.R`, `R/FeatureEngineering_CrossRowOperations.R` | Calendar units, holiday features, time-series filling and lookbacks. | Calendar units are generally scoring-safe; holiday/window features need saved settings. | Often by reference. | data.table, timeDate, lubridate, loops. | Keep legacy; vNext includes simple calendar units now; holiday/cross-row wrappers deferred/benchmark-first. |
| Interactions | `AutoInteraction`, `Interact`, `CreateInteractions`, `MEOW` | `R/FeatureEngineering_CrossRowOperations.R`, `R/MiscFunctions.R` | Numeric, categorical, and generated interaction features. | Mixed; cardinality and feature-count control is important. | Often by reference. | data.table, nested loops, combinat. | Keep legacy; vNext adds capped numeric x numeric, categorical x numeric, categorical x categorical specs. |
| Cross-row operations | `AutoDiffLagN`, `DiffDT`, `DiffLagN`, `AutoLagRollMode`, `AutoLagRollStats`, `AutoLagRollStatsScoring` | `R/FeatureEngineering_CrossRowOperations.R` | Lag, diff, rolling stats, grouped time-order features. | Some explicit scoring support exists. | Often by reference. | data.table shift, groups, order, loops. | Benchmark first; vNext architecture placeholder only in this pass. |
| Model prep / partitioning | `AutoDataPartition`, `PartitionData`, `ModelDataPrep`, `DT_GDL_Feature_Engineering`, `Partial_DT_GDL_Feature_Engineering`, `Partial_DT_GDL_Feature_Engineering2` | `R/FeatureEngineering_DataSets.R` | Data splitting, model-ready preparation, generalized feature engineering pipelines. | Mixed; reusable args/specs exist in older patterns. | Mixed. | data.table, loops, helper lists. | Wrap only after benchmark/API review; do not merge into first vNext feature plan. |
| Model-Based Features | `AutoClustering`, `AutoClusteringScoring`, `AutoEncoder_H2O`, `AutoWord2VecModeler`, `AutoWord2VecScoring`, `Clustering_H2O`, `H2OAutoencoder`, `H2OAutoencoderScoring`, `H2OIsolationForest`, `H2OIsolationForestScoring`, `IsolationForest_H2O`, `Word2Vec_H2O` | `R/FeatureEngineering_ModelBased.R` | H2O clustering, autoencoder, isolation forest, Word2Vec/model-derived features. | Requires separate leakage-safe redesign. | Mixed. | H2O/model wrappers. | Deferred. Do not modernize in this phase. |
| Data generation / utilities | `FakeDataGenerator`, `BenchmarkData`, `BuildBinary`, `Install`, `UpdateDocs`, `Mode` | `R/FakeDataGenerator.R`, `R/FeatureEngineering_DataSets.R`, `R/MiscFunctions.R` | Fixtures, benchmark data, package utilities, mode helper. | N/A. | Mixed. | data.table/base. | Keep legacy; use for tests/benchmarks as needed. |
| vNext plan/spec layer | `rodeo_feature_plan`, `rodeo_fit_feature_plan`, `rodeo_transform_feature_plan`, `rodeo_fit_transform_feature_plan`, `generate_rodeo_feature_engineering_artifacts`, `qa_rodeo_vnext*` | `R/FeatureEngineering_vNext.R` | Clean fit/transform front door for scoring-safe non-model feature engineering. | Yes. Fitted plans store parameters, levels, manifest, warnings, diagnostics. | Defaults to copied output; internal creation uses data.table set. | data.table, base vector operations. | New vNext entry points. |

## Baseline Candidates

Legacy benchmark baselines should include:

- Numeric transform functions and explicit scoring variants.
- `DummifyDT`, `DummyVariables`, and `CategoricalEncoding`.
- `CreateCalendarVariables` and holiday helpers.
- `AutoInteraction`, `Interact`, and `CreateInteractions`.
- Cross-row lag/diff/rolling functions where bounded test data is available.

## vNext Scope

The first vNext layer covers numeric, categorical, calendar, text, missingness, and interactions. Cross-row, model prep, and model-based feature families remain benchmark-first or deferred.
