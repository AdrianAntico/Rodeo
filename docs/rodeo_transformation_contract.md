# Rodeo Structured Transformation Contract

## Purpose

The structured transformation contract defines the minimum shape every future deterministic Rodeo transformation should follow before it is used by AnalyticsShinyApp or any other downstream system.

The contract is intentionally narrow. It is not a workflow engine, feature store, AutoML layer, or arbitrary expression evaluator. It exists so feature preparation can be deterministic, serializable, replayable, and safe across train/scoring boundaries.

## Public API

- `rodeo_transformation_spec()`
- `rodeo_fit_transformation()`
- `rodeo_apply_transformation()`
- `rodeo_fit_apply_transformation()`
- `rodeo_validate_transformation_schema()`
- `rodeo_save_transformation()`
- `rodeo_load_transformation()`
- `rodeo_transformation_metadata()`
- `qa_rodeo_transformation_contract()`

## Specification Shape

A transformation specification is a serializable R list with class `rodeo_transformation_spec`.

It contains:

- `id`
- `type`
- `input_columns`
- `output_columns`
- `parameters`
- `learned_state`
- `schema_metadata`
- `version`
- `warnings`
- `diagnostics`
- `metadata`
- `created_at`

The unfitted specification stores intent and parameters only. It must not store executable code.

## Fit / Apply Lifecycle

Fit consumes training data and an unfitted specification.

Fit produces:

- learned state
- input schema
- output-column metadata
- columns added
- columns removed
- deterministic diagnostics
- warnings
- a fitted specification with class `rodeo_fitted_transformation`

Apply consumes a fitted specification and a new dataset.

Apply must:

- validate required input columns;
- validate duplicated output names;
- warn when column types differ from the fit schema;
- use only learned state from Fit;
- never recompute learned state;
- return transformed data.

## Initial Transformation Set

The first contract implementation supports a deliberately small deterministic set:

| Transformation | Type | Fit state | Apply behavior |
|---|---|---|---|
| Missing value imputation | `missing_impute` | replacement value per input column | fills missing values using fitted replacements |
| Constant-column removal | `constant_remove` | columns found constant in training | removes those columns during apply |
| Near-zero variance removal | `near_zero_variance_remove` | numeric columns above fitted threshold | removes learned columns during apply |
| Factor level management | `factor_levels` | fitted levels and unseen level | maps unseen values to fitted unseen level |
| Date feature extraction | `date_features` | features and generated output names | creates deterministic date-derived columns |

This set is not intended to be the full Rodeo feature engineering catalog. It is the contract seed.

## Serialization

`rodeo_save_transformation()` writes a fitted specification with `saveRDS(..., version = 3)`.

`rodeo_load_transformation()` reads the object and verifies that it inherits `rodeo_fitted_transformation`.

A loaded fitted transformation should replay identically against the same input data.

## Diagnostics

Diagnostics are deterministic tables with:

- `check`
- `status`
- `detail`

Transformations should report:

- learned replacements;
- skipped columns;
- removed columns;
- generated columns;
- incompatible columns;
- schema warnings.

Warnings are non-fatal when the fitted contract can still be applied safely.

## Schema Expectations

Before apply, Rodeo validates:

- required input columns exist;
- generated output names are not duplicated;
- generated output names do not already exist in the apply dataset;
- fitted input classes versus apply-time input classes.

Missing required columns and invalid output names are hard errors. Type changes are warnings for now because some deterministic transformations, such as missing imputation or factor management, can still safely apply after coercion or with unchanged learned state.

## Extension Guidelines

Future transformations should:

- expose parameters through `parameters`;
- learn all train-only values in Fit;
- store learned values under `learned_state`;
- store generated/removal effects under metadata;
- validate schemas before Apply;
- return deterministic diagnostics;
- avoid by-reference mutation unless `copy_data = FALSE`;
- avoid executable code in specs;
- keep target-aware transformations out until leakage-safe contracts exist.

Examples of future candidates:

- fitted numeric scaling;
- fitted clipping;
- fitted categorical rare-level handling;
- fitted frequency/count encoding;
- cyclic date encoding;
- ratio features;
- controlled interaction features;
- group and rolling features with explicit group/sort contracts.

## AnalyticsShinyApp Compatibility

AnalyticsShinyApp should eventually call this layer through a narrow adapter:

```text
app config
-> Rodeo transformation spec
-> Rodeo fit
-> fitted transformation
-> Rodeo apply
-> prepared data + metadata
-> app artifact system
```

The app should own UI, artifacts, lineage, project state, and governance. Rodeo should own deterministic transformation execution and fitted transformation state.

## Current Limitations

- The contract currently applies one transformation at a time.
- It does not implement a pipeline engine.
- It does not include target-aware transformations.
- It does not include cross-row lag/rolling features.
- It does not replace `rodeo_feature_plan()`.
- It does not integrate with AnalyticsShinyApp yet.

