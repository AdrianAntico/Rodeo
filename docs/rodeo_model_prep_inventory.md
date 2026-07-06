# Rodeo Model Prep Inventory

## Legacy Surface

Rodeo already contains legacy model-prep helpers in `R/FeatureEngineering_DataSets.R`:

- `AutoDataPartition()`
- `PartitionData()`
- `ModelDataPrep()`

These functions remain untouched. They are useful compatibility paths, but the vNext layer should not depend on their side effects or broaden their behavior.

## vNext Scope

The vNext model-prep layer adds scoring-safe partition contracts:

- `rodeo_partition_plan()`
- `rodeo_fit_partition_plan()`
- `rodeo_apply_partition_plan()`
- `rodeo_create_folds()`
- `generate_rodeo_model_prep_artifacts()`

Supported vNext partition modes:

- random train/test or train/validation/test
- stratified train/test or train/validation/test
- grouped partitions that keep groups together
- time partitions that keep earlier rows in earlier partitions
- random, stratified, and grouped k-fold assignments

## Contract

The fitted plan stores:

- original plan
- row-level partition assignments
- row-level fold assignments
- partition manifest
- fold manifest
- diagnostics
- warnings

The generated artifacts expose model-prep metadata without training a model or creating model-based features.

## Leakage Safety

The vNext contract keeps partition and fold metadata explicit. Grouped partitions do not split a group across partitions. Time partitions sort by the supplied date column and assign earlier rows first. Stratified partitions use the target only for assignment balance, not feature creation.

## Out Of Scope

- model training
- model-based features
- target encoding
- WOE / credibility encoding
- broad recipe frameworks
- replacement of legacy APIs
