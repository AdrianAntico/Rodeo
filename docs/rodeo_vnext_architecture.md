# Rodeo vNext Architecture

Rodeo vNext is a clean fit/transform layer over the useful non-model feature engineering concepts already present in Rodeo. It does not replace legacy APIs. Legacy APIs remain performance and behavior baselines.

## Core Principle

Create a scoring-safe feature plan once, fit it on training data, and reuse the fitted plan on scoring data without leaking scoring information back into the spec.

## Public API

- `rodeo_feature_plan()`
- `rodeo_fit_feature_plan()`
- `rodeo_transform_feature_plan()`
- `rodeo_fit_transform_feature_plan()`
- `generate_rodeo_feature_engineering_artifacts()`

The structured transformation contract is a lower-level additive contract for individual deterministic transformations:

- `rodeo_transformation_spec()`
- `rodeo_fit_transformation()`
- `rodeo_apply_transformation()`
- `rodeo_save_transformation()`
- `rodeo_load_transformation()`
- `rodeo_transformation_metadata()`

See `docs/rodeo_transformation_contract.md`.

## Supported vNext Families

| Family | vNext status | Notes |
|---|---|---|
| Numeric | Implemented | `log1p`, `sqrt`, `standardize`, `winsorize`. Box-Cox/Yeo-Johnson remain legacy/wrap-later candidates. |
| Categorical | Implemented | Top-N one-hot encoding with rare and unseen levels. |
| Calendar | Implemented | Year, month, day, weekday, week, quarter, weekend flag. Holiday wrappers are benchmark-first. |
| Text | Implemented | Lightweight counts and ratios only. No embeddings or model-based text features. |
| Missingness | Implemented | Binary missingness indicators. |
| Interactions | Implemented | Numeric x numeric, categorical x numeric, categorical x categorical with caps. |
| Cross-row | Deferred | Existing lag/diff/rolling functions are benchmark baselines; vNext wrappers need explicit sort/group contracts. |
| Model prep | Deferred | Needs separate design for partitioning and model-ready recipes. |
| Model-Based Features | Deferred | H2O, Word2Vec, clustering, and anomaly features need separate modern leakage-safe design. |

## Fitted Plan Contract

A fitted plan stores:

- Original plan.
- Numeric parameters, including means, standard deviations, and clipping bounds.
- Categorical levels, rare-level mapping, and unseen-level mapping.
- Calendar/text/missingness column settings.
- Interaction definitions and feature caps.
- Feature manifest.
- Diagnostics.
- Warnings.
- Fit timestamp.

## Artifact Generator Contract

`generate_rodeo_feature_engineering_artifacts()` returns:

- `artifacts`: overview text, config table, feature manifest, diagnostics, engineered data summary, optional benchmark summary.
- `metadata`: generator and timestamp.
- `warnings`: non-fatal warnings.
- `diagnostics`: structured checks.
- `value`: engineered data, fitted plan, manifest, diagnostics, and warnings.

This is intentionally app-agnostic. Analytics/reporting apps can adapt these objects into their own artifact systems later.

## Benchmark Alignment

Full benchmark suites live in the Benchmarks repo. Rodeo vNext should not hard-code implementation thresholds until benchmark evidence supports them. Benchmark candidates include legacy Rodeo, vNext, direct `data.table::set()`, `:=`, `collapse`, and base vectorized approaches.

## Current Optimization Notes

The first optimization pass keeps the vNext API contract unchanged and improves internals in conservative places:

- Feature manifests are accumulated as lightweight row lists and materialized once, instead of repeatedly binding one-row data.tables.
- Transform output columns are precomputed by feature family and assigned in batches to reduce repeated column-growth overhead on wide data.
- `data.table::setalloccol()` is used inside the batch-assignment helper to reduce reallocation pressure.
- Numeric `log1p` and `sqrt` retain safety behavior for invalid values, but use a direct fast path when a column is already valid.
- Categorical fitted specs store generated dummy column names so scoring does not recompute them.
- Calendar and text feature flags are computed once per transform call rather than inside every feature branch.
- Interaction fitted specs store generated feature names, preserving capped scoring-safe behavior while avoiding name reconstruction.

Do not interpret this as "`data.table::set()` everywhere." The current benchmark direction is mixed:

- `set()` is often best for narrow direct assignment and some wide numeric/calendar cases.
- Batch assignment is competitive for combined plans and some categorical shapes because it reduces repeated column growth.
- `:=`, grouped `:=`, collapse, and base vectorized paths remain benchmark candidates.
- Large/wide cases should prioritize fewer reallocations, precomputed output vectors, and minimized repeated name scans.

Thresholds are still provisional. They should be documented in the Benchmarks repo until repeated moderate/overnight evidence supports moving adaptive choices into Rodeo itself.
