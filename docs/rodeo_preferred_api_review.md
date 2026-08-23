# Rodeo Preferred API Review

## Decision

The previous `114 -> 4` summary was not a valid statement of complete expert
coverage. The current namespace has 118 exports: the 114-symbol legacy baseline
plus the preferred API checkpoint. The exhaustive classification is in
`rodeo_public_api_audit.csv`.

The existing preferred calls cover three coherent workflows:

1. `rodeo_features()` for modern static tabular plans, supervised target/WOE
   encoding, interactions, lineage, and scoring-safe replay.
2. `rodeo_temporal_features()` for leakage-safe temporal plans, including lags,
   rolling summaries, differences, Fourier terms, known-future variables, and
   entity-aware replay.
3. `rodeo_model_prep()` for random, stratified, grouped, and chronological
   partitions and folds.

`predict()` applies each fitted object. It is a standard generic, not a fourth
Rodeo workflow.

These calls do not yet cover all materially distinct Rodeo workflows. The
package therefore remains in API reconstruction; compatibility exports cannot
be hidden or deprecated until the missing preferred entry points exist.

## Revised Target Surface

The smallest currently defensible target is nine memorable Rodeo workflow
entry points plus the standard `predict()` generic:

| Preferred entry point | Workflow | Status |
|---|---|---|
| `rodeo_features()` | Static tabular generation, interactions, simple categorical encoding, lineage | Implemented |
| `rodeo_encode()` | Full categorical, target, WOE, credibility, and contrast encoding | Missing |
| `rodeo_transform()` | Invertible transforms, grouped scaling, percentile ranks, fitted parameters | Missing |
| `rodeo_temporal_features()` | Forecast-safe lags, rolling, differences, Fourier/calendar, known-future and grouped features | Implemented |
| `rodeo_cross_row_features()` | General grouped lag/diff/rolling/mode operations outside forecast framing | Missing |
| `rodeo_complete_time()` | Explicit time-grid completion and rolling fill behavior | Missing |
| `rodeo_model_features()` | Word2Vec, autoencoder, isolation forest, and clustering fitted features | Missing |
| `rodeo_recipe()` | Persistable imputation, constant/NZV removal, factor-level and date transformation state | Missing |
| `rodeo_model_prep()` | Partitioning, folds, and model-ready split manifests | Implemented |

This is a target contract, not a claim that wrappers already exist. New facades
must return reusable fitted state, support `predict()`, retain diagnostics and
lineage, and directly expose consequential controls. Thin renames over legacy
functions are not sufficient.

## Workflow Reachability

| Workflow | Reachable from current preferred calls? | Important controls exposed? | Expert override? | Reusable state? | Old export required? | Hidden by 4-call claim? |
|---|---|---|---|---|---|---|
| One-hot categorical handling | Yes | Yes | Yes | Yes | No | No |
| Cross-fitted target encoding | Yes | Yes | Yes | Yes | No | No |
| Cross-fitted WOE | Yes | Yes | Yes | Yes | No | No |
| Credibility encoding | No | Only through `CategoricalEncoding()` | Legacy only | Partial/list-or-file | Yes | Yes |
| Polynomial/backward-difference/Helmert contrasts | No | Legacy only | Legacy only | Partial/list-or-file | Yes | Yes |
| Basic numeric transforms | Yes | Limited set | Yes | Yes | No | No |
| Box-Cox/Yeo-Johnson/asin/asinh/logit and inverse transforms | No | Legacy only | Legacy only | Create/score metadata | Yes | Yes |
| Grouped standardization | No | Legacy only | Yes | Score table | Yes | Yes |
| Grouped percentile ranking | No | Legacy only | Yes | Score table | Yes | Yes |
| Missingness indicators | Yes | Yes | Yes | Yes | No | No |
| Missing-value imputation | No | Structured spec API only | Yes | Yes, persisted | Yes | Yes |
| Constant/NZV removal | No | Structured spec API only | Yes | Yes, persisted | Yes | Yes |
| Factor-level stabilization | Partly | Preferred categorical plan covers common path | Yes | Yes | Sometimes | Partly |
| Numeric/categorical interactions | Yes | Yes | Yes | Yes | No | No |
| Temporal lags and rolling statistics | Yes | Yes | Yes | Yes | No | No |
| Rolling mode/general grouped cross-row features | No | Legacy only | Yes | Mixed scoring pair | Yes | Yes |
| Differencing/seasonal differencing | Yes | Yes | Yes | Yes | No | No |
| Fourier/calendar features | Yes | Yes | Yes | Yes | No | No |
| Holiday features | No | Legacy only | Partial | No unified fitted state | Yes | Yes |
| Known-future variables | Yes | Yes | Yes | Yes | No | No |
| Entity/group temporal features | Yes | Yes | Yes | Yes | No | No |
| Time-grid completion/fill | No | Legacy only | Yes | No unified fitted state | Yes | Yes |
| Random/stratified/group/time partitioning | Yes | Yes | Yes | Yes | No | No |
| Fold assignment | Yes | Yes | Yes | Yes | No | No |
| Generic persisted recipes | No | Low-level spec API only | Yes | Yes | Yes | Yes |
| Word2Vec features | No | Legacy modeler/scorer | Yes | Yes | Yes | Yes |
| Autoencoder features | No | Legacy fit/scoring pair | Yes | Yes | Yes | Yes |
| Isolation-forest anomaly features | No | Legacy fit/scoring pair | Yes | Yes | Yes | Yes |
| Clustering features | No | Legacy fit/scoring pair | Yes | Yes | Yes | Yes |
| Diagnostics and lineage | Yes for vNext paths | Yes | N/A | Yes | Legacy paths vary | Partly |
| Feature selection | Partial | Constant/NZV only through spec API | Yes | Yes | Yes | Yes |
| Inverse transformation | No | Legacy family-specific functions | Yes | Parameter-dependent | Yes | Yes |

## API Rules For Closure

- Do not remove specialist exports until their advanced controls are represented
  by a preferred fitted workflow.
- Do not merge model-based methods into a lowest-common-denominator contract.
  `method` may select the engine, but method-specific controls must remain
  directly available or explicitly accepted through an inspectable control
  object.
- Every new fit entry point must support out-of-sample `predict()` without
  re-estimating training state.
- Persisted state must be ordinary serializable R objects. File-based CSV side
  channels in legacy APIs are compatibility behavior, not the target contract.
- Diagnostics and feature lineage are required for every preferred workflow.
- Inverse transforms must be available when the fitted transformation is
  mathematically invertible.

## Qualification State

`NEEDS_MORE_WORK`. The audit settles the scope and corrects the public narrative,
but six preferred workflow facades remain unimplemented. The package must not be
reported as a complete four-call expert API.
