# Rodeo Temporal Transformation Contract

Status: Phase 13 extended as an additive vNext contract.

Rodeo owns deterministic temporal feature preparation for forecasting. Model
packages should fit forecasting engines on prepared temporal data rather than
rebuilding lags, rolling statistics, calendar features, or future-known-variable
alignment themselves.

## Contract

The public temporal contract is:

- `rodeo_temporal_transformation_spec()`
- `rodeo_fit_temporal_transformation()`
- `rodeo_apply_temporal_transformation()`
- `rodeo_validate_temporal_schema()`
- `rodeo_prepare_forecast_supervised_data()`
- `rodeo_temporal_prediction_frame()`
- `rodeo_temporal_transformation_metadata()`
- `rodeo_temporal_normalize_frequency()`
- `rodeo_temporal_resolve_frequency()`
- `rodeo_temporal_coerce_index()`
- `rodeo_temporal_next_index()`
- `rodeo_temporal_frequency_is_intraday()`

The lifecycle is:

```text
raw temporal data
-> temporal transformation spec
-> fitted temporal transformation
-> prepared temporal history
-> supervised forecast frames
-> forecast engine
-> replay metadata
```

## Supported Features

The additive vNext contract supports:

- target lags
- seasonal target lags
- shifted rolling mean, variance, standard deviation, median, minimum, maximum,
  range, skewness, kurtosis, and quantile features
- strictly prior ordinary and seasonal difference features
- exponentially weighted means
- expanding mean, standard deviation, minimum, and maximum features
- Fourier terms for declared periodicities
- calendar/date features, including boundary indicators and intraday hour/minute
- known future variables
- forecast horizon framing
- direct supervised forecast frames
- recursive prediction rows
- entity-aware direct prediction frames
- entity-aware recursive prediction rows
- deterministic `entity_id_code` features
- static entity features exposed as `static_*`
- panel future row generation and horizon indexing
- schema validation
- serialization and deterministic replay
- normalized forecast-calendar frequency
- Date vs POSIXct precision preservation
- frequency-correct generated future calendars
- Cross-target preparation reuses the same frequency authority
- recursive panel prediction is one correct row per entity per horizon

## Frequency and Precision

Canonical frequencies are `1min`, `5min`, `10min`, `15min`, `30min`, `hour`,
`day`, `week`, `month`, `quarter`, and `year`. `auto` resolves from the fitted
index. Intraday frequencies keep `POSIXct` timestamps. Date-grain frequencies
use `Date`. Generated future rows follow the fitted frequency, including
calendar-month, quarter, and year increments. Explicit `future_data` remains
authoritative when supplied. Panel generation emits one correct horizon per
entity and does not reuse another entity's timestamps or static state.

Rodeo does not train forecasting engines. Panel/global model fitting remains a
consumer responsibility; Rodeo prepares the deterministic entity-aware temporal
frames that packages such as AutoQuant can use.

## Leakage Policy

Lag and rolling features are constructed from values available strictly before
the forecast origin row. Direct supervised labels may use future target values,
but feature columns must not include the target column, date column, entity id,
or raw known-future-variable columns. Known future variables are exposed through
`future_*` feature columns aligned to the forecasted date.

Difference, exponentially weighted, and expanding features follow the same
strictly-prior rule. Fourier terms depend only on the deterministic time index.
The feature manifest records the owner, family, temporal resolution, as-of
safety, leakage policy, and human-readable definition for every generated
column.

Rodeo materializes a declared feature specification; it does not choose an
optimal feature set. Feature selection and rolling-origin qualification belong
to the consuming analytical system. Implementations use vectorized cumulative
or rolling kernels where available and avoid quadratic expanding-window scans.

## AutoQuant Boundary

AutoQuant consumes this contract for CatBoost forecasting. AutoQuant remains
responsible for:

- forecast specification
- temporal validation
- partitioning
- engine fitting
- forecast artifacts
- forecast assessment
- rolling-origin evaluation

Rodeo remains responsible for deterministic temporal preparation and replay.

## QA

`qa_rodeo_package()` includes `qa_rodeo_temporal_transformation()` and
`qa_rodeo_temporal_calendar()`, covering:

- spec creation
- fit metadata
- lag leakage protection
- shifted rolling means
- expanded rolling statistics and quantiles
- seasonal lags, differencing, EWM, expanding, Fourier, and calendar features
- feature-manifest lineage and as-of invariants
- direct supervised frames
- future-known-variable alignment
- recursive prediction rows
- panel prediction frames
- static entity feature replay
- serialization replay
- schema validation
- metadata readability
- frequency-correct future calendars for minute through yearly grains
- panel and explicit-future calendar ownership
- Date/POSIXct precision and replay of generated calendars
