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

The Phase 13 contract supports:

- target lags
- shifted rolling target means
- calendar/date features
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

Rodeo does not train forecasting engines. Panel/global model fitting remains a
consumer responsibility; Rodeo prepares the deterministic entity-aware temporal
frames that packages such as AutoQuant can use.

## Leakage Policy

Lag and rolling features are constructed from values available strictly before
the forecast origin row. Direct supervised labels may use future target values,
but feature columns must not include the target column, date column, entity id,
or raw known-future-variable columns. Known future variables are exposed through
`future_*` feature columns aligned to the forecasted date.

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

`qa_rodeo_package()` includes `qa_rodeo_temporal_transformation()`, covering:

- spec creation
- fit metadata
- lag leakage protection
- shifted rolling means
- direct supervised frames
- future-known-variable alignment
- recursive prediction rows
- panel prediction frames
- static entity feature replay
- serialization replay
- schema validation
- metadata readability
