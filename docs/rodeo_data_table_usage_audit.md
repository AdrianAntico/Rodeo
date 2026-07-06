# Rodeo data.table Usage Audit

Rodeo is intentionally data.table-heavy. The legacy code often optimizes for low-copy mutation and local performance, which is appropriate for large feature engineering workloads but can surprise users when inputs are changed by reference.

## Observed Patterns

| Pattern | Where used | Current value | Risk / note | vNext direction |
|---|---|---|---|---|
| `data.table::set()` | Numeric/categorical/cross-row style feature creation | Very fast repeated column assignment. | Harder to read; mutates in place. | Use internally where speed matters, but default public transform returns a copied table. |
| `:=` | Across feature engineering files | Idiomatic grouped and direct mutation. | By-reference side effects. | Keep legacy behavior; vNext should make copy behavior explicit. |
| `setnames()` | Encoding/model prep/cross-row helpers | Fast renaming. | Can be risky when called on user-owned tables. | Use in internals after copying or document mutation. |
| `setcolorder()` | Model prep / output organization | Efficient output ordering. | Output order assumptions can be brittle. | Track generated columns in manifest instead of relying only on physical order. |
| `setorderv()` / ordering | Cross-row and rolling functions | Needed for time-order features. | Sorting by reference changes caller order. | Cross-row vNext should store order requirements and copy by default. |
| `shift()` | Lag/diff/rolling families | Correct primitive for grouped row-offset features. | Requires sorted groups. | Benchmark and wrap later with explicit sort keys. |
| `by =` groups | Cross-row, calendar, encoding, summary helpers | Fast grouped feature generation. | Group cardinality can explode runtime. | Add cardinality diagnostics and caps in vNext wrappers. |
| `.SD` | Grouped feature subsets | Flexible grouped column operations. | Can allocate more than expected on wide data. | Benchmark `.SD` vs `set()` loops for wide workloads. |
| `copy()` | Some safer paths | Prevents accidental mutation. | Extra memory on large data. | Public vNext APIs expose `copy_data`. |
| `alloc.col()` | Legacy optimization pattern where present | Avoids repeated reallocations. | Less obvious to maintainers. | Use only in benchmark-proven hot paths. |
| `rbindlist()` | QA, data generation, summaries | Fast row binding. | Need consistent column types. | Keep. |
| `collapse` | Imported for optimized grouped operations | Useful for aggregation-heavy paths. | Adds API surface and benchmark justification needed. | Benchmark against data.table for vNext thresholds. |
| Base loops over columns | Many legacy functions | Often acceptable and explicit. | Can become slow with nested level/feature loops. | Keep where simple; benchmark nested feature creation. |
| Nested loops over rows/levels/features | Categorical interactions, cross-row/model prep | Flexible but can be expensive. | Cardinality explosions and memory spikes. | vNext adds feature caps and diagnostics. |

## Likely Fast Areas

- Numeric vector transforms.
- `set()`/`:=` direct column creation.
- data.table `shift()` for bounded lag/diff features.
- `rbindlist()` summary construction.

## Likely Slow or Risky Areas

- Wide one-hot encoding with many levels.
- Full-factorial categorical interactions.
- Cross-row rolling features without strict sort/group contracts.
- Repeated by-reference changes to caller-owned objects.
- Model-based feature functions, which require a separate leakage-safe redesign.

## vNext Guidance

The vNext layer should keep data.table as the core engine, use scoring-safe fitted specs, make copying behavior explicit, and produce a feature manifest so downstream systems can reason about generated columns without reverse engineering names.
