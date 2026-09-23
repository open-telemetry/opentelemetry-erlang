Glossary (SDK)
==============

- MeterProvider: SDK service that creates meters, manages views and readers.
- Instrument: Counter, UpDownCounter, Histogram, ObservableCounter, ObservableUpDownCounter, ObservableGauge.
- Stream: A per-Reader aggregation pipeline for an instrument (and view), with its own temporality and aggregation.
- View: Selection and processing configuration for instruments (criteria, attribute keys, aggregation).
- Aggregation: Implementation module that maintains/collects metric data (sum, last value, histogram, drop).
- Temporality: Strategy for reporting (delta or cumulative). Streams may reset (forget) after collection.
- Reader: Collector that triggers callbacks, iterates streams, and exports metrics.
- Exporter: Sends serialized metrics to a backend (console or OTLP).
- Exemplars: Sampled measurements attached to metrics for rich diagnostics.
- ETS: Erlang Term Storage used for in-memory tables (instruments, streams, metrics, exemplars).


