Glossary (API)
==============

- Instrument: A Counter, UpDownCounter, Histogram, or Observable variant used to capture measurements.
- Measurement: A numeric value plus optional attributes recorded against an instrument.
- Attributes: Key/value pairs attached to a measurement for dimensionality.
- Meter: Factory for instruments within an instrumentation scope; resolved implicitly by macros.
- Instrumentation scope: Name/version/schema of the library emitting telemetry.
- View: SDK rule that selects instruments, filters attributes, and chooses an aggregation.
- Aggregation: Sum, Last Value, Explicit Bucket Histogram, or Drop.
- Temporality: Delta (per-interval) or Cumulative (running total), selected by the Reader.
- Reader: SDK component that collects and exports metrics.
- Exporter: Component that serializes and sends metrics (Console, OTLP).


