Test Coverage (SDK)
===================

Files
-----
- `test/otel_metrics_SUITE.erl` – Core metrics SDK behaviors (views, aggregations, temporality).
- `test/otel_exporter_metrics_otlp_SUITE.erl` – OTLP exporter integration.
- `test/simple_metric_producer.erl` – Example producer integration.

Spec areas exercised
--------------------
- View selection and default stream creation when no view matches.
- Aggregations: sum, last value, explicit histogram; drop behavior.
- Temporality: delta vs cumulative behavior per reader.
- Observable callbacks executed at collection time.
- Export pipeline to OTLP and console (serialization and send).

How to run
----------
- Using rebar3 (from umbrella root):
  - `rebar3 as test eunit apps=opentelemetry_experimental`

Gaps to consider
----------------
- End-to-end exemplar sampling scenarios may need expanded coverage.
- Multi-reader configurations with divergent temporality and aggregation mappings can be extended.


