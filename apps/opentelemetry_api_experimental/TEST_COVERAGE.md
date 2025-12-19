Test Coverage (API)
===================

Files
-----
- `test/otel_metrics_test.exs` – Elixir tests covering API instrumentation semantics.

Spec areas exercised
--------------------
- Instrument creation via macros.
- Recording of counters, histograms, and updown counters with attributes.
- Observable callback behavior when paired with SDK readers.

How to run
----------
- Using Mix (from umbrella root with Elixir available):
  - `mix test apps/opentelemetry_api_experimental`

Gaps to consider
----------------
- Negative value propagation relies on SDK enforcement; see SDK tests for monotonic handling.
- Per-reader temporality behavior belongs to SDK tests.


