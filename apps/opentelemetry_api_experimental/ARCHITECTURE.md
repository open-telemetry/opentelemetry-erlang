opentelemetry_api_experimental – Architecture (Metrics API)
==========================================================

Purpose: Describe the instrumentation surface and how it delegates to the SDK so non-Erlang reviewers can reason about behavior.

API surface
-----------
- Erlang macros in `include/otel_meter.hrl` provide a minimal, ergonomic API:
  - Instrument creation: `?create_counter`, `?create_updown_counter`, `?create_histogram`, and observable counterparts.
  - Recording: `?counter_add`, `?updown_counter_add`, `?histogram_record`.
  - Observables: `?register_callback` for one or many instruments.
- Macros resolve the “current meter” automatically from the module’s application scope and call underlying modules (`otel_meter`, `otel_counter`, `otel_histogram`, etc.).

Delegation to SDK
-----------------
- Instrument creation and recording calls are forwarded to the SDK’s MeterProvider (`otel_meter_server`).
- The SDK applies views, aggregations, temporality, and export; the API does not implement these rules.

Elixir wrapper modules
----------------------
- `lib/open_telemetry/*.ex` expose equivalent APIs for Elixir users, delegating to the same underlying functions.

Conceptual flow
---------------
1) Application code includes `otel_meter.hrl` and uses macros to create instruments and record measurements.
2) Macros capture the current context and meter, and call the appropriate API module.
3) The API module calls into the SDK MeterProvider, which routes to streams and aggregations.

Interoperability notes
----------------------
- Attributes are passed through as maps; filtering happens in the SDK via Views.
- Units and descriptions are provided at creation and propagated to the SDK.
- Callback lifecycles for observables are controlled by the SDK’s Readers during collection.

See also
--------
- `SPEC_COMPLIANCE.md` for spec crosswalk and code anchors.
- Package README for quickstart and macro cheat sheet.


