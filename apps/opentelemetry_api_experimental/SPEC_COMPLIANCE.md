opentelemetry_api_experimental – Spec Compliance Crosswalk
==========================================================

Audience: Reviewers validating OpenTelemetry Metrics API semantics without Erlang knowledge.

Scope: Instrument types, recording semantics, observables, attributes, scope propagation, and how these map to SDK behavior.

How to read code anchors: Blocks below cite file, start:end lines. They are illustrative; exact line numbers may drift slightly across commits.

1) Instrument kinds supported
-----------------------------
- Counter, UpDownCounter, Histogram
- ObservableCounter, ObservableUpDownCounter, ObservableGauge

API surface (macros) maps to implementation functions. The macros resolve the current meter automatically and call the appropriate module.

```41:52:apps/opentelemetry_api_experimental/include/otel_meter.hrl
-define(counter_add(Name, Number, Attributes),
        otel_counter:add(otel_ctx:get_current(), ?current_meter, Name, Number, Attributes)).

-define(histogram_record(Name, Number, Attributes),
        otel_histogram:record(otel_ctx:get_current(), ?current_meter, Name, Number, Attributes)).

-define(register_callback(Instruments, Callback, CallbackArgs),
        otel_meter:register_callback(?current_meter, Instruments, Callback, CallbackArgs)).
```

Where implemented (Elixir shims):
- `lib/open_telemetry/*.ex` provide Elixir wrappers matching the same instrument set.

2) Instrument creation
----------------------
Macros to create instruments delegate to `otel_meter` (API) which communicates with the SDK meter provider.

```10:35:apps/opentelemetry_api_experimental/include/otel_meter.hrl
-define(create_counter(Name, Opts),
        otel_meter:create_counter(?current_meter, Name, Opts)).
-define(create_histogram(Name, Opts),
        otel_meter:create_histogram(?current_meter, Name, Opts)).
-define(create_updown_counter(Name, Opts),
        otel_meter:create_updown_counter(?current_meter, Name, Opts)).
-define(create_observable_counter(Name, Callback, CallbackArgs, Opts),
        otel_meter:create_observable_counter(?current_meter, Name, Callback, CallbackArgs, Opts)).
```

Behavioral expectations (from spec):
- Instrument name uniqueness per meter.
- Observable instruments tie to callbacks; callbacks are invoked on collection.

Where enforced (SDK): see SDK crosswalk (Streams/View matching and reader callback execution).

3) Recording semantics
----------------------
Synchronous instruments record via macros which capture the current context and meter.

```41:49:apps/opentelemetry_api_experimental/include/otel_meter.hrl
-define(counter_add(Name, Number, Attributes),
        otel_counter:add(otel_ctx:get_current(), ?current_meter, Name, Number, Attributes)).
-define(updown_counter_add(Name, Number, Attributes),
        otel_updown_counter:add(otel_ctx:get_current(), ?current_meter, Name, Number, Attributes)).
```

Spec alignment notes:
- Attributes: arbitrary key/value map is forwarded; filtering is performed by SDK views.
- Units/descriptions are provided at creation time and carried to SDK.

4) Observable semantics
-----------------------
Observable instruments can be created with a callback, or callbacks can later be registered for multiple instruments.

```50:52:apps/opentelemetry_api_experimental/include/otel_meter.hrl
-define(register_callback(Instruments, Callback, CallbackArgs),
        otel_meter:register_callback(?current_meter, Instruments, Callback, CallbackArgs)).
```

Spec alignment notes:
- Callback returns a list of measurements with attributes per instrument.
- Callbacks are run when the configured Metric Reader collects (see SDK crosswalk).

5) Scope and resource propagation
---------------------------------
The API resolves a “current meter” tied to the application/module scope. The SDK attaches instrumentation scope and resource to exported metrics.

```7:9:apps/opentelemetry_api_experimental/include/otel_meter.hrl
-define(current_meter, opentelemetry_experimental:get_meter(
                         opentelemetry:get_application_scope(?MODULE))).
```

6) Defaults (aggregation and temporality)
-----------------------------------------
Defaults are an SDK concern. API defers to SDK’s per-instrument defaults and per-reader temporality. See SDK SPEC_COMPLIANCE.md for exact mapping.

Verification steps (manual)
---------------------------
1. Configure SDK reader with console exporter and 1s interval (see SDK README).
2. Create each instrument via macros.
3. Record values; observe expected aggregation on console.
4. Register an observable callback; confirm values appear only on collection cycles.
5. Add a view in SDK that filters attributes; confirm only allowed keys appear.

Related code/tests
------------------
- API macros: `apps/opentelemetry_api_experimental/include/otel_meter.hrl`
- Erlang modules: `apps/opentelemetry_api_experimental/src/otel_*`
- Elixir wrappers: `apps/opentelemetry_api_experimental/lib/open_telemetry/*.ex`
- Tests: `apps/opentelemetry_api_experimental/test/otel_metrics_test.exs`


