opentelemetry_experimental – Spec Compliance Crosswalk (Metrics SDK)
===================================================================

Audience: Reviewers validating OpenTelemetry Metrics SDK behavior without Erlang knowledge.

Scope: MeterProvider, Views, Streams, Aggregations, Temporality, Readers, Observables, Exporters, Exemplars, Resource/Scope.

How to read code anchors: Blocks below cite file, start:end lines. They are illustrative; exact line numbers may drift slightly across commits.

1) MeterProvider responsibilities
---------------------------------
Creates meters, manages instruments, registers views, and wires readers. Matching instruments to views produces per-reader streams.

```33:52:apps/opentelemetry_experimental/src/otel_meter_server.erl
%% MeterProvider responsibilities and API surface
```

Streams creation on instrument add and when views/readers change:

```333:359:apps/opentelemetry_experimental/src/otel_meter_server.erl
update_streams_(Instrument, CallbacksTab, StreamsTab, Views, Readers, ExemplarsEnabled, ExemplarFilter) ->
    ViewMatches = otel_view:match_instrument_to_views(Instrument, Views, ExemplarsEnabled, ExemplarFilter),
    lists:foreach(fun(Reader=#reader{id=ReaderId}) ->
                          Matches = per_reader_aggregations(Reader, Instrument, ViewMatches),
                          [true = otel_metrics_tables:insert_stream(StreamsTab, Meter, Name, M) || M <- Matches],
                          case {Instrument#instrument.callback, Instrument#instrument.callback_args} of
                              {undefined, _} -> ok;
                              {Callback, CallbackArgs} ->
                                  otel_metrics_tables:insert_callback(CallbacksTab, ReaderId, Callback, CallbackArgs, Instrument)
                          end
                  end, Readers).
```

2) View selection and defaults
------------------------------
Views select instruments by name/kind/unit and meter scope; can filter attributes and override aggregation.

```64:91:apps/opentelemetry_experimental/src/otel_view.erl
new(Criteria, Config) -> ... %% builds view with matchspec and config
```

Wildcard rule (named wildcard forbidden):

```69:76:apps/opentelemetry_experimental/src/otel_view.erl
new(Name, #{instrument_name := '*'}, _Config) ->
    ?LOG_INFO("Wildacrd Views can not have a name, discarding view ~s", [Name]),
    {error, named_wildcard_view};
```

Matching; default when no view matched yields a stream with instrument defaults:

```92:139:apps/opentelemetry_experimental/src/otel_view.erl
match_instrument_to_views(Instrument, Views, ExemplarsEnabled, ExemplarFilter) ->
    case lists:filtermap(...) of
        [] -> [{undefined, #stream{name=InstrumentName, ...}}];
        Aggs -> Aggs
    end.
```

3) Aggregations and temporality
-------------------------------
Default aggregation mapping per instrument kind; per-reader temporality mapping.

```366:379:apps/opentelemetry_experimental/src/otel_meter_server.erl
aggregation_module(Instrument, View, Reader) -> ... %% view override or reader default
```

Reader collects with set temporality and calls exporter:

```172:207:apps/opentelemetry_experimental/src/otel_metric_reader.erl
collect_(State=...) ->
    Metrics = run_collection(...),
    otel_exporter_metrics:export(Exporter, Metrics, Resource),
    ...
```

4) Recording path and validation
--------------------------------
Measurement handling updates per-stream aggregations. Negative values are rejected for monotonic instruments per spec.

```386:405:apps/opentelemetry_experimental/src/otel_meter_server.erl
maybe_init_aggregate(_, _Meter, Value, #instrument{kind=Kind} = Instrument, _Stream, _Attributes)
        when Value < 0, Kind == ?KIND_COUNTER orelse Kind == ?KIND_HISTOGRAM ->
    ?LOG_INFO("Discarding negative value for instrument ~s of type ~s", [Instrument#instrument.name, Kind]),
    ok;
```

5) Observables
--------------
Callbacks registered per reader; executed during collection; supports multi-instrument callbacks.

```249:256:apps/opentelemetry_experimental/src/otel_metric_reader.erl
run_callbacks(ReaderId, CallbacksTab, StreamsTab, MetricsTab, ExemplarsTab) ->
    try ets:lookup_element(CallbacksTab, ReaderId, 2) of
        Callbacks -> otel_observables:run_callbacks(Callbacks, ReaderId, StreamsTab, MetricsTab, ExemplarsTab)
    catch error:badarg -> [] end.
```

6) Readers (periodic vs pull)
-----------------------------
- `export_interval_ms` undefined → pull-based; `collect/1` or `call_collect/1` triggers.
- Set interval → periodic collection with rescheduled timer.

```88:107:apps/opentelemetry_experimental/src/otel_metric_reader.erl
init([ReaderId, ProviderSup, Config]) ->
    Exporter = otel_exporter_metrics:init(...),
    ExporterIntervalMs = maps:get(export_interval_ms, Config, undefined),
    TRef = case ExporterIntervalMs of undefined -> undefined; _ -> erlang:send_after(ExporterIntervalMs, self(), collect) end,
```

7) Exporters
------------
Console exporter and OTLP exporter supported for metrics; logs exporters present but out of scope for metrics review.

Files: `otel_metric_exporter_console.erl`, `otel_exporter_metrics_otlp.erl`.

8) Exemplars
------------
Reservoirs and filters implemented; per-stream exemplar reservoir chosen based on instrument kind and filter.

```98:107:apps/opentelemetry_experimental/src/otel_view.erl
ExemplarReservoir = otel_metric_exemplar:reservoir(Kind, ExemplarsEnabled, ExemplarFilter),
```

9) Resource and scope propagation
---------------------------------
Reader exports include resource; metric scope set from meter/instrumentation scope.

```288:294:apps/opentelemetry_experimental/src/otel_metric_reader.erl
metric(#instrument{meter={_, Meter=#meter{}}}, Name, Description, Unit, Data) ->
    #metric{scope=otel_meter_default:scope(Meter), name=Name, description=Description, unit=Unit, data=Data}.
```

10) Verification steps (manual)
-------------------------------
1. Configure a periodic reader with console exporter (1s interval).
2. Create instruments; record values (API macros). Expect default aggregations.
3. Add a view filtering to attribute allow-list; confirm attributes filtered.
4. Override histogram bucket boundaries; confirm bucket counts change.
5. Configure two readers with different temporalities; confirm delta vs cumulative behavior.
6. Register observable callback for multiple instruments; values exported on each collect.

Related code/tests
------------------
- MeterProvider: `src/otel_meter_server.erl`
- Views: `src/otel_view.erl`
- Reader: `src/otel_metric_reader.erl`
- Aggregations: `src/otel_aggregation_*.erl`
- Exporters: `src/otel_metric_exporter_console.erl`, `src/otel_exporter_metrics_otlp.erl`
- Observables: `src/otel_observables.erl`


