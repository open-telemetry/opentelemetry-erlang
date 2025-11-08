Review Guide (Metrics SDK)
==========================

Audience: Validate spec-correct behavior without reading Erlang.

Prerequisites
-------------
- Include `opentelemetry_experimental` and `opentelemetry_api_experimental` apps.
- Configure a periodic console Reader (1s) via application env:

```erlang
{opentelemetry_experimental,
 [{readers, [#{module => otel_metric_reader,
               config => #{export_interval_ms => 1000,
                           exporter => {otel_metric_exporter_console, #{}}}}]}]}.
```

1) Counter monotonicity
-----------------------
Goal: Negative values are discarded; only increases are summed.
- Create Counter `app_counter`; record `+5`, then `-3`.
- Expect: Console shows only positive increments; negative is ignored.

Code anchor:
```386:401:apps/opentelemetry_experimental/src/otel_meter_server.erl
maybe_init_aggregate(_, _Meter, Value, #instrument{kind=Kind} = Instrument, _Stream, _Attributes)
        when Value < 0, Kind == ?KIND_COUNTER orelse Kind == ?KIND_HISTOGRAM ->
    ?LOG_INFO("Discarding negative value for instrument ~s of type ~s", [Instrument#instrument.name, Kind]),
    ok;
```

2) UpDownCounter accepts negatives
----------------------------------
Goal: UpDownCounter supports positive and negative adds.
- Create UpDownCounter `app_gauge`; record `+3`, `-2`.
- Expect: Sum reflects net value.

3) Histogram with explicit buckets
----------------------------------
Goal: View overrides buckets.
- Add View with `aggregation_module=otel_aggregation_histogram_explicit` and `aggregation_options` with custom `explicit_bucket_boundaries`.
- Record values across ranges.
- Expect: Bucket counts match boundaries.

Example:
```erlang
ViewCriteria = #{instrument_name => my_histogram},
ViewConfig = #{aggregation_module => otel_aggregation_histogram_explicit,
               aggregation_options => #{explicit_bucket_boundaries => [0, 10, 50, 100]}},
ok = otel_meter_server:add_view(ViewCriteria, ViewConfig).
```

4) Attribute filtering via Views
--------------------------------
Goal: Only allowed attributes remain.
- Add View `attribute_keys => [host]` for an instrument.
- Record with attributes `host`, `user`.
- Expect: Exported point contains only `host`.

5) Observable instruments and callbacks
---------------------------------------
Goal: Callbacks run on collection and can serve multiple instruments.
- Create two observable gauges; register a single callback returning both values.
- Expect: Values appear each collect tick; nothing emitted between collects.

Callback execution anchor:
```249:256:apps/opentelemetry_experimental/src/otel_metric_reader.erl
run_callbacks(ReaderId, CallbacksTab, StreamsTab, MetricsTab, ExemplarsTab) ->
    try ets:lookup_element(CallbacksTab, ReaderId, 2) of
        Callbacks -> otel_observables:run_callbacks(Callbacks, ReaderId, StreamsTab, MetricsTab, ExemplarsTab)
    catch error:badarg -> [] end.
```

6) Temporality per Reader (delta vs cumulative)
-----------------------------------------------
Goal: Different readers expose different temporalities.
- Configure two readers: one delta, one cumulative.
- Record successive Counter adds.
- Expect: Delta reader reports per-interval increments; cumulative reports running total.

Reader collection/export anchor:
```172:207:apps/opentelemetry_experimental/src/otel_metric_reader.erl
collect_(State=...) ->
    Metrics = run_collection(...),
    otel_exporter_metrics:export(Exporter, Metrics, Resource),
    ...
```

7) Drop via View
----------------
Goal: Measurements can be dropped.
- Add View with `aggregation_module=otel_aggregation_drop` selecting an instrument.
- Record values.
- Expect: No points exported for that instrument.

8) Resource and scope propagation
---------------------------------
Goal: Exported metrics include resource and scope.
- Expect: Console output shows scope (instrumentation scope) and metrics under configured resource.

If something fails
------------------
- Ensure the SDK app is started and the reader is configured.
- Force a one-time collection: call the reader’s `call_collect/1` if using pull mode.

Pointers
--------
- Architecture: `ARCHITECTURE.md`
- Spec crosswalk: `SPEC_COMPLIANCE.md`


