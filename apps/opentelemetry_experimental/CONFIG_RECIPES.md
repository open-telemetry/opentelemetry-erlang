Configuration Recipes (Metrics SDK)
===================================

Console reader (periodic)
-------------------------
```erlang
{opentelemetry_experimental,
 [{readers, [#{module => otel_metric_reader,
               config => #{export_interval_ms => 1000,
                           exporter => {otel_metric_exporter_console, #{}}}}]}]}.
```

Drop all measurements
---------------------
```erlang
ViewCriteria = #{instrument_name => '*'},
ViewConfig = #{aggregation_module => otel_aggregation_drop},
ok = otel_meter_server:add_view(ViewCriteria, ViewConfig).
```

Attribute allow-list (keep `host` only)
---------------------------------------
```erlang
ViewCriteria = #{instrument_name => app_request_count},
ViewConfig = #{attribute_keys => [host]},
ok = otel_meter_server:add_view(ViewCriteria, ViewConfig).
```

Explicit histogram buckets
--------------------------
```erlang
ViewCriteria = #{instrument_name => latency_ms},
ViewConfig = #{aggregation_module => otel_aggregation_histogram_explicit,
               aggregation_options => #{explicit_bucket_boundaries => [0, 5, 10, 25, 50, 100, 250, 500]}},
ok = otel_meter_server:add_view(ViewCriteria, ViewConfig).
```

Two readers with different temporalities
----------------------------------------
Example: one delta, one cumulative (merge with defaults as needed).
```erlang
Readers = [
  #{module => otel_metric_reader,
    config => #{export_interval_ms => 1000,
                exporter => {otel_metric_exporter_console, #{}}}},
  #{module => otel_metric_reader,
    config => #{export_interval_ms => 1000,
                default_temporality_mapping => #{counter => ?TEMPORALITY_DELTA},
                exporter => {otel_metric_exporter_console, #{}}}}
],
{opentelemetry_experimental, [{readers, Readers}]}.
```

OTLP exporter (metrics)
-----------------------
```erlang
{opentelemetry_experimental,
 [{readers, [#{module => otel_metric_reader,
               config => #{export_interval_ms => 1000,
                           exporter => {otel_exporter_metrics_otlp, #{endpoint => <<"http://localhost:4317">>}}}}]}]}.
```

Notes
-----
- Views can be configured via application env as a list under `{opentelemetry_experimental, views, [...]}`.
- Temporality mapping keys follow instrument kinds; unspecified kinds use defaults.


