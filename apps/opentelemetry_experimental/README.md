opentelemetry_experimental
=====

Signals still in experimental status in the Erlang/Elixir SDK.

# Metrics

## Aggregation

Aggregations are how measurements over a period of time are combined into either
an exact (like a sum) or a statistical metric. Each Instrument has a default
aggregation that is used if none is specified through a View by the user or
through configuration to the Reader to override the default. 

Supported aggregators:

- sum (`otel_aggregation_sum`): Collects the arithmetic sum of the Measurement values.
- drop (`otel_aggregation_drop`): Ignore Measurement values.
- last value (`otel_aggregation_last_value`): Collects only the value and timestamp of the last Measurement.
- explicit bucket histogram (`otel_aggregation_histogram_explicit`): Collects a
  histogram with static bucket boundaries.

Default aggregators for instruments:

- counter: sum
- updown counter: sum
- histogram: explicit bucket histogram
- observable counter: sum
- observable updown counter: sum
- observable gauge: last value

## View

Views allow the user to define which instruments to process and how to process
them. Processing is the aggregation of the measurements and filtering of any
attributes the View is configured to not keep in the final Metric. A View has a
name, when not specified in the configuration the name is the same as the 1
Instrument the View `selector` must match, a description, a set of criteria (the
`selector`) for knowing which Instruments it is for a list of attribute keys to
keep and the aggregation to use for the measurements.

The type definition of a View is the following:

```erlang
#{name => otel_instrument:name() | undefined,
  description => unicode:unicode_binary() | undefined,
  selector => otel_view:criteria(),
  attribute_keys => [opentelemetry:attribute_key()] | undefined,
  aggregation_module => module() | undefined,
  aggregation_options => map()}
```

All options but the selector are optional. To match all Instruments use `'*'` as
the `selector`. Not setting `attribute_keys` means to keep all attributes. Not
setting the `aggregation_module` and `aggregation_options` means the default
aggregation will be used for the type of each Instrument matched.

The following View results in a Metric `app_request_count` for Instrument
`app_request_count` to be created using  its default aggregation and keeping
only the attribute `host` from each Measurement:

```erlang
#{name => app_request_count,
  selector => #{instrument_name => app_request_count},
  attribute_keys => [host]}
```

There is a `drop` view for dropping the metrics for all matching instruments.
Example, adding a view that matches all instruments and drops the measurements
for them:

```erlang
ViewCriteria = #{instrument_name => '*'},
ViewConfig = #{aggregation_module => otel_aggregation_drop},

otel_meter_server:add_view(ViewCriteria, ViewConfig)
```

The views can be added with the `add_view` function or configured through the
OTP application environment for `opentelemetry_experimental`:

```erlang
{opentelemetry_experimental, views,
 [#{selector => #{instrument_name => '*'},
    aggregation_module => otel_aggregation_drop}]},
```

## Reader

The Metric Reader (configured under `readers`) is where the exporter to use is configured and optionally the
default aggregation of each instrument type and their temporality (delta or
cumulative). There are 2 ways for the Reader to run, periodic or pull. The pull
based Reader will wait for `collect` to be called on it and then returns the
metrics (think Prometheus) and the periodic Reader will run collection on a
configurable interval (`export_interval_ms`) and pass those to the exporter to be sent to the backend
(the default and used to send metrics to the OpenTelemetry collector).

The `readers` configuration is a list of maps that define each Reader. Each map
must have a `module` and `config`:

```erlang
#{module => otel_metric_reader,
  config => #{export_interval_ms => 1000,
              exporter => {otel_metric_exporter_console, #{}}}}
```

The above example uses the default reader `otel_metric_reader` that comes with
this application and in `config` sets a 1 second interval and an exporter that
sends to stdout. `exporter` is a 2-tuple of the module name and the config to
pass to the exporter when calling `init/1`.

## Exporter

An Exporter is responsible for serializing the Metrics and sending to some
backend for processing. Exporters are called by the Reader. There is a builtin
exporter for sending to either an OTLP backend, `otel_exporter_metrics_otlp`,
(like the OpenTelemetry collector) or to stdout, `otel_metric_exporter_console`.

The Exporter is configured as part of a Reader. For example, a Reader which uses
the stdout exporter every 5 seconds:

```erlang
{opentelemetry_experimental,
 [{readers, [#{module => otel_metric_reader,
               config => #{export_interval_ms => 5000,
                           exporter => {otel_metric_exporter_console, #{}}}}]}]},
```

# Configuration Through the Application Environment

The easiest way to configure the default reader is with the OTP application
environment: For example, a reader using the default module `otel_metric_reader`
configured to export to the console every five seconds would look like:

```erlang
{opentelemetry_experimental,
 [{readers, [#{module => otel_metric_reader,
               config => #{export_interval_ms => 5000,
                           exporter => {otel_metric_exporter_console, #{}}}}]}]},
```

## For non-Erlang reviewers

- **What to read first**: See `SPEC_COMPLIANCE.md` in this folder for a plain-language crosswalk to the OpenTelemetry Metrics spec with code anchors.
- **How the pieces fit**: Instruments created via the API are matched to Views here to produce per-Reader Streams. Readers collect and export (periodic or pull). Aggregations and temporality are enforced per Reader.
- **Quick validation**:
  - Add a console Reader (example above) and start the app.
  - Create a Counter and record a value via the API macros.
  - Observe a sum metric printed by the console exporter.
  - Add a View to filter attributes or change histogram buckets; observe effect on output.

