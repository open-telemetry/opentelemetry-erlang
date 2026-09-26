# Erlang/Elixir OpenTelemetry SDK

[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry?label=SDK&style=for-the-badge)](https://hex.pm/packages/opentelemetry)
[![EEF Observability WG
project](https://img.shields.io/badge/EEF-Observability-black?style=for-the-badge)](https://github.com/erlef/eef-observability-wg)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/open-telemetry/opentelemetry-erlang/Erlang?style=for-the-badge)](https://github.com/open-telemetry/opentelemetry-erlang/actions)

The SDK is an implementation of the
[OpenTelemetry tracing SDK](https://opentelemetry.io/docs/specs/otel/trace/sdk/)
and should be included in the final deployable artifact, usually an OTP
release.

## Configuration

The SDK uses the
[OpenTelemetry declarative configuration](https://github.com/open-telemetry/opentelemetry-configuration)
model. When using a configuration file, YAML parsing, environment variable
substitution, and JSON Schema validation must be completed before the SDK starts.

With no configuration file and an empty `opentelemetry` application environment,
the SDK starts a batch processor with OTLP HTTP/protobuf export to
`http://localhost:4318/v1/traces`, a parent-based always-on sampler, and Trace
Context and Baggage propagation. Standard environment variables configure this
zero-config startup, for example:

```shell
export OTEL_EXPORTER_OTLP_ENDPOINT=http://collector:4318
export OTEL_SERVICE_NAME=checkout
export OTEL_RESOURCE_ATTRIBUTES=deployment.environment.name=production
export OTEL_TRACES_SAMPLER=parentbased_traceidratio
export OTEL_TRACES_SAMPLER_ARG=0.1
```

This mode also supports `OTEL_SDK_DISABLED`, `OTEL_PROPAGATORS`,
`OTEL_TRACES_EXPORTER=none`, OTLP protocol/headers/compression settings, batch
processor timing and queue settings, and attribute/span limits. Trace-specific
OTLP variables take precedence over general OTLP variables. The general HTTP
endpoint receives a `/v1/traces` suffix; a trace-specific endpoint is used as-is.
Empty environment values are treated as unset. Invalid or unsupported values
produce warnings and are ignored. The older `OTEL_BSP_SCHEDULE_DELAY_MILLIS`
and `OTEL_BSP_EXPORT_TIMEOUT_MILLIS` names remain accepted, with the standard
names taking precedence.

Loading a JSON configuration file requires **Erlang/OTP 27 or later**, because
the SDK uses the built-in [`json:decode/1`](https://www.erlang.org/doc/apps/stdlib/json.html#decode/1)
function introduced in OTP 27. Native application configuration (`sys.config`
or `runtime.exs`) and zero-config environment defaults do not use the JSON
decoder and do not impose this additional OTP requirement.

Set `OTEL_CONFIG_FILE` to a preprocessed JSON document:

```shell
export OTEL_CONFIG_FILE=/path/to/otel-sdk-config.json
```

```json
{
  "file_format": "1.1",
  "resource": {
    "attributes": [
      {"name": "service.name", "value": "checkout"}
    ]
  },
  "propagator": {
    "composite": [
      {"tracecontext": null},
      {"baggage": null}
    ]
  },
  "tracer_provider": {
    "processors": [
      {
        "batch": {
          "exporter": {
            "otlp_http": {
              "endpoint": "http://localhost:4318/v1/traces"
            }
          }
        }
      }
    ]
  }
}
```

The JSON decoder does not create atoms from property or component names.
Unsupported components cause SDK startup to fail with a configuration error.

The `opentelemetry` application environment expresses the same settings using
idiomatic Erlang terms. The `file_format` property may be omitted because
application configuration is not a versioned file:

```erlang
[
 {opentelemetry,
  [{resource,
    #{attributes =>
          #{<<"service.name">> => <<"checkout">>}}},
   {propagator,
    #{composite => [tracecontext, baggage]}},
   {tracer_provider,
    #{processors =>
          [{batch,
            #{exporter =>
                  {otlp_http,
                   #{endpoint => <<"http://localhost:4318/v1/traces">>}}}}],
      sampler => {parent_based, #{root => always_on}}}}]}
].
```

Elixir configuration uses the same shape:

```elixir
config :opentelemetry,
  propagator: %{
    composite: [:tracecontext, :baggage]
  },
  tracer_provider: %{
    processors: [
      {:batch,
       %{exporter:
           {:otlp_http,
            %{endpoint: "http://localhost:4318/v1/traces"}}}}
    ]
  }
```

Configuration precedence is:

1. A nonempty `OTEL_CONFIG_FILE` selects the preprocessed JSON document.
2. Otherwise, a nonempty `opentelemetry` application environment supplies the
   complete native configuration.
3. Otherwise, the SDK uses environment variables and zero-config defaults.

Explicit file and native configurations are authoritative: other OTEL
variables neither override their values nor fill omitted sections. For files,
variables may be referenced during the external substitution step. An invalid
explicit configuration fails startup rather than falling back to environment
configuration.

The previous flat application keys such as `processors`, `sampler`,
`text_map_propagators`, and `traces_exporter` are rejected at startup. Put their
declarative equivalents under `tracer_provider` or `propagator`.

The SDK retains the complete source tree and creates a typed runtime
configuration from it. Resource attributes become maps, ordered name/value
pairs such as headers become tuple lists, and components become tagged tuples.
Batch and simple processors read fields such as `schedule_delay`,
`export_timeout`, and `max_queue_size` directly. Every processor receives the
resource belonging to its own tracer provider.

The built-in processor component names are `batch` and `simple`. Application
configuration writes them as `{batch, Options}` and `{simple, Options}`, using
the same names as the declarative document. The implementation module names
`otel_batch_processor` and `otel_simple_processor` remain accepted for
compatibility, but component names are preferred in configuration.

The canonical native form for a component with options is a tagged tuple,
such as `{batch, Options}` or `{otlp_http, Options}`. The equivalent single-entry
maps, `#{batch => Options}` and `#{otlp_http => Options}`, are also accepted to
ease translation from JSON. Prefer tagged tuples in `sys.config` and
`runtime.exs`; JSON uses single-entry objects.

For built-in propagators without options, use atoms in `composite`, for example
`[tracecontext, baggage]`. `tracecontext` is the canonical name, matching the
declarative schema and `OTEL_PROPAGATORS`; `trace_context` remains an accepted
native alias.

Built-in processors reject unknown option names with a path-specific error.
When migrating older native configurations, rename `scheduled_delay_ms` to
`schedule_delay`, `exporting_timeout_ms` to `export_timeout`, and
`check_table_size_ms` to `check_table_size`. Durations remain in milliseconds.
Batch-only options are not accepted by `simple`. Custom processors retain
control over their own option names.

The built-in span exporter names are `otlp_http`, `otlp_grpc`, and `console`.
`console` uses `otel_exporter_stdout` to print spans for debugging and has no
options. For example, native configuration can use:

```erlang
{tracer_provider,
 #{processors => [{simple, #{exporter => {console, #{}}}}]}}
```

The equivalent JSON processor is
`{"simple": {"exporter": {"console": {}}}}`. `{"console": null}` is also
accepted. Both `simple` and `batch` support this exporter.

In an explicit file or native configuration, missing `tracer_provider` and
`propagator` sections have the declarative model's no-op behavior. For example,
`{tracer_provider, null}` explicitly disables provider creation without opting
into zero-config defaults. `meter_provider` and `logger_provider` are retained
in the configuration model but are not interpreted by the stable application
yet. Their presence produces a warning. Metrics remain the responsibility of
the experimental application while that implementation is being redesigned.

Some optional settings are retained but ignored with a warning identifying the
configuration path: `log_level`, attribute value depth limits, `max_export_batch_size`,
OTLP exporter `timeout`, `max_request_size` and `max_response_size`, gRPC
`tls.insecure`, resource `detection/development`, and
`tracer_configurator/development`. The corresponding SDK behavior remains
unchanged; accepting these properties does not implement their limits or
features. For gRPC transport security, use an explicit `http://` or `https://`
endpoint. Invalid values and unresolved components still return errors.

`log_level` (including `OTEL_LOG_LEVEL` in zero-config mode) is validated and
retained in the source model but does not control SDK logging yet. Configure
Erlang/OTP's `logger` directly to control log output.

For `tracer_provider.limits.attribute_count_limit` and
`attribute_value_length_limit`, omission inherits the corresponding
`attribute_limits` value. An explicit `null` resets the field to the span
schema default: 128 attributes or no length limit. For example, with a global
count limit of 64, an omitted span count limit resolves to 64, while an explicit
`null` resolves to 128. Omitting or nulling the whole `limits` map leaves its
fields absent, so the global values are inherited. This follows the
[Create null handling rule](https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/configuration/sdk.md#create)
and the [SpanLimits defaults](https://github.com/open-telemetry/opentelemetry-configuration/blob/fce52c3f13cc96f41ee5493598d1354a62367641/schema/tracer_provider.yaml).

### Erlang components

Application configuration may also directly select an Erlang implementation
module. This is the current extension point for components that cannot be
represented by portable JSON. For example:

```erlang
{tracer_provider,
 #{processors =>
       [{my_span_processor, #{processor_option => value}}],
   sampler => {my_sampler, #{sampler_option => value}},
   id_generator => my_id_generator}}
```

An atom exporter name may be used inside a standard processor:

```erlang
{tracer_provider,
 #{processors =>
       [{simple,
         #{exporter =>
               {my_span_exporter, #{exporter_option => value}}}}]}}
```

These atom forms are available only to the Erlang application configuration.
Names from JSON remain binaries and are not converted into module names.

### Named tracer providers

Start additional named tracer providers through `otel_tracer_provider_sup`.
Its configuration map has the same shape as the `tracer_provider` application
environment entry and is validated before any provider processes start:

```erlang
Resource = otel_resource:create(
             [{<<"service.name">>, <<"checkout-worker">>}]),
{ok, _} = otel_tracer_provider_sup:start(
            checkout_worker,
            Resource,
            #{processors =>
                  [{batch,
                    #{exporter =>
                          {otlp_http,
                           #{endpoint =>
                                 <<"http://localhost:4318/v1/traces">>}}}}],
              sampler => always_on}).
```

`start/2` uses an empty resource. `start/3` accepts an `otel_resource:t()` as
shown above. Both functions return configuration errors directly, including
the path of an invalid option.

### Erlang distribution settings

Settings specific to this SDK live under `distribution.erlang`:

```erlang
{distribution,
 #{erlang =>
       #{create_application_tracers => false,
         deny_list => [kernel],
         resource_detectors => [otel_resource_env_var],
         resource_detector_timeout => 5000,
         sweeper =>
             #{interval => 600000,
               strategy => drop,
               span_ttl => 1800000,
               storage_size => infinity}}}}
```

With explicit SDK configuration, resource environment variables are only read
when the corresponding resource detector is explicitly configured.
`otel_resource_env_var` reads `OTEL_RESOURCE_ATTRIBUTES` and `OTEL_SERVICE_NAME`;
a nonempty `OTEL_SERVICE_NAME` overrides `service.name` from
`OTEL_RESOURCE_ATTRIBUTES`. Attributes supplied through the standard `resource`
section take precedence over detected attributes. Without any SDK configuration,
the environment defaults described above read these variables automatically.

The span sweeper periodically handles spans for which `end_span` was never
called. Its strategies are `drop`, `end_span`, and
`failed_attribute_and_end_span`; a function may also be supplied through the
Erlang application environment.

## Contributing

Read the OpenTelemetry project
[contributing guide](https://github.com/open-telemetry/community/blob/main/CONTRIBUTING.md)
for general information about the project.
