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
model. YAML parsing, environment variable substitution, and JSON Schema
validation must be completed before the SDK starts.

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
    #{composite => [trace_context, baggage]}},
   {tracer_provider,
    #{processors =>
          [{otel_batch_processor,
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
    composite: [:trace_context, :baggage]
  },
  tracer_provider: %{
    processors: [
      {:otel_batch_processor,
       %{exporter:
           {:otlp_http,
            %{endpoint: "http://localhost:4318/v1/traces"}}}}
    ]
  }
```

`OTEL_CONFIG_FILE` takes precedence over the application environment. Other
OpenTelemetry SDK environment variables are not read as an independent
configuration source. They may be referenced during the external substitution
step used to produce the JSON document.

The previous flat application keys such as `processors`, `sampler`,
`text_map_propagators`, and `traces_exporter` are rejected at startup. Put their
declarative equivalents under `tracer_provider` or `propagator`.

The SDK retains the complete source tree and creates a typed runtime
configuration from it. Resource attributes become maps, ordered name/value
pairs such as headers become tuple lists, and components become tagged tuples.
Batch and simple processors read fields such as `schedule_delay`,
`export_timeout`, and `max_queue_size` directly. Every processor receives the
resource belonging to its own tracer provider.

Missing `tracer_provider` and `propagator` sections have the declarative
model's no-op behavior. In particular, an empty application environment does
not start a tracer provider. `meter_provider` is retained in the configuration
model but is not interpreted by the stable application yet; metrics remain the
responsibility of the experimental application while that implementation is
being redesigned.

### Erlang components

Application configuration may use an atom component name to directly select an
Erlang implementation module. This provides programmatic configuration for
components that cannot be represented by portable JSON. For example:

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
       [{otel_simple_processor,
         #{exporter =>
               {my_span_exporter, #{exporter_option => value}}}}]}}
```

These atom forms are available only to the Erlang application configuration.
Names from JSON remain binaries and are not converted into module names.

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

Resource environment variables are only read when the corresponding resource
detector is explicitly configured. Portable resource attributes should usually
be supplied through the standard `resource` section.

The span sweeper periodically handles spans for which `end_span` was never
called. Its strategies are `drop`, `end_span`, and
`failed_attribute_and_end_span`; a function may also be supplied through the
Erlang application environment.

## Contributing

Read the OpenTelemetry project
[contributing guide](https://github.com/open-telemetry/community/blob/main/CONTRIBUTING.md)
for general information about the project.
