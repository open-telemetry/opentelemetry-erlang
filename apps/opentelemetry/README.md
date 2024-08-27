# Erlang/Elixir OpenTelemetry SDK

[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry?label=SDK&style=for-the-badge)](https://hex.pm/packages/opentelemetry)
[![EEF Observability WG
project](https://img.shields.io/badge/EEF-Observability-black?style=for-the-badge)](https://github.com/erlef/eef-observability-wg)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/open-telemetry/opentelemetry-erlang/Erlang?style=for-the-badge)](https://github.com/open-telemetry/opentelemetry-erlang/actions)

The [SDK](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/trace/sdk.md) is an implementation of the [OpenTelemetry
API](https://hex.pm/packages/opentelemetry_api) and should be included in your
final deployable artifact (usually an OTP Release).

## Configuration

The SDK starts up its supervision tree on boot, so initial configuration must be
done through the Application or [OS environment
variables](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/sdk-configuration.md).
The following example configurations show configuring the SDK to use the batch
span processor which then exports through the [OpenTelemetry
Protocol](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/protocol/otlp.md)
over HTTP to `http://localhost:4318`, encoding the Spans with protobufs.

```erlang
[
 {opentelemetry,
  [{span_processor, batch},
   {traces_exporter, otlp}]},

 {opentelemetry_exporter, 
  [{otlp_protocol, http_protobuf},
   {otlp_endpoint, "http://localhost:4318"}]}]}
].
```

```elixir
config :opentelemetry, 
  span_processor: :batch,
  traces_exporter: :otlp

config :opentelemetry_exporter,
  otlp_protocol: :http_protobuf,
  otlp_endpoint: "http://localhost:4318"
```

If your exporting pipeline is not ready, you may disable trace exporting to prevent memory overflows.
This can be done by setting `traces_exporter`'s value to the atom `none`.

```erlang
[
 {opentelemetry,
  [{traces_exporter, none}]}
].
```

```elixir
config :opentelemetry, traces_exporter: :none
```

The following sections detail the available SDK options and how to configure
them through either the OS or Application environment.

### Automatically Created Tracers

Spans are created by Tracers and each Tracer is associated with a [name and
version](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/trace/api.md#get-a-tracer).
The name and version can be anything, but the macros provided by the API will always use
a Tracer with name/version of the OTP Application the macro is used in. To
facilitate this a Tracer is created for each available Application when the SDK
boots. It relies on mapping modules to Applications, so if there is a huge
number of modules in a Release the time to create all these Tracers can become
more than you want to wait on boot.

| OS                              | Application                | Default | Type    |
|:--------------------------------|:---------------------------|:--------|:--------|
| OTEL_CREATE_APPLICATION_TRACERS | create_application_tracers | true    | boolean |

Disabling the creation of Tracers at start-up means when the macros are used to
start Spans the name and version of the Tracer creating those Spans will be
undefined, but everything will continue to work as before.

### Samplers

Sampling is a mechanism to control the number of traces collected and sent to the backend. A [Sampler](https://opentelemetry.io/docs/reference/specification/trace/sdk/#sampler) is responsible for making a decision on whether to sample a trace or not.

This decision happens when starting a span. For this reason, only the initial attributes passed to `with_span` or `start_span` are available to the Sampler. Attributes added within `with_span` or between `start_span` and `end_span` calls don't affect the outcome as the decision to sample or not has already been made.

There are several [built-in samplers](https://opentelemetry.io/docs/reference/specification/trace/sdk/#built-in-samplers) available, and it's possible to create custom samplers.

| OS                             | Application              | Default   | Type    |
|:-------------------------------|:-------------------------|:----------|:--------|
| OTEL_TRACES_SAMPLER             | sampler                 | parentbased_always_on | always_on, always_off, traceidratio, parentbased_always_on, parentbased_always_off, parentbased_traceidratio |
| OTEL_TRACES_SAMPLER_ARG         | sampler                 |          | String. Each Sampler type defines its own expected input, if any.|


#### Custom Sampler

To create a custom sampler, implement a module with behaviour `otel_sampler`, and then install your custom sampler when configuring the `opentelemetry` application

```elixir
defmodule MySampler do
  require OpenTelemetry.Tracer, as: Tracer
  @behaviour :otel_sampler

  @impl :otel_sampler
  def setup(_sampler_opts) do
    []
  end

  @impl :otel_sampler
  def description(_sampler_config) do
    "MySampler"
  end

  @impl :otel_sampler
  def should_sample(
        ctx,
        _trace_id,
        _links,
        _span_name,
        _span_kind,
        _attributes,
        _sampler_config
  ) do
    sample_decision = ... # custom logic to determine whether to sample or not

    tracestate = Tracer.current_span_ctx(ctx) |> OpenTelemetry.Span.tracestate()

    case sample_decision do
      true -> {:record_and_sample, [], tracestate}
      false -> {:drop, [], tracestate}
    end
  end
end
```

```elixir
config :opentelemetry,
  sampler: {:parent_based, %{root: {MySampler, %{my_config_arg: 1}}}},
  span_processor: :batch,
  traces_exporter: :otlp
```

### Propagators

Propagators define how to inject and extract context in requests to or from
external services. A propagator that works on key/value pairs of text, like HTTP
headers, can be any module that implements the `otel_propagator_text_map`
behaviour. There are 4 built-in propagators that are part of the API:

- [tracecontext](https://w3c.github.io/trace-context/): Encodes the Trace
  context as headers `traceparent` and `tracestate`.
- [baggage](https://w3c.github.io/baggage/): Encodes
  [baggage](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/baggage/api.md)
  elements as a comma delimited list under header key `baggage`.
- [b3multi](https://github.com/openzipkin/b3-propagation#multiple-headers): The
  propagation format from [Zipkin](https://zipkin.io/) that breaks the context across multiple
  headers, `X-B3-TraceId`, `X-B3-SpanId` and `X-B3-Sampled`.
- [b3](https://github.com/openzipkin/b3-propagation#single-header):  The
  propagation format from Zipkin that encodes to a single header `b3`.

| OS               | Application          | Default                | Type                                                             |
|:-----------------|:---------------------|:-----------------------|:-----------------------------------------------------------------|
| OTEL_PROPAGATORS | text_map_propagators | [tracecontext,baggage] | List of propagators (`tracecontext`, `baggage`, `b3multi`, `b3`) |


### Batch Span Processor

[Span
processors](https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/sdk.md#span-processor)
are invoked when a Span is started and when it ends. The default processor is
the batch processor (`otel_batch_processor`) -- also available is the simple
processor (`otel_simple_processor`) which blocks on every finished Span until it
is exported.

The batch processor waits `bsp_scheduled_delay_ms` milliseconds or until the buffer of
finished Spans reaches `bsp_max_queue_size` and then exports the spans together.
The export is canceled if it takes longer than `bsp_exporting_timeout_ms`.

| OS                             | Application              | Default | Type    |
|:-------------------------------|:-------------------------|:--------|:--------|
| OTEL_BSP_SCHEDULE_DELAY_MILLIS | bsp_scheduled_delay_ms   | 5000    | integer |
| OTEL_BSP_EXPORT_TIMEOUT_MILLIS | bsp_exporting_timeout_ms | 30000   | integer |
| OTEL_BSP_MAX_QUEUE_SIZE        | bsp_max_queue_size       | 2048    | integer |

### Resource Detectors

[Resources](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/overview.md#resources)
capture information about the environment the program is running in. For
example, a process producing telemetry that is running in a container on
Kubernetes has a Pod name, it is in a namespace and possibly is part of a
Deployment which also has a name. All three of these attributes can be included
in the Resource.

| OS                             | Application               | Default                                        | Type            |
|:-------------------------------|:--------------------------|:-----------------------------------------------|:----------------|
| OTEL_RESOURCE_DETECTORS        | resource_detectors        | [otel_resource_env_var, otel_resource_app_env] | list of modules |
| OTEL_RESOURCE_DETECTOR_TIMEOUT | resource_detector_timeout | 5000                                           | integer         |
    
The default detectors read resource attributes from the OS environment variable
`OTEL_RESOURCE_ATTRIBUTES` and Application environment variable `resource`.

### Span Limits

The number of Attributes, Events and Links on a Span are limited, as well as the
length of an Attribute's value. When the limit is reached any additional
Attributes, Events or Links are dropped and Attribute values larger than the
length limit are truncated.

| OS                                     | Application                  | Default  | Type                    |
|:---------------------------------------|:-----------------------------|:---------|:------------------------|
| OTEL_SPAN_ATTRIBUTE_COUNT_LIMIT        | attribute_count_limit        | 128      | integer                 |
| OTEL_SPAN_ATTRIBUTE_VALUE_LENGTH_LIMIT | attribute_value_length_limit | infinity | integer \| infinity     |
| OTEL_SPAN_EVENT_COUNT_LIMIT            | event_count_limit            | 128      | integer                 |
| OTEL_SPAN_LINK_COUNT_LIMIT             | link_count_limit             | 128      | integer                 |
| OTEL_EVENT_ATTRIBUTE_COUNT_LIMIT       | attribute_per_event_limit    | 128      | integer                 |
| OTEL_LINK_ATTRIBUTE_COUNT_LIMIT        | attribute_per_link_limit     | 128      | integer                 |

Read more in the specification about [Span
limits](https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/sdk.md#span-limits)
and [Attribute
limits](https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/common/common.md#attribute-limits).

### Span Sweeper

Because of Erlang's "Let It Crash" philosophy it is possible that at some point
a Span will never have `end_span` called. To handle these cases the SDK includes
a process that will periodically check the active Span table for Spans that are
older than the `span_ttl` and will drop or end the Span depending on the
configured `strategy`. The `failed_attribute_and_end_span` strategy will add the
attribute `finished_by_sweeper` with value `true` to the Span before calling
`end_span`.

The sweeper's configuration is a map under environment key `sweeper`.

| OS                             | Application  | Default | Type                                                        |
|:-------------------------------|:-------------|:---------|:-----------------------------------------------------------|
| OTEL_SPAN_SWEEPER_INTERVAL     | interval     | 600000   | integer \| infinity                                        |
| OTEL_SPAN_SWEEPER_STRATEGY     | strategy     | drop     | drop \| end_span \| failed_attribute_and_end_span \| fun/1 |
| OTEL_SPAN_SWEEPER_SPAN_TTL     | span_ttl     | 1800000  | integer \| infinity                                        |
| OTEL_SPAN_SWEEPER_STORAGE_SIZE | storage_size | infinity | integer \| infinity                                        |

## Contributing

Read OpenTelemetry project [contributing
guide](https://github.com/open-telemetry/community/blob/main/CONTRIBUTING.md)
for general information about the project.
