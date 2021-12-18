# OpenTelemetry Zipkin Exporter

Zipkin exporter for OpenTelemetry Erlang/Elixir instrumentation.

Requires Zipkin 2.8 or above because it uses the [v2 API](https://zipkin.io/zipkin-api/#/default/post_spans) with [protobuf](https://github.com/openzipkin/zipkin-api/blob/master/zipkin.proto) content type.

## Setup

Easiest way to setup is to add configuration for the batch processor in OpenTelemetry application environment.

For an Erlang release in `sys.config`:

``` erlang
{opentelemetry,
  [{processors, 
    [{otel_batch_processor,
        #{exporter => {opentelemetry_zipkin, #{address => "http://localhost:9411/api/v2/spans",
                                               local_endpoint => #{service_name => <<"ServiceName">>}}}}}]}]}
```

An Elixir release uses `releases.exs`:

``` elixir
config :opentelemetry,
    :processors, otel_batch_processor: %{exporter: {:opentelemetry_zipkin, %{address: 'http://localhost:9411/api/v2/spans', local_endpoint: %{service_name: "ServiceName"}}}}
```
