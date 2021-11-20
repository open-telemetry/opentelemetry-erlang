# opentelemetry_exporter

![Common Test](https://github.com/opentelemetry-beam/opentelemetry_exporter/workflows/Common%20Test/badge.svg) 

The OpenTelemetry Protocol exporter for use with the [OpenTelemetry Collector](https://github.com/open-telemetry/opentelemetry-collector).

Currently only supports the Tracer protocol using either GRPC or Protobuffers over HTTP1.1. Metrics are to come soon.

## Configuration

### Options to Batch Processor

Easiest way to setup is to add configuration for the batch processor in OpenTelemetry application environment.

For an Erlang release in `sys.config`:

``` erlang
{opentelemetry,
  [{processors, 
    [{otel_batch_processor,
        #{exporter => {opentelemetry_exporter, #{endpoints =>
        ["http://localhost:9090"],
            headers => [{"x-honeycomb-dataset", "experiments"}]}}}}]}]}
```

The default protocol is `http_protobuf`, to override this and use grpc add `protocol` to the config map:

``` erlang
{opentelemetry,
  [{processors, 
    [{otel_batch_processor,
        #{exporter => {opentelemetry_exporter, #{protocol => grpc,
                                                 endpoints => ["http://localhost:9090"],
                                                 headers => [{"x-honeycomb-dataset", "experiments"}]}}}}]}]}
```

An Elixir release uses `releases.exs`:

``` elixir
config :opentelemetry, :processors,
  otel_batch_processor: %{
    exporter: {:opentelemetry_exporter, %{endpoints: ["http://localhost:9090"],
                                          headers: [{"x-honeycomb-dataset", "experiments"}]}}
  }
```

### Application Environment

- `otlp_endpoint`: The URL to send traces and metrics to, for traces the path `v1/traces` is appended to the path in the URL.
- `otlp_traces_endpoint`: URL to send only traces to. This takes precedence for exporting traces and the path of the URL is kept as is, no suffix is appended.
- `otlp_headers`: List of additional headers (`[{unicode:chardata(), unicode:chardata()}]`) to add to export requests.
- `otlp_traces_headers`: Additional headers (`[{unicode:chardata(), unicode:chardata()}]`) to add to only trace export requests.
    
    
``` erlang
{opentelemetry_exporter,
  [{otlp_endpoint, "https://api.honeycomb.io:443"},
   {otlp_headers, [{"x-honeycomb-dataset", "experiments"}]}]}
```

An Elixir release uses `releases.exs`:

``` elixir
config :opentelemetry_exporter,
  otlp_endpoint: "https://api.honeycomb.io:443",
  otlp_headers: [{"x-honeycomb-dataset", "experiments"}]
```
    
    
The default SSL options for HTTPS requests are set using
[tls_certificate_check](https://hex.pm/packages/tls_certificate_check). This
package also provides the [CA certificates from Mozilla](https://curl.se/docs/caextract.html).

The user can override these options either as part of the endpoint or for all
endpoints used by the exporter with the Application environment variable
`ssl_options`

See [secure coding with
inets](https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/inets)
for more information on securing HTTP requests in Erlang.

### OS Environment

- `OTEL_EXPORTER_OTLP_ENDPOINT`: The URL to send traces and metrics to, for traces the path `v1/traces` is appended to the path in the URL.
- `OTEL_EXPORTER_OTLP_TRACES_ENDPOINT`: URL to send only traces to. This takes precedence for exporting traces and the path of the URL is kept as is, no suffix is appended.
- `OTEL_EXPORTER_OTLP_HEADERS`: List of additional headers to add to export requests.
- `OTEL_EXPORTER_OTLP_TRACES_HEADERS`: Additional headers to add to only trace export requests.

Example usage of setting the environment variables:
```
OTEL_EXPORTER_OTLP_TRACES_ENDPOINT=https://api.honeycomb.io:443
OTEL_EXPORTER_OTLP_PROTOCOL=grpc
OTEL_EXPORTER_OTLP_TRACES_HEADERS=x-honeycomb-team=<HONEYCOMB API TOKEN>,x-honeycomb-dataset=experiments
```

## Contributing

This project uses a submodule during development, it is not needed if the application is being used as a dependency, so be sure to clone with the option `recurse-submodules`:

``` shell
$ git clone --recurse-submodules https://github.com/opentelemetry-beam/opentelemetry_exporter
```

### Upgrading OpenTelemetry Protos

The protos are in a separate repository, [opentelemetry-proto](https://github.com/open-telemetry/opentelemetry-proto/), and used as a submodule in this repo. To update the Erlang protobuf modules and GRPC client first update the submodule and then use the [rebar3 grpcbox plugin](https://github.com/tsloughter/grpcbox_plugin/) to generate the client:

``` shell
$ git submodule update --remote opentelemetry-proto
$ rebar3 grpc gen -t client
===> Writing src/trace_service_pb.erl
===> Writing src/opentelemetry_proto_collector_trace_v_1_trace_service_client.erl (forcibly overwriting)
$ mv src/opentelemetry_proto_collector_trace_v_1_trace_service_client.erl src/opentelemetry_trace_service.erl
```

Then open `src/opentelemetry_trace_service.erl` and fix the module name.

