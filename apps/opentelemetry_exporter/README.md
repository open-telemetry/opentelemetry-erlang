# opentelemetry_exporter

The OpenTelemetry Protocol exporter for use with the [OpenTelemetry
Collector](https://github.com/open-telemetry/opentelemetry-collector). The
version of this Application does not track the supported version of the
OpenTelemetry Protocol (OTLP). The currently used version of the [OTLP protobufs
is v0.11.0](https://github.com/open-telemetry/opentelemetry-proto/tree/v0.11.0).

Currently only supports the Tracer protocol using either GRPC or Protobuffers over HTTP1.1.

## Configuration

By default the exporter will use HTTP to export protobuf encoded Spans to
`http://localhost:4318/v1/traces`. 

Available configuration keys:

- `otlp_endpoint`: The URL to send traces and metrics to, for traces the path `v1/traces` is appended to the path in the URL.
- `otlp_traces_endpoint`: URL to send only traces to. This takes precedence for exporting traces and the path of the URL is kept as is, no suffix is appended.
- `otlp_headers`: List of additional headers (`[{unicode:chardata(), unicode:chardata()}]`) to add to export requests.
- `otlp_traces_headers`: Additional headers (`[{unicode:chardata(),
  unicode:chardata()}]`) to add to only trace export requests.
- `otlp_protocol`: The transport protocol, supported values: `grpc` and `http_protobuf`. Defaults to `http_protobuf`.
- `otlp_traces_protocol`: The transport protocol to use for exporting traces, supported values: `grpc` and `http_protobuf`. Defaults to `http_protobuf`.
- `otlp_compression`: Compression type to use, supported values: `gzip`. Defaults to no compression.
- `otlp_traces_compression`: Compression type to use for exporting traces, supported values: `gzip`. Defaults to no compression.

``` erlang
{opentelemetry_exporter,
  [{otlp_protocol, grpc},
   {otlp_compression, gzip},
   {otlp_endpoint, "https://api.honeycomb.io:443"},
   {otlp_headers, [{"x-honeycomb-dataset", "experiments"}]}]}
```

An Elixir release uses `releases.exs`:

``` elixir
config :opentelemetry_exporter,
  otlp_protocol: :grpc,
  otlp_compression: :gzip,
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

Lastly, configuring the exporter can be done with OS environment variables,
which take precedence:

- `OTEL_EXPORTER_OTLP_ENDPOINT`: The URL to send traces and metrics to, for traces the path `v1/traces` is appended to the path in the URL.
- `OTEL_EXPORTER_OTLP_TRACES_ENDPOINT`: URL to send only traces to. This takes precedence for exporting traces and the path of the URL is kept as is, no suffix is appended.
- `OTEL_EXPORTER_OTLP_HEADERS`: List of additional headers to add to export requests.
- `OTEL_EXPORTER_OTLP_TRACES_HEADERS`: Additional headers to add to only trace export requests.
- `OTEL_EXPORTER_OTLP_PROTOCOL`: The transport protocol to use, supported values: `grpc` and `http_protobuf`. Defaults to `http_protobuf`
- `OTEL_EXPORTER_OTLP_TRACES_PROTOCOL`: The transport protocol to use for exporting traces, supported values: `grpc` and `http_protobuf`. Defaults to `http_protobuf`.
- `OTEL_EXPORTER_OTLP_COMPRESSION`: Compression to use, supported value: gzip. Defaults to no compression.
- `OTEL_EXPORTER_OTLP_TRACES_COMPRESSION`: Compression to use when exporting traces, supported value: gzip. Defaults to no compression.

Example usage of setting the environment variables:
```
OTEL_EXPORTER_OTLP_TRACES_ENDPOINT=https://api.honeycomb.io:443
OTEL_EXPORTER_OTLP_TRACES_PROTOCOL=grpc
OTEL_EXPORTER_OTLP_TRACES_COMPRESSION=gzip
OTEL_EXPORTER_OTLP_TRACES_HEADERS=x-honeycomb-team=<HONEYCOMB API TOKEN>,x-honeycomb-dataset=experiments
```

### Options to span processor

In addition to using the environment variables the exporter accepts a map of
arguments. The argument to the exporter's `init` function can be configured as
part of the Span Processor (simple or batch) in the OpenTelemetry application
environment.

For an Erlang release in `sys.config`:

``` erlang
{opentelemetry,
  [{processors,
    [{otel_batch_processor,
        #{exporter => {opentelemetry_exporter, #{endpoints =>
        ["http://localhost:9090"],
            headers => [{"x-honeycomb-dataset", "experiments"}]}}}}]}]}
```

The default protocol is `http_protobuf`, to override this and use grpc add
`protocol` to the config map:

``` erlang
{opentelemetry,
  [{processors,
    [{otel_simple_processor,
        #{exporter => {opentelemetry_exporter, #{protocol => grpc,
                                                 endpoints => ["http://localhost:9090"],
                                                 headers => [{"x-honeycomb-dataset", "experiments"}]}}}}]}]}
```

In Elixir, you can use `config.exs` or `runtime.exs`:

``` elixir
config :opentelemetry, :processors,
  otel_batch_processor: %{
    exporter: {:opentelemetry_exporter, %{endpoints: ["http://localhost:9090"],
                                          headers: [{"x-honeycomb-dataset", "experiments"}]}}
  }
```

#### The configuration map

The second element of the configuration tuple is a configuration map. It can contain the following keys:

- `protocol` - one of: `http_protobuf`, `grpc` or `http_json`. Defaults to `http_protobuf`. `http_json` is not implemented yet.
- `endpoints` - A list of endpoints to send traces to. Can take one of the forms described below. By default, exporter sends data to `http://localhost:4318`.
- `headers` - a list of headers to send to the collector (i.e `[{<<"x-access-key">> <<"secret">>}]`). Defaults to an empty list.
- `compression` - an atom. Setting it to `gzip` enables gzip compression.
- `ssl_options` - a list of SSL options. See Erlang's [SSL docs](https://www.erlang.org/doc/man/ssl.html#TLS/DTLS%20OPTION%20DESCRIPTIONS%20-%20CLIENT) for what options are available.

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
