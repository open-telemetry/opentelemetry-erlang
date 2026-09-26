# opentelemetry_exporter

The OpenTelemetry Protocol exporter for use with the [OpenTelemetry
Collector](https://github.com/open-telemetry/opentelemetry-collector). The
version of this Application does not track the supported version of the
OpenTelemetry Protocol (OTLP). The currently used version of the [OTLP protobufs
is v0.11.0](https://github.com/open-telemetry/opentelemetry-proto/tree/v0.11.0).

Currently only supports the Tracer protocol using either GRPC or Protobuffers over HTTP1.1.

## SDK configuration

Configure the exporter under the `opentelemetry` application's
`tracer_provider`. Select the transport with the component name `otlp_http` or
`otlp_grpc`; there is no separate `protocol` option in this form.

For an Erlang release in `sys.config`:

```erlang
{opentelemetry,
 [{tracer_provider,
   #{processors =>
         [{batch,
           #{exporter =>
                 {otlp_http,
                  #{endpoint => <<"http://localhost:9090/v1/traces">>,
                    headers =>
                        [{<<"x-honeycomb-dataset">>, <<"experiments">>}]}}}}]}}]}
```

To use gRPC, select the `otlp_grpc` exporter:

```erlang
{opentelemetry,
 [{tracer_provider,
   #{processors =>
         [{simple,
           #{exporter =>
                 {otlp_grpc,
                  #{endpoint => <<"http://localhost:9090">>,
                    headers =>
                        [{<<"x-honeycomb-dataset">>, <<"experiments">>}]}}}}]}}]}
```

In Elixir, you can use `config.exs` or `runtime.exs`:

```elixir
config :opentelemetry,
  tracer_provider: %{
    processors: [
      {:batch,
       %{exporter:
           {:otlp_http,
            %{endpoint: "http://localhost:9090/v1/traces",
              headers: [{"x-honeycomb-dataset", "experiments"}]}}}}
    ]
  }
```

The alias configuration map supports:

- `endpoint`: one signal-specific endpoint. HTTP defaults to
  `http://localhost:4318/v1/traces`; gRPC defaults to
  `http://localhost:4317`.
- `headers`: header name/value pairs, for example
  `[{<<"x-access-key">>, <<"secret">>}]`.
- `headers_list`: a comma-separated string in the
  `OTEL_EXPORTER_OTLP_HEADERS` format. Explicit `headers` entries take
  precedence.
- `compression`: `gzip` or `none`.
- `tls`: a map containing `ca_file`, or both `key_file` and `cert_file` for
  client authentication. System certificate verification is used by default.
- `encoding`: `protobuf` for HTTP. JSON encoding is not implemented.

`max_request_size`, `max_response_size`, `timeout`, and gRPC `tls.insecure`
are not implemented and are rejected when set.

The SDK configuration is authoritative. `opentelemetry_exporter` application
environment and `OTEL_EXPORTER_*` variables do not override either alias.
Environment substitution in a declarative configuration document must happen
before the document reaches the Erlang SDK.

See [secure coding with
inets](https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/inets)
for more information on securing HTTP requests in Erlang.

## Direct exporter compatibility

The implementation module can still be selected directly when Erlang-specific
transport options are required:

```erlang
{simple,
 #{exporter =>
       {opentelemetry_exporter,
        #{endpoints => [<<"http://localhost:4318/v1/traces">>],
          headers => [],
          protocol => http_protobuf,
          compression => undefined,
          ssl_options => undefined}}}}
```

This map uses the implementation options `endpoints`, `headers`, `protocol`,
`compression`, and `ssl_options`. When it appears under `tracer_provider`, it
is treated as SDK configuration and is not overridden by application or OS
environment values.

Code that initializes `opentelemetry_exporter` directly, outside the SDK
configuration resolver, retains the older merge behavior. Such callers may
provide those implementation options to `opentelemetry_exporter:init/1`, use
the `opentelemetry_exporter` application environment keys `otlp_endpoint`,
`otlp_traces_endpoint`, `otlp_headers`, `otlp_traces_headers`,
`otlp_protocol`, `otlp_traces_protocol`, `otlp_compression`,
`otlp_traces_compression`, and `ssl_options`, or use their corresponding
`OTEL_EXPORTER_*` environment variables. OS environment values take precedence
in that direct-initialization compatibility path.

## Upgrading OpenTelemetry Protos

The protos are in a separate repository,
[opentelemetry-proto](https://github.com/open-telemetry/opentelemetry-proto/),
and used as a submodule in this repo. To update the Erlang protobuf modules and
GRPC client first update the submodule and then use the [rebar3 grpcbox
plugin](https://github.com/tsloughter/grpcbox_plugin/) to generate the client:

```shell
$ pushd apps/opentelemetry_exporter/opentelemetry-proto
$ git fetch origin
$ git checkout <tag>
$ popd

# bug in grpcbox plugin means we need to delete _pb files first to regenerate them
$ rm ./apps/opentelemetry_exporter/src/opentelemetry_exporter_trace_service_pb.erl  ./apps/opentelemetry_exporter/src/opentelemetry_exporter_metrics_service_pb.erl ./apps/opentelemetry_exporter/src/opentelemetry_exporter_logs_service_pb.erl

$ rebar3 grpc gen -t client
...
$ mv apps/opentelemetry_exporter/src/opentelemetry_proto_collector_trace_v_1_trace_service_client.erl apps/opentelemetry_exporter/src/opentelemetry_trace_service.erl
$ mv apps/opentelemetry_exporter/src/opentelemetry_proto_collector_logs_v_1_logs_service_client.erl apps/opentelemetry_exporter/src/opentelemetry_logs_service.erl
$ mv apps/opentelemetry_exporter/src/opentelemetry_proto_collector_metrics_v_1_metrics_service_client.erl apps/opentelemetry_exporter/src/opentelemetry_metrics_service.erl
```

Then open each moved module and fix the module name.
