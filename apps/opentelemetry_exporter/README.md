# opentelemetry_exporter

![Common Test](https://github.com/opentelemetry-beam/opentelemetry_exporter/workflows/Common%20Test/badge.svg) [![Gitter](https://badges.gitter.im/open-telemetry/opentelemetry-erlang.svg)](https://gitter.im/open-telemetry/opentelemetry-erlang?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

The OpenTelemetry Protocol exporter for use with the [OpenTelemetry Collector](https://github.com/open-telemetry/opentelemetry-collector).

Currently only supports the Tracer protocol using either GRPC or Protobuffers over HTTP1.1. Metrics are to come soon.

## Using

Easiest way to setup is to add configuration for the batch processor in OpenTelemetry application environment.

For an Erlang release in `sys.config`:

``` erlang
{opentelemetry,
  [{processors, 
    [{otel_batch_processor,
        #{exporter => {opentelemetry_exporter, #{endpoints => [{http, "localhost", 9090, []}]}}}}]}]}
```

The default protocol is `http_protobuf`, to override this and use grpc add `protocol` to the config map:

``` erlang
{opentelemetry,
  [{processors, 
    [{otel_batch_processor,
        #{exporter => {opentelemetry_exporter, #{protocol => grpc,
                                                 endpoints => [{http, "localhost", 9090, []}]}}}}]}]}
```

An Elixir release uses `releases.exs`:

``` elixir
config :opentelemetry, :processors,
  otel_batch_processor: %{
    exporter: {:opentelemetry_exporter, %{endpoints: [{:http, 'localhost', 9090, []}]}}
  }
```

## Contributing

This project uses a submodule during developement, it is not needed if the application is being used as a dependency, so be sure to clone with the option `recurse-submodules`:

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

