# Contributing to OpenTelemetry Exporter

## OpenTelemetry Proto

This project uses a Git submodule to include [OpenTelemetry proto files](https://github.com/open-telemetry/opentelemetry-proto) during developement.

To clone this repository with submodule:

```console
git clone --recurse-submodules https://github.com/opentelemetry-beam/opentelemetry_exporter
```

To fetch and pull submodules after cloning:

```console
git submodule update --init --recursive
```

To update the Erlang protobuf modules and GRPC client:

1. Update the submodule
2. Run [rebar3 grpcbox plugin](https://github.com/tsloughter/grpcbox_plugin/) to generate new file
3. Rename the generated file and fix the module name in it

```console
$ git submodule update --remote opentelemetry-proto

$ rebar3 grpc gen -t client
===> Writing src/trace_service_pb.erl
===> Writing src/opentelemetry_proto_collector_trace_v_1_trace_service_client.erl (forcibly overwriting)

$ mv src/opentelemetry_proto_collector_trace_v_1_trace_service_client.erl src/opentelemetry_trace_service.erl
```
