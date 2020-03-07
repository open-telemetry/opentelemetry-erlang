# opentelemetry_exporter

The OpenTelemetry Protocol exporter.

## Upgrading OpenTelemetry Protos

The protos are in a separate repository, [opentelemetry-proto](https://github.com/open-telemetry/opentelemetry-proto/), and used as a submodule in this repo. To update the Erlang protobuf modules and GRPC client first update the submodule and then use the [rebar3 grpcbox plugin](https://github.com/tsloughter/grpcbox_plugin/) to generate the client:

``` shell
$ git submodule update --remote opentelemetry-proto
$ rebar3 grpc gen -t client
===> Writing src/trace_service_pb.erl
===> Writing src/opentelemetry_proto_collector_trace_v_1_trace_service_client.erl (forcibly overwriting)
$ mv src/opentelemetry_proto_collector_trace_v_1_trace_service_client.erl src/opentelemetry_trace_service.erl
```

