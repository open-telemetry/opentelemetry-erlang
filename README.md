OpenTelemetry
=====

OpenTelemetry stats collection and distributed tracing framework for Erlang.

Requires OTP 21.3 of above.

## Design

The [OpenTelemetry specification](https://github.com/open-telemetry/opentelemetry-specification) defines a language library as having 2 components, the API and the SDK. The API must not only define the interfaces of any implementation in that language but also be able to function as a minimal implementation. The SDK is the default implementation of the API that must be optional.

In this library the API is defined as the behaviours `ot_tracer`, `ot_span` and `ot_ctx`. It is not a separate Erlang application from the SDK, nor are there subdirectories following the [layout described in the spec](https://github.com/open-telemetry/opentelemetry-specification/blob/master/specification/library-layout.md). Not using subdirectories as the spec's layout describes is simply because the OTP application structure has all source files directly under `src/`. There is no separation of API and SDK into distinct libraries because a) there is not yet support in rebar3 for using git repos with multiple applications as dependencies b) since Erlang has a flat namespace the same module naming strategy would be used whether they are separate applications or not. Additionally, a release can be configured to drop any part of an application the user chooses. So in the unlikely event there are users who wish to not include the SDK's default implementation it is possible.

### Default Tracer

## Benchmarks

Running benchmarks is done with [benchee](https://github.com/bencheeorg/benchee). Benchmark functions are in modules under `samples/`. To run them open a rebar3 shell in the `bench` profile:

``` shell
$ rebar3 as bench compile

> ot_benchmarks:run().
```

If an Elixir script is wanted for the benchmarks they could be run like:

``` shell
$ ERL_LIBS=_build/bench/lib/ mix run --no-mix-exs samples/run.exs
```

## Contributing

