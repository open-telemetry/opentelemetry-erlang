OpenTelemetry
=====

OpenTelemetry stats collection and distributed tracing framework for Erlang.

Requires OTP 21.3 of above.

## Design

The [OpenTelemetry specification](https://github.com/open-telemetry/opentelemetry-specification) defines a language library as having 2 components, the API and the SDK. The API must not only define the interfaces of any implementation in that language but also be able to function as a noop implementation of the tracer. The SDK is the default implementation of the API that must be optional.

If you are instrumenting a project your application should only depend on the [OpenTelemetry API](https://github.com/open-telemetry/opentelemetry-erlang-api/) application. 

This repository is the Erlang's SDK implementation and should be included in the final release and configured to setup the sampler, span processors and span exporters.

## Using

In an Erlang project add `opentelemetry` as the first element of the release's applications:

``` erlang
{relx, [{release, {<name>, <version>}, 
         [{opentelemetry, temporary},
          <your applications>]},
        ...]}.
```

In the above example `opentelemetry` is set to `temporary` so that if the `opentelemetry` application crashes, or is shutdown, it does not terminate the other applications in the project. This is optional, the `opentelemetry` application purposely sticks to `permanent` for the processes started by the root supervisor to leave it up to the end user whether they want the crash or shutdown or `opentelemetry` to be ignored or cause the shutdown of the rest of the applications in the release.

Doing the same for an Elixir project would be:

``` elixir
def project do
  [
    ...
    releases: [
      <name>: [
        applications: [opentelemetry: :temporary]
      ],

      ...
    ]
  ]
end
```

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

