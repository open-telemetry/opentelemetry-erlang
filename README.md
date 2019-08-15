OpenTelemetry
=====

OpenTelemetry stats collection and distributed tracing framework for Erlang.

Requires OTP 21.3 of above.

## Design

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

