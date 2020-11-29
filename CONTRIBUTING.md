# Contributing

## Using Git for dependencies

Since packages are in this single GitHub repository, it requires additional options to use dependencies via Git.

`rebar.config`:

```erlang
{opentelemetry_api, {git_subdir,
"http://github.com/open-telemetry/opentelemetry-erlang", {branch, "main"}, "apps/opentelemetry_api"}}
{opentelemetry, {git_subdir,
"http://github.com/open-telemetry/opentelemetry-erlang", {branch, "main"}, "apps/opentelemetry"}}
```

Note that `git_subdir` requires Rebar 3.14 or above.

`mix.exs`:

```elixir
defp deps() do
  [
    {:opentelemetry_api,
     github: "open-telemetry/opentelemetry-erlang",
     ref: "main",
     sparse: "apps/opentelemetry_api",
     override: true},
    {:opentelemetry,
     github: "open-telemetry/opentelemetry-erlang",
     ref: "main",
     sparse: "apps/opentelemetry",
     override: true}
  ]
end
```

The `override: true` is required to override the Hex dependencies.

## Testing

### W3C Trace Context Interop Tests

Start the interop web server in a shell:

```console
$ rebar3 as interop shell

> w3c_trace_context_interop:run().
```

Then, clone the [W3C Trace Context repo](https://github.com/w3c/trace-context) and run the tests:

```console
cd test
python3 test.py http://127.0.0.1:5000/test
```

### Benchmarks

It uses [benchee](https://github.com/bencheeorg/benchee) for benchmarking. Benchmark functions are in modules under the `samples/` directory.

To run the benchmarks with Rebar3, open a Rebar3 shell in the `bench` profile:

```console
$ rebar3 as bench shell

> otel_benchmarks:run().
```

To run the benchmarks with Elixir:

```console
ERL_LIBS=_build/bench/lib/ mix run --no-mix-exs samples/run.exs
```

## Guides

- Use [`mix format`](https://hexdocs.pm/mix/Mix.Tasks.Format.html) for Elixir source code and examples
