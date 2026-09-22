# Erlang/Elixir OpenTelemetry OTP integration

[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry?label=OTP%20Integration&style=for-the-badge)](https://hex.pm/packages/opentelemetry)

This application integrates the OpenTelemetry SDK with an OTP release. At
startup it:

1. loads declarative or Erlang application configuration;
2. configures text-map propagators;
3. starts the SDK resource, span-storage, and tracer-provider supervision
   trees;
4. starts the configured global tracer provider; and
5. optionally creates tracers for applications already loaded in the release.

The SDK components and OTLP exporter live in the `opentelemetry_sdk`
application. Starting `opentelemetry_sdk` by itself does not automatically
start a provider. Applications that need custom lifecycle management can
depend directly on it; releases that want automatic setup should include this
`opentelemetry` application.

Configuration remains under the `opentelemetry` application environment
because this application owns the automatic startup policy. See the
[`opentelemetry_sdk` README](../opentelemetry_sdk/README.md) for the complete
configuration model and component documentation.

## Release setup

For rebar3/relx:

```erlang
{relx, [{release, {my_release, "0.1.0"},
         [{opentelemetry, temporary}, my_application]}]}.
```

For a Mix release:

```elixir
releases: [
  my_release: [applications: [opentelemetry: :temporary]]
]
```

The `opentelemetry_sdk` dependency and its exporter dependencies are started
before this application's callback by OTP.

## Contributing

Read the OpenTelemetry project
[contributing guide](https://github.com/open-telemetry/community/blob/main/CONTRIBUTING.md)
for general information about the project.
