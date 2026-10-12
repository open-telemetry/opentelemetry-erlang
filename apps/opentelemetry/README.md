# Erlang/Elixir OpenTelemetry OTP integration

[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry?label=OTP%20Integration&style=for-the-badge)](https://hex.pm/packages/opentelemetry)

This application integrates the OpenTelemetry SDK with an OTP release. After
its `opentelemetry_sdk` dependency has started the configured SDK and global
tracer provider, this application optionally creates tracers for applications
already loaded in the release.

The SDK components and OTLP exporter live in the `opentelemetry_sdk`
application. Starting `opentelemetry_sdk` by itself does not automatically
create per-application tracer mappings. Applications that do not need those
mappings can depend directly on the SDK; releases that want them should include
this `opentelemetry` application.

SDK configuration belongs to the `opentelemetry_sdk` application environment.
See the
[`opentelemetry_sdk` README](https://hexdocs.pm/opentelemetry_sdk/readme.html)
for the complete configuration model and component documentation.

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

The `opentelemetry_sdk` dependency and its exporter dependencies, including the
configured global provider, are started before this application's callback by
OTP.

## Contributing

Read the OpenTelemetry project
[contributing guide](https://github.com/open-telemetry/community/blob/main/CONTRIBUTING.md)
for general information about the project.
