# OpenTelemetry SDK

OptenTelemetry SDK in Erlang.

[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
[![Gitter chat](https://badges.gitter.im/open-telemetry/opentelemetry-erlang.svg)](https://gitter.im/open-telemetry/opentelemetry-erlang)

[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry)](https://hex.pm/packages/opentelemetry)
[![License - Apache-2.0](https://img.shields.io/hexpm/l/opentelemetry)](LICENSE)

![Build Status - Erlang](https://github.com/open-telemetry/opentelemetry-erlang/workflows/Erlang/badge.svg)
![Build Status - Elixir](https://github.com/open-telemetry/opentelemetry-erlang/workflows/Elixir/badge.svg)

---

## Features

Implements OpenTelemetry API in Erlang.

## Installation

Add [`opentelemetry`](https://hex.pm/packages/opentelemetry) to your dependencies.

`rebar.config`:

```erlang
{deps, [{opentelemetry, "~> 0.5.0"}]}.
```

`mix.exs`:

```elixir
defp deps() do
  [
    {:opentelemetry, "~> 0.5.0"}
  ]
end
```

To run the OpenTelemetry SDK independent of other applications, add the
dependency to the project, not an individual application.

For example, in a umbrella project, add the `opentelemetry` application to the root `mix.exs` of the umbrella project, instead of `mix.exs` of an individual application.

## Configuring

The `opentelemetry` application automatically configures when included.

Followings are available configuration for `opentelemetry` application:

- `processors`
- `deny_list`
- `text_map_propagators`
- `sampler`

### Configuring Exporter

To export data, your project have to include exporter(s).

- [opentelemetry_exporter](https://hex.pm/packages/opentelemetry_exporter): OpenTelemetry Collector
- [opentelemetry_zipkin](https://hex.pm/packages/opentelemetry_zipkin): Zipkin

```elixir
config :opentelemetry, :processors,
  otel_batch_processor: %{
    exporter: {:opentelemetry_exporter, %{endpoints: [{:http, 'localhost', 9090, []}]}}
  }
```

### Configuring Release

Optionally you may add the `opentelemetry` application with `temporary` option to avoid terminating the other applications in the project when the `opentelemetry` application crashes or shutdowns.

In Erlang with relx:

```erlang
{relx, [{release, {<name>, <version>},
         [{opentelemetry, temporary},
          <your applications>]},
        ...]}.
```

In Elixir with [mix release](https://hexdocs.pm/mix/Mix.Tasks.Release.html):

```elixir
def project do
  [
    releases: [
      <name>: [
        applications: [opentelemetry: :temporary]
      ],

      ...
    ]
  ]
end
```
