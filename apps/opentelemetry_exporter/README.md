# OpenTelemetry Exporter

OpenTelemetry Protocol Exporter in Erlang.

[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
[![Gitter chat](https://badges.gitter.im/open-telemetry/opentelemetry-erlang.svg)](https://gitter.im/open-telemetry/opentelemetry-erlang)

[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry_exporter)](https://hex.pm/packages/opentelemetry_exporter)
[![License - Apache-2.0](https://img.shields.io/hexpm/l/opentelemetry_exporter)](LICENSE)

![Build Status - Erlang](https://github.com/open-telemetry/opentelemetry-erlang/workflows/Erlang/badge.svg)
![Build Status - Elixir](https://github.com/open-telemetry/opentelemetry-erlang/workflows/Elixir/badge.svg)

---

## Features

 OpenTelemetry Protocol exporter supports following features for use with the [OpenTelemetry Collector](https://github.com/open-telemetry/opentelemetry-collector).

Supported protocols:

- [OTLP/gRPC](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.0.1/specification/protocol/otlp.md#otlpgrpc)
- [OTLP/HTTP](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.0.1/specification/protocol/otlp.md#otlphttp) with binary-encoded Protobuf

Supported services:

- Trace service

## Installation

Add [`opentelemetry_exporter`](https://hex.pm/packages/opentelemetry_exporter) to your dependencies.

`rebar.config`:

```erlang
{deps, [{opentelemetry_exporter, "~> 0.5.0"}]}.
```

`mix.exs`:

```elixir
defp deps() do
  [
    {:opentelemetry_exporter, "~> 0.5.0"}
  ]
end
```

## Usage

Add `opentelemetry-exporter` as dependency to get started.

Add configuration for the batch processor in `opentelemetry` application environment.

Erlang with `sys.config`:

```erlang
{opentelemetry,
  [{processors,
    [{otel_batch_processor,
        #{exporter => {opentelemetry_exporter, #{endpoints => [{http, "localhost", 9090, []}]}}}}]}]}
```

Elixir with `runtime.exs`:

```elixir
config :opentelemetry, :processors,
  otel_batch_processor: %{
    exporter: {:opentelemetry_exporter, %{endpoints: [{:http, 'localhost', 9090, []}]}}
  }
```

By default it uses OTLP/HTTP with binary-encoded Protobuf (`http_protobuf`). To use OTLP/gRPC set `protocol` to `grpc` in the config map:

```erlang
{opentelemetry,
  [{processors,
    [{otel_batch_processor,
        #{exporter => {opentelemetry_exporter, #{protocol => grpc,
                                                 endpoints => [{http, "localhost", 9090, []}]}}}}]}]}
```

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).
