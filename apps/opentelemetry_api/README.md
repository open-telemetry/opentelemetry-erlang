# OpenTelemetry API

OpenTelemetry API in Erlang.

[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
[![Gitter chat](https://badges.gitter.im/open-telemetry/opentelemetry-erlang.svg)](https://gitter.im/open-telemetry/opentelemetry-erlang)

[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry_api)](https://hex.pm/packages/opentelemetry_api)
[![License - Apache-2.0](https://img.shields.io/hexpm/l/opentelemetry_api)](LICENSE)

![Build Status - Erlang](https://github.com/open-telemetry/opentelemetry-erlang/workflows/Erlang/badge.svg)
![Build Status - Elixir](https://github.com/open-telemetry/opentelemetry-erlang/workflows/Elixir/badge.svg)

---

## Features

OpenTelemetry API (`opentelemetry_api` package) is a library application to provide interfaces for instrumentation. Most interfaces are under `opentelemetry` module.

It also provides no-op implemention without starting processes, creating ETS tables, or changing the process dictionary.

## Installation

Add [`opentelemetry_api`](https://hex.pm/packages/opentelemetry_api) to your dependencies.

`rebar.config`:

```erlang
{deps, [{opentelemetry_api, "~> 0.6.0"}]}.
```

`mix.exs`:

```elixir
defp deps() do
  [
    {:opentelemetry_api, "~> 0.6.0"}
  ]
end
```

## Configuring OpenTelemetry SDK

Since OpenTelemetry API provides only no-op implementation, your project has to include the implementation of it to do the actual jobs.

`opentelemetry` package implements the OpenTelemetry SDK in Erlang. Check out the [documentation](../opentelemetry/) on how to add and configure the package.

To use own implementation, check out following functions:

- `opentelemetry:set_default_tracer/2`
- `opentelemetry:set_default_meter/2`

## Using OpenTelemetry instrumentation packages

You may use existing packages for instrumenting other packages. [Search hex with `opentelemetry_`](https://hex.pm/packages?search=opentelemetry_&sort=recent_downloads) to find such packages.

Following are examples:

- [opentelemetry_elli](https://hex.pm/packages/opentelemetry_elli): Elli
- [opentelemetry_ecto](https://hex.pm/packages/opentelemetry_ecto): Ecto
- [opentelemetry_phoenix](https://hex.pm/packages/opentelemetry_phoenix): Phoenix

Check out their documentation how to use them.

## Tracing

### Registring a tracer

You can register a tracer manually with `register_tracer/2`.

Since registering a tracer with OTP application name and version is recommended, this library provides a function for "application tracer" which is a tracer with OTP application name and version.

To register an application tracer when the application starts, call `register_application_tracer/1` in `start/2`.

Erlang:

```erlang
start(_StartType, _StartArgs) ->
    _ = opentelemetry:register_application_tracer(my_app),
...
```

Elixir:

```elixir
defmodule MyApp.Application do
  use Application

  @impl Application
  def start(_type, _args) do
    OpenTelemetry.register_application_tracer(:my_app)
  end
end
```

Instead of using `start/2`, you may register the tracer in a function that is called once before the library would create any spans.

For example, the [Elli](https://github.com/elli-lib/elli) middleware for OpenTelemetry instrumentation registers the `Tracer` during Elli startup:

```erlang
handle_event(elli_startup, _Args, _Config) ->
    _ = opentelemetry:register_application_tracer(opentelemetry_elli),
    ok;
```

If there is no startup of any kind to hook into in the library itself, it is recommended to export a function `register_application_tracer/0` to be used by any application that depends on it to do the registration:

```erlang
-module(my_lib).

-export([register_tracer/0]).

register_tracer() ->
    _ = opentelemetry:register_application_tracer(my_lib),
    ok.
```

Please note that using a tracer without registration would not cause any issues, since OpenTelemetry simply will fallback to the default tracer in such cases.

### Creating a span

To create a span, get the tracer with `get_tracer/1` and pass it to other functions for spans.

Since a tracer value can be passed by function calls across process boundaries, it is safe to get the tracer once, store it (e.g. in a `gen_server`), and pass it to other functions, instead of getting the tracer every time.

```erlang
Tracer = opentelemetry:get_tracer(my_app).

% take a function
otel_tracer:with_span(Tracer, <<"span-1">>, fun() -> ... end).

% or start and end span manually
otel_tracer.start_span(Tracer, <<"span-1">>).
% ...
otel_tracer.end_span(Tracer)
```

When an application tracer is registered with `register_application_tracer/1`, you may use Erlang or Elixir macros that look up the tracer with the current module's name and make use of the current module's name to lookup the tracer for you and use it, instead of passing the tracer around.

```erlang
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

some_fun() ->
    ?with_span(<<"some_fun/0">>, #{},
        fun(_SpanCtx) ->
            ...
            ?set_attribute(<<"key">>, <<"value">>),
            ...
        end),
```

```elixir
defmodule MyApp do
  require OpenTelemetry.Tracer
  require OpenTelemetry.Span

  def ping do
    OpenTelemetry.Tracer.with_span "span-1" do
      OpenTelemetry.Span.set_attribute("key", "value")
      # ...
    end

    OpenTelemetry.Tracer.start_span("span-1")
    # ...
    OpenTelemetry.Tracer.end_span()
  end
end
```

## Writing OpenTelemetry Library

Third-party libraries or frameworks SHOULD depend only on the OpenTelemetry API, so that the user of the libraries or frameworks can choose any implementation.

Please use the following conventions:

- Use `opentelemetry_<app>` for the package name and tracer name (e.g. `opentelemetry_foo` to provide OpenTelemetry integration of `foo` package)
- Use the version of instrumenting package (e.g. version of `opentelemetry_foo` package), not the version of instrumented package (e.g. version of `foo` package)'s version for the tracer version

Note that registration is done through a single process and uses a [persistent_term](https://erlang.org/doc/man/persistent_term.html), thus updating the version on a release upgrade is possible but will involve the performance penalty of updating a persistent term.
