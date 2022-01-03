# Erlang/Elixir OpenTelemetry API

[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry)](https://hex.pm/packages/opentelemetry_api)

This is the API portion of [OpenTelemetry](https://opentelemetry.io/) for Erlang
and Elixir Applications, implementing the API portion of [the specification](https://github.com/open-telemetry/opentelemetry-specification).

This is a library, it does not start any processes, and should be the only OpenTelemetry dependency of Erlang/Elixir Applications. 

The end user of your Application can then choose to include the [OpenTelemetry implementation](https://github.com/open-telemetry/opentelemetry-erlang) Application. If the implementation Application is not in the final release the OpenTelemetry instrumentation will all be no-ops. This means no processes started, no ETS tables created and nothing added to the process dictionary.

This separation is done so you should feel comfortable instrumenting your Erlang/Elixir Application with OpenTelemetry and not worry that a complicated dependency is being forced on your users.

## Use

When instrumenting an Application to be used as a dependency of other projects it is best practice to register a `Tracer` with a name and a version using the Application's name and version. This should be the name and version of the Application that has the `opentelemetry` calls being written in it, not the name of the Application it might be being used to instrument. For example, an [Elli](https://github.com/elli-lib/elli) middleware to add tracing to the Elli HTTP server would *not* be named `elli`, it would be the name of the middleware Application, like `opentelemetry_elli`.

Registration is done through a single process and uses a [persistent_term](https://erlang.org/doc/man/persistent_term.html), so should be done only once per-Application. Updating a registration is allowed, so updating the version on a release upgrade can, and should, be done, but will involve the performance penalty of updating a [persistent_term](https://erlang.org/doc/man/persistent_term.html).

Naming the `Tracers` provides additional metadata on spans and allows the user of your Application to disable the traces from the dependency if it is needed.

### Dependency in Elixir

``` elixir
def deps do
  [
    {:opentelemetry_api, "~> 1.0"}
  ]
end
```

### Helper Macros for Application Tracers

There are both Erlang and Elixir macros that make use of the current module's
name to lookup a Named Tracer -- a Named Tracer is created for each Application
loaded in the system at start time -- for you and can be used for Trace and Span
operations:

``` erlang
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

some_fun() ->
    ?with_span(<<"some_fun/0">>, #{}, 
        fun(_SpanCtx) -> 
            ...
            ?set_attribute(<<"key">>, <<"value">>),
            ...
        end),
```

``` elixir
require OpenTelemetry.Tracer
require OpenTelemetry.Span
      
def some_fun() do
    OpenTelemetry.Tracer.with_span "some-span" do
      ...
      OpenTelemetry.Span.set_attribute("key", "value")
      ...
    end
end
```

### Including the OpenTelemetry SDK

For traces to actually be tracked, propagated and exported, the [opentelemetry](https://github.com/open-telemetry/opentelemetry-erlang) Application must be included as a dependency of your project, likely as part of a [Release](https://erlang.org/doc/design_principles/release_structure.html) and not as a dependency of an individual Application within the Release.

See the [Using section](https://github.com/open-telemetry/opentelemetry-erlang#using) of the [OpenTelemetry-Erlang](https://github.com/open-telemetry/opentelemetry-erlang) repository for details.

### Exporters

Exporters can be found as separate Applications on Github under the [OpenTelemetry Beam Organization](https://github.com/opentelemetry-beam).

- [Zipkin](https://hex.pm/packages/opentelemetry_zipkin)
- [OpenTelemetry Collector](https://hex.pm/packages/opentelemetry_exporter)

### HTTP Integrations

- [Elli](https://hex.pm/packages/opentelemetry_elli)

### Database Client Integration

- [Ecto](https://hex.pm/packages/opentelemetry_ecto)

## Contributing

See the [contributing file](CONTRIBUTING.md).
