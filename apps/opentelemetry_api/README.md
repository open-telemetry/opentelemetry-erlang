# Erlang/Elixir OpenTelemetry API

[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry)](https://hex.pm/packages/opentelemetry_api)

This is the API portion of [OpenTelemetry](https://opentelemetry.io/) for Erlang
and Elixir Applications, implementing the API portion of [the specification](https://github.com/open-telemetry/opentelemetry-specification).

This is a library, it does not start any processes, and should be the only
OpenTelemetry dependency of Erlang/Elixir Applications.

## Use

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

When only the API is available at runtime a no-op Tracer is used and no Traces
are exported. The [OpenTelemetry SDK](https://hex.pm/packages/opentelemetry)
provides the functionality of Tracers, Span Processors and Exporters and should
be included as part of a
[Release](https://erlang.org/doc/design_principles/release_structure.html) and
not as a dependency of any individual Application.

### Exporters

Included in the same [Github
repo](https://github.com/open-telemetry/opentelemetry-erlang) as the API and SDK are an exporter for the [OpenTelemetry Protocol
(OTLP)](https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/protocol/otlp.md)
and [Zipkin](https://zipkin.io/):

- [OpenTelemetry Protocol](https://hex.pm/packages/opentelemetry_exporter)
- [Zipkin](https://hex.pm/packages/opentelemetry_zipkin)

### Integrations

Instrumentations of many popular Erlang and Elixir projects can be found in the
[contrib repo](https://github.com/open-telemetry/opentelemetry-erlang-contrib)
and on [hex.pm](https://hex.pm) under the [OpenTelemetry organization](https://hex.pm/orgs/opentelemetry).

## Contributing

See the [contributing file](CONTRIBUTING.md).
