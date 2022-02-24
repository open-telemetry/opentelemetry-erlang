# Erlang/Elixir OpenTelemetry API

[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry)](https://hex.pm/packages/opentelemetry_api)

This is the API portion of [OpenTelemetry](https://opentelemetry.io/) for Erlang
and Elixir Applications, implementing the API portion of [the specification](https://github.com/open-telemetry/opentelemetry-specification).

This is a library, it does not start any processes, and should be the only
OpenTelemetry dependency of Erlang/Elixir Applications.

## Use

There are both Erlang and Elixir macros that make use of the current module's
name to lookup a [Named
Tracer](https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/glossary.md#tracer-name--meter-name)
-- a Named Tracer is created for each Application loaded in the system at start
time -- for you and can be used for Trace and Span operations:

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
      
def some_fun() do
    OpenTelemetry.Tracer.with_span "some-span" do
      ...
      OpenTelemetry.Tracer.set_attribute("key", "value")
      ...
    end
end
```

### Tracing API

The macros and functions available for Elixir in `OpenTelemetry.Tracer` and the
Erlang macros in `otel_tracer.hrl` are the best way to work with Spans. They
will automatically use the Tracer named for the Application the module using the
macro is in. For example, the Spans created in
[opentelemetry_oban](https://hex.pm/packages/opentelemetry_oban) use the
`with_span` macro resulting in the Span being created with the
`opentelemetry_oban` named Tracer and associated with the [Instrumentation
Library](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/glossary.md#instrumentation-library)
of the same name and version of the Tracer -- the version also matches the
`opentelemetry_oban` Application version.

#### Context

[Context](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/context/context.md) is used to pass values associated with the current [execution
unit](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/glossary.md#execution-unit).
At this time the only values kept in the Context by this OpenTelemetry library
are the [Span
Context](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/trace/api.md#spancontext)
for the currently active Span and the
[Baggage](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/baggage/api.md)

When a Context variable is not an explicit argument in the API macros or
functions the Context from the [process
dictionary](https://www.erlang.org/doc/reference_manual/processes.html#process-dictionary)
is used. If no Context is found in the current process's pdict then one is
created.

#### Starting and Ending Spans

A Span represents a single operation in a Trace. It has a start and end time,
can have a single parent and one or more children. The easiest way to create
Spans is to wrap the operation you want a Span to represent in the `with_span`
macro. The macro handles getting a
[Tracer](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/trace/api.md#tracer)
associated with the OTP Application the module is in, starting the Span, setting
it as the currently active Span in the Context stored in the process dictionary
and ending the Span when the `Fun` or body of the Elixir macro finish, even if
an exception is thrown -- however, the exception is not caught, so it does not
change how user code should deal with raised exceptions. After the Span is
ended the Context in the process dictionary is reset to its value before the
newly started Span was set as the active Span. This handling of the active Span
in the process dictionary ensures proper lineage of Spans is kept when starting
and ending child Spans.

``` erlang
?with_span(SpanName, StartOpts, Fun)
```

``` elixir
OpenTelemetry.Tracer.with_span name, start_opts do
    ...
end
```

`StartOpts`/`start_opts` is a map of [Span creation options](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/trace/api.md#span-creation):

- `kind`: 
[SpanKind](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/trace/api.md#spankind)
defines the relationship between the Span, its parents, and its children in a
Trace. Possible values: `internal`, `server`, `client`, `producer` and
`consumer`. Defaults to `internal` if not specified.
- `attributes`: See
  [Attributes](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/common/common.md#attributes)
  for details about Attributes. Default is an empty list of attributes.
- `links`:  List of [Links](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/overview.md#links-between-spans) to causally related Spans from the same or a different Trace.
- `start_time`: The start time of the Span operation. Defaults to the current
  time. The option should only be set if the start of the operation described by
  the Span has already passed.

current_span_ctx(ctx)

set_current_span(span_ctx)

When using `start_span` instead of `with_span` there must be a corresponding
call to the [end Span
API](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/trace/api.md#end)
to signal that the operation described by the Span has ended. `end_span`
optionally takes a timestamp to use as the end time of the Span.

``` erlang
?end_span()
?end_span(Timestamp)
```

``` elixir
OpenTelemetry.Tracer.end_span(timestamp \\ :undefined)
```

#### Setting Attributes

[Setting
Attributes](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/trace/api.md#set-attributes)
can be done with a single key and value passed to `set_attribute` or through a
map of
[Attributes](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/common/common.md#attributes)
all at once. Setting an attribute with a key that already exists in the Span's
map of attributes will result in that key's value being overwritten.

``` erlang
?set_attribute(Key, Value)
?set_attributes(Attributes)
```

``` elixir
OpenTelemetry.Tracer.set_attribute(key, value)
OpenTelemetry.Tracer.set_attributes(attributes)
```

Be aware that there are [configurable limits](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/common/common.md#attribute-limits) on the number and size of
Attributes per Span.

#### Adding Events

[Adding
Events](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/trace/api.md#add-events)
can be done by passing the name of the event and the
[Attributes](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/common/common.md#attributes)
to associate with it or as a list of Events. Each Event in the list of Events is
a map containing the timestamp, name, and Attributes which can be created with
the function `event/2` and `event/3` in the `opentelemetry` and `OpenTelemetry`
modules.

``` erlang
?add_event(Name, Attributes)
?add_events(Events)
```

``` elixir
OpenTelemetry.Tracer.add_event(event, attributes)
OpenTelemetry.Tracer.add_events(events)
```

#### Setting the Status

[Set
Status](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/trace/api.md#set-status)
will override the default Span Status of `Unset`. A Status is a code (`ok`,
`error` or `unset`) and, only if the code is `error`, an optional message string
that describes the error.

``` erlang
?set_status(Code, Message)
```

``` elixir
OpenTelemetry.Tracer.set_status(code, message)
```

#### Update Span Name

[Updating the Span
name](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/trace/api.md#updatename)
can be done after starting the Span but must be done before the Span is end'ed.

``` erlang
?update_name(Name)
```

``` elixir
OpenTelemetry.Tracer.update_name(name)
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
(OTLP)](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/protocol/otlp.md)
and [Zipkin](https://zipkin.io/):

- [OpenTelemetry Protocol](https://hex.pm/packages/opentelemetry_exporter)
- [Zipkin](https://hex.pm/packages/opentelemetry_zipkin)

### Integrations

Instrumentations of many popular Erlang and Elixir projects can be found in the
[contrib repo](https://github.com/open-telemetry/opentelemetry-erlang-contrib)
and on [hex.pm](https://hex.pm) under the [OpenTelemetry organization](https://hex.pm/orgs/opentelemetry).

## Contributing

Read OpenTelemetry project [contributing
guide](https://github.com/open-telemetry/community/blob/main/CONTRIBUTING.md)
for general information about the project.
