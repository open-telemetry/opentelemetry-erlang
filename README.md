# Erlang/Elixir OpenTelemetry API

This is the API portion of [OpenTelemetry](https://opentelemetry.io/) for Erlang and Elixir Applications.

This is a library, it does not start any processes, and should be the only OpenTelemetry dependency of Erlang/Elixir Applications. 

The end user of your Application can then choose to include the [OpenTelemetry implementation](https://github.com/open-telemetry/opentelemetry-erlang) Application. If the implementation Application is not in the final release the OpenTelemetry instrumentation will all be no-ops. This means no processes started, no ETS tables created and nothing added to the process dictionary.

This separation is done so you should feel comfortable instrumenting your Erlang/Elixir Application with OpenTelemetry and not worry that a complicated dependency is being forced on your users.

## Use

When instrumenting an Application to be used as a dependency of other projects it is best practice to register a `Tracer` with a name and a version using the Application's name and version. This should be the name and version of the Application that has the `opentelemetry` calls being written in it, not the name of the Application it might be being used to instrument. For example, an [Elli](https://github.com/elli-lib/elli) middleware to add tracing to the Elli HTTP server would *not* be named `elli`, it would be the name of the middleware Application, like `opentelemetry_elli`.

Registration is done through a single process and uses a [persistent_term](https://erlang.org/doc/man/persistent_term.html), so should be done only once per-Application. Updating a registration is allowed, so updating the version on a release upgrade can, and should, be done, but will involve the performance penalty of updating a [persistent_term](https://erlang.org/doc/man/persistent_term.html).

Naming the `Tracers` provides additional metadata on spans and allows the user of your Application to disable the traces from the dependency if it is needed.

### Registering and Using Tracers Directly

If it is a runnable application then this registration should happen in `start/2`, example below is adding `Tracer` registration to the Postgres library [pgo](https://github.com/erleans/pgo):

``` erlang
start(_StartType, _StartArgs) ->
    _ = opentelemetry:register_application_tracer(pgo),
...
```

Or for an Elixir Application named `MyApp`:

``` elixir
defmodule MyApp do
  use Application

  def start(_type, _args) do
    _ = OpenTelemetry.register_application_tracer(MyApp),
    ...
  end
end
```

Then when the spans are started and finished in the application's code the `Tracer` is fetched with `get_tracer/1` and passed to `with_span/3` or `start_span/3`:

``` erlang
Tracer = opentelemetry:get_tracer(pgo),
ot_tracer:with_span(Tracer, <<"pgo:query/3">>, fun() -> ... end).
```

A `Tracer` variable can be passed through your Application's calls so `get_tracer` only has to be called once, it is safe to store it in the state of a `gen_server` and to pass across process boundaries.

If the application does not have a `start/2` there may be another function that is always called before the library would create any spans. For example, the [Elli](https://github.com/elli-lib/elli) middleware for OpenTelemetry instrumentation registers the `Tracer` during Elli startup:

``` erlang
handle_event(elli_startup, _Args, _Config) ->
    _ = opentelemetry:register_application_tracer(opentelemetry_elli),
    ok;
```

When there is no startup of any kind to hook into in the library itself it should export a function `register_application_tracer/0` to be used by any application that depends on it to do the registration:

``` erlang
-module(mylib).

-export([register_tracer/0]).

register_tracer() ->
    _ = opentelemetry:register_application_tracer(mylib),
    ok.
```

Not registering does not cause any issues or crashes, OpenTelemetry simply will fallback to the default `Tracer` if `get_tracer/1` is called with a name that is not registered.


### Helper Macros for Application Tracers

When `register_application_tracer/1` is used to register a Tracer there are both Erlang and Elixir macros that make use of the current module's name to lookup the Tracer for you and can be used for Trace and Span operations:

``` erlang
-include_lib("opentelemetry_api/include/tracer.hrl").

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
    OpenTelemetry.Tracer.with_span \"some-span\" do
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
