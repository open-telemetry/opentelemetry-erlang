# Opentelemetry Semantic Conventions

Auto-generated constants that represent the [OpenTelemetry Semantic
Conventions](https://github.com/open-telemetry/semantic-conventions).

## How to Use

For Erlang include the semantic conventions header for the particular kind you
need (`trace`, `resource`, `logs`, `metrics`):

```
-include_lib("opentelemetry_semantic_conventions/include/trace.hrl").
```

You can then use the macros for the attribute keys:

```
?DB_CONNECTION_STRING
```

### Elixir

You could also use `OpenTelemetry.SemanticConventions.Logs`, `OpenTelemetry.SemanticConventions.Resource` or
`OpenTelemetry.SemanticConventions.Trace` modules.

    iex> OpenTelemetry.SemanticConventions.Logs.event_name()
    :"event.name"
