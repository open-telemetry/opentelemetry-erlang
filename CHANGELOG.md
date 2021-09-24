# Changelog

## Unreleased

### API

#### Context

- Propagators must now be implementations of a propagator type's behaviour. At
  this time only the `otel_propagator_text_map` behaviour exists. Callbacks for
  inject and extract take an optional "set" and "get" function for working with
  a carrier.
- Configuration of propagators is now a list of atoms representing either the
  name of a builtin propagator (at this time those are, `trace_context`, `b3` and
  `baggage`) or the name of a module implementing the propagator's behaviour.
  - Default configuration: `{text_map_propagators, [trace_context, baggage]}`
- Injectors and extractors can be configured separately instead of using the
  same list of propagators for both by configuring `text_map_injectors` and
  `text_map_extractors`.
  - For example you may want your service to support receiving `b3multi` headers
    but have no need for it including `b3multi` headers when it is propagating to
    other services:
            
    ```
    {text_map_injectors, [trace_context, baggage]},
    {text_map_extractors, [b3multi, trace_context, baggage]}
    ```
- `b3` propagator renamed `b3multi` to properly convey it is the version of the
  B3 spec that creates multiple headers
