# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [API 1.0.0-rc.3.2] - 2021-10-13

##### Fixed

- `otel_propagator_trace_context:extract/1` no longer crashes on `undefined`
  header values
  
## [API 1.0.0-rc.3.1] - 2021-10-12

##### Fixed

- Properly published the package with Erlang and Elixir source. Package for
  1.0.0-rc.3 was retired on Hex.pm

## [API 1.0.0-rc.3] - 2021-10-12

### Removed

- Removed `opentelemetry:register_application_tracer`. Each application has a
  Tracer registered for it automatically on boot. This can be disabled by
  setting `opentelemetry` environment variable `register_loaded_applications` to
  `false`.
- Named Tracers registered with `opentelemetry:register_tracer` are now stored
  separately from the mapping of Named Tracers created for each
  application. Meaning if you have a module `mod_a` in application `app_a` with
  application vsn `0.1.0` and also manually register a Tracer named `mod_a` with
  version `1.1.1` then use of macros like `?with_span` will use the `app_a`
  version `0.1.0` Named Tracer and manual use of a Named Tracer like:
  
  ```
  Tracer = opentelemetry:get_tracer(mod_a),
  otel_tracer:with_span(Tracer, span_name, #{}, fun() -> ... end),
  ```
  
  will use Named Tracer `mod_a` with version `1.1.1`. In previous versions after
  registering a Tracer named `mod_a` it would override the `mod_a` pointing to
  the `app_a` Tracer.
  
  Additionally, manual registration of a Named Tracer with the name `app_a` will
  not override the application registered Tracer of `app_a`.

#### Context

##### Added

- B3 single header format support added

##### Changed

- Propagators must now be implementations of a propagator type's behaviour. At
  this time only the `otel_propagator_text_map` behaviour exists. Callbacks for
  inject and extract take an optional "set" and "get" function for working with
  a carrier.
- Configuration of propagators is now a list of atoms representing either the
  name of a builtin propagator (at this time those are, `trace_context`, `b3`,
  `b3multi` and `baggage`) or the name of a module implementing the
  propagator's behaviour.
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
    
##### Fixed

- `b3` propagator renamed `b3multi` to properly convey it is the version of the
  B3 spec that creates multiple headers

## [SDK - 1.0.0-rc.3] - 2021-10-12

### Fixed

- Memory leak fix: Non-recording Spans are no longer inserted into the ETS table tracking active span.
- Ratio based root span sampling fixed, before it didn't take into account the
  generated trace id.
