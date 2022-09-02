# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### SDK

- Fix dependency on API to require `~> 1.1`

### Exporter

- Fix dependency on API and SDK to require `~> 1.1`

## API 1.1.0 - 2020-8-31

#### Added

- [Span context now set in logger metadata when context is updated in process
  dictionary](https://github.com/open-telemetry/opentelemetry-erlang/pull/394)
- [Instrumentation Scope replaces Instrumentation
  Library](https://github.com/open-telemetry/opentelemetry-erlang/pull/405) --
  If you were using the record directly, please use the function
  `opentelemetry:instrumentation_scope/3` or
  `opentelemetry:instrumentation_library/3` to create an `instrumentation_scope`
  record. 

## SDK 1.1.0 - 2020-8-31

#### Added

- [Instrumentation Scope replaces Instrumentation
  Library](https://github.com/open-telemetry/opentelemetry-erlang/pull/405) --
  If you were using the record directly, please use the function
  `opentelemetry:instrumentation_scope/3` or
  `opentelemetry:instrumentation_library/3` to create an `instrumentation_scope`
  record.

### Fixed

- [Allow custom text propagator to be configured via application
  env](https://github.com/open-telemetry/opentelemetry-erlang/pull/408)
- [No longer grow export table in batch processor if no export table is
  configured](https://github.com/open-telemetry/opentelemetry-erlang/pull/413)

## Exporter 1.1.0 - 2020-8-31

#### Added

- [Instrumentation Scope replaces Instrumentation
  Library](https://github.com/open-telemetry/opentelemetry-erlang/pull/405) --
  If you were using the record directly, please use the function
  `opentelemetry:instrumentation_scope/3` or
  `opentelemetry:instrumentation_library/3` to create an `instrumentation_scope`
  record.

## Experimental API/SDK

#### Added

- [Initial work to support the stable spec for the Metrics API and
  SDK](https://github.com/open-telemetry/opentelemetry-erlang/pull/412)

## SDK 1.0.5 - 2022-05-20

### Fixed

- [span processor config: don't override user settings with
  defaults](https://github.com/open-telemetry/opentelemetry-erlang/pull/397)

## SDK 1.0.4 - 2022-05-13

### Fixed

- [Setting the exporter with `traces_exporter` application environment variable
  now properly overrides the configuration passed to the
  processor](https://github.com/open-telemetry/opentelemetry-erlang/pull/393)

## Exporter 1.0.4 - 2022-05-06

- [fix bug where port 80 is used even for https](https://github.com/open-telemetry/opentelemetry-erlang/pull/389)

## 1.0.3 - 2022-04-27

### [API 1.0.3]

- Doc fixes and improvements to `tracer.ex` and `otel_propagator_text_map.erl`

### [SDK 1.0.3]

- [Improve performance of Span set_status](https://github.com/open-telemetry/opentelemetry-erlang/pull/384)

### [Exporter 1.0.3]

- [Fix use of `otlp_endpoint` configuration from Elixir](https://github.com/open-telemetry/opentelemetry-erlang/pull/376)
- [Remove the SDK application `opentelemetry` from the Exporter's runtime dependencies](https://github.com/open-telemetry/opentelemetry-erlang/pull/387)

## 1.0.2 - 2022-02-22

### [API 1.0.2]

#### Added

- [Docs for Erlang and Elixir macros added](https://github.com/open-telemetry/opentelemetry-erlang/pull/362)

### [SDK 1.0.2]

#### Added

- [Simpler configuration of span processors](https://github.com/open-telemetry/opentelemetry-erlang/pull/357)

#### Fixed

- Span Status: Ignore status changes that don't follow the [define precedence in
  the spec](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/trace/api.md#set-status)

### [Zipkin Exporter 1.0.0]

#### Fixed

- [Attribute values that are lists are converted to strings in Zipkin tags](https://github.com/open-telemetry/opentelemetry-erlang/pull/363)
- [Status converted to Zipkin tags](https://github.com/open-telemetry/opentelemetry-erlang/pull/363)

## 1.0.1 - 2022-02-03

### [API]

#### Fixed

- [W3C Tracestate: Keep last value for duplicate key](https://github.com/open-telemetry/opentelemetry-erlang/pull/346)
- [add report_cb to format log messages in composite propagator to have more
  detailed logs](https://github.com/open-telemetry/opentelemetry-erlang/pull/355)
- [Fix use of wrong timestamp format on events](https://github.com/open-telemetry/opentelemetry-erlang/pull/354)

#### Fixed

### [SDK]

- [Fix use of wrong timestamp format on events](https://github.com/open-telemetry/opentelemetry-erlang/pull/354)

### [Exporter]

#### Fixed

- [use the default port of the scheme (http/https) instead of default OTLP
  port](https://github.com/open-telemetry/opentelemetry-erlang/pull/354)

## 1.0.0 - 2022-01-03

### [SDK]

#### Fixed

- [SDK will continue to try initializing exporter if it fails to resolve startup
  ordering issues](https://github.com/open-telemetry/opentelemetry-erlang/pull/338)
- [elixir span docs: fix reference to attributes
  type](https://github.com/open-telemetry/opentelemetry-erlang/pull/336)
  
### [API]

#### Fixed

- [events: accept map for attributes on an event](https://github.com/open-telemetry/opentelemetry-erlang/pull/335)

### [Exporter]

#### Added

- New `opentelemetry_exporter` application environment options:
  - `otlp_protocol`: The transport protocol, supported values: `grpc` and `http_protobuf`. Defaults to `http_protobuf`.
  - `otlp_traces_protocol`: The transport protocol to use for exporting traces, supported values: `grpc` and `http_protobuf`. Defaults to `http_protobuf`.
  - `otlp_compression`: Compression type to use, supported values: `gzip`. Defaults to no compression.
  - `otlp_traces_compression`: Compression type to use for exporting traces, supported values: `gzip`. Defaults to no compression.

- New environment variable options:
  - `OTEL_EXPORTER_OTLP_PROTOCOL`: The transport protocol to use, supported values: `grpc` and `http_protobuf`. Defaults to `http_protobuf`
  - `OTEL_EXPORTER_OTLP_TRACES_PROTOCOL`: The transport protocol to use for exporting traces, supported values: `grpc` and `http_protobuf`. Defaults to `http_protobuf`.
  - `OTEL_EXPORTER_OTLP_COMPRESSION`: Compression to use, supported value: gzip. Defaults to no compression.
  - `OTEL_EXPORTER_OTLP_TRACES_COMPRESSION`: Compression to use when exporting traces, supported value: gzip. Defaults to no compression.

## [API 1.0.0-rc.4.1] - 2021-12-28

##### Fixed

- `add_event` function and macros fixed to accept both a map of attributes or a
  list.
  
## [API 1.0.0-rc.4] - 2021-12-25

### Added

- Looking up a Tracer for a module is now done by first looking up the
  OTP Application name and then the Tracer using that name. This means all
  Tracers share the same "namespace" again which saves space by not duplicating
  the Tracer record for every module.
- Configurable limits for the number of Attributes, Events and Links allowed in
  a Span, an Event or a Link -- defaults to 128. The length of each Attribute's
  value can also be limited but has a default of infinity.
  
  Environment variables added to configure the limits:
  
  - `OTEL_SPAN_ATTRIBUTE_COUNT_LIMIT`: Limit on number of Attributes on a Span.
  - `OTEL_SPAN_ATTRIBUTE_VALUE_LENGTH_LIMIT`: Limit length of Attribute values.
  - `OTEL_SPAN_EVENT_COUNT_LIMIT`: Limit number of Events on a Span.
  - `OTEL_SPAN_LINK_COUNT_LIMIT`: Limit number of Links on a Span.
  - `OTEL_EVENT_ATTRIBUTE_COUNT_LIMIT`: Limit on number of Attributes on an Event.
  - `OTEL_LINK_ATTRIBUTE_COUNT_LIMIT`: Limit on number of Attributes on a Link.
- Support for a Schema URL in a Resource and in the Instrumentation Library
  information of a Tracer. For more information about Schema URLs see the
  specification for [Resources](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/resource/sdk.md)
  and [getting a Tracer](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.8.0/specification/trace/api.md).
- `OTEL_CREATE_APPLICATION_TRACERS` is a new environment variable,
  `create_application_tracers` is the application environment key, for disabling
  the automatic creation of Tracers for every Application at boot. The old keys,
  `OTEL_REGISTER_LOADED_APPLICATIONS` and `register_loaded_applications`, will
  continue to work as well.

### Fixed

- Attribute values now validate against what is allowable per the specification
  rather than allowing anything the protobuf could encode. This may be breaking
  to some users who were relying on the incorrect behavior, such as allowing
  dictionaries or non-homogenous lists/tuples. The one exception we have
  kept is continuing to allow atoms in place of binaries for performance.
- Attribute values of type list/tuple must be homogenous.
- Span start opts are now validated. Previously, opts underwent no validations.
- Event and link attributes are now validated. Previously only span attributes
  were validated.
- Events accept atoms for the name again.

### Removed

- The `sampler` option to `start_span` and `with_span` was removed.
- `register_tracer` has been removed and now `get_tracer` will cache the Tracer
  after creation if needed.

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
