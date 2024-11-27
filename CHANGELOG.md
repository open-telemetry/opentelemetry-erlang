# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## Experimental API 0.5.2 - 2024-11-22

### Added

- [Hard code semconv values](https://github.com/open-telemetry/opentelemetry-erlang/pull/780)

## Experimental SDK 0.6.0 - 2024-11-22

### Added

- [move metrics and logs to separate exporter modules from
  traces](https://github.com/open-telemetry/opentelemetry-erlang/pull/720)
- [make the console metric exporter handle all
  values](https://github.com/open-telemetry/opentelemetry-erlang/pull/709)

### Fixes

- [metric exporter: nothing to do for an empty list of
  metrics](https://github.com/open-telemetry/opentelemetry-erlang/pull/782)
- [Hard code semconv
  values](https://github.com/open-telemetry/opentelemetry-erlang/pull/780)
- [removed old unused otel_metric_exporter
  module](https://github.com/open-telemetry/opentelemetry-erlang/pull/715)
  
## Exporter 1.8.0 - 2024-10-05

### Fixes

- [BREAKING: Fixes support for attribute values that are lists when the elements
  are strings. Lists of strings in attribute values are no longer flattened but
  remain lists. Meaning to use an Erlang charlist string or iolist as a value in
  an attribute you must convert with `unicode:characters_to_binary` before
  adding to the
  attributes](https://github.com/open-telemetry/opentelemetry-erlang/pull/737)

## API 1.4.0 - 2024-10-05

### Changes

- [Hard code semconv 0.2 values](https://github.com/open-telemetry/opentelemetry-erlang/pull/780)

## SDK 1.5.0 - 2024-10-05

### Changes

- [Fix invalid warning log causing FORMATTER
  CRASH](https://github.com/open-telemetry/opentelemetry-erlang/pull/774)
- [Hard code semconv 0.2 values](https://github.com/open-telemetry/opentelemetry-erlang/pull/780)

## API 1.3.1 - 2024-09-03

### Fixes

- [Fix dialyzer warning when starting a span by adding
  `otel_span:start_config`](https://github.com/open-telemetry/opentelemetry-erlang/pull/717)

## Experimental API 0.5.1 - 2024-03-18

### Added

- [instrument kind temporality function for use by the
  SDK](https://github.com/open-telemetry/opentelemetry-erlang/pull/713)

## Experimental SDK 0.5.1 - 2024-03-18

### Fixes

- [use correct default temporality for streams based on the instrument
  kind](https://github.com/open-telemetry/opentelemetry-erlang/pull/713)

## API 1.3.0 - 2024-03-15

### Changes

- [Add `otel_tracestate` module for creating and updating
  tracestate](https://github.com/open-telemetry/opentelemetry-erlang/pull/607)
- [Attributes module `otel_attributes` moved to
  API](https://github.com/open-telemetry/opentelemetry-erlang/pull/618)
- [Moved attribute processing functions to `otel_attributes` from
  `otel_span`](https://github.com/open-telemetry/opentelemetry-erlang/pull/620)

## SDK 1.4.0 - 2024-03-15

### Changes

- [Attributes module `otel_attributes` moved to
  API](https://github.com/open-telemetry/opentelemetry-erlang/pull/618)
- [create unique processor name in
  otel_tracer_server](https://github.com/open-telemetry/opentelemetry-erlang/pull/646)

### Fixes

- [Fix leak of atoms/persistent terms by creating unique processor name in `otel_tracer_server`](https://github.com/open-telemetry/opentelemetry-erlang/pull/646)
- [fix(otel_batch_processor): don't divide `max_queue_size` by
  word-size](https://github.com/open-telemetry/opentelemetry-erlang/pull/635)
- [fix(otel_processor): wait for runner process
  termination](https://github.com/open-telemetry/opentelemetry-erlang/pull/641)

## Exporter 1.7.0 - 2024-03-15

## Added

- [Add User-Agent header to exporter
  requests](https://github.com/open-telemetry/opentelemetry-erlang/pull/605)

## Experimental API 0.5.0 - 2024-03-15

### Changes

- [Allow to create observable instruments without passing callback
  arguments](https://github.com/open-telemetry/opentelemetry-erlang/pull/604)
- [Allow to give `advisory_params` to instrument creation functions](https://github.com/open-telemetry/opentelemetry-erlang/pull/628)
- [Attributes are optional in Counter.add(), UpDownCounter.add() and Histo.record()](https://github.com/open-telemetry/opentelemetry-erlang/pull/632)
- [Support explicit_bucket_boundaries advisory
  parameters](https://github.com/open-telemetry/opentelemetry-erlang/pull/628)

## Experimental SDK 0.5.0 - 2024-03-15

### Added

- [Add `instrument_unit` to view criteria](https://github.com/open-telemetry/opentelemetry-erlang/pull/604)
- [Validate instrument name](https://github.com/open-telemetry/opentelemetry-erlang/pull/604)
- [Handle `explicit_bucket_boundaries` advisory parameter](https://github.com/open-telemetry/opentelemetry-erlang/pull/628)
- [Rename `boundaries` to `explicit_bucket_boundaries` in histogram explicit aggregation options](https://github.com/open-telemetry/opentelemetry-erlang/pull/628)
- [Allow creating wildcard views](https://github.com/open-telemetry/opentelemetry-erlang/pull/624)
- [Exemplars support](https://github.com/open-telemetry/opentelemetry-erlang/pull/692)
- [Metric
  producers](https://github.com/open-telemetry/opentelemetry-erlang/pull/701)
- [Exemplar reservoir support](https://github.com/open-telemetry/opentelemetry-erlang/pull/692)
  
### Changes

- [Align histogram default boundaries with specification](https://github.com/open-telemetry/opentelemetry-erlang/pull/614)
- [Metrics: fix observable callbacks to return a list of
  results](https://github.com/open-telemetry/opentelemetry-erlang/pull/561)
- [Add a fresh context to each observable callback and test observe
  exemplars](https://github.com/open-telemetry/opentelemetry-erlang/pull/697)
  
### Fixes

 - [Correctly record histogram values greater than last boundary](https://github.com/open-telemetry/opentelemetry-erlang/pull/614)
 - [Readers should use a default cumulative temporality if not specified](https://github.com/open-telemetry/opentelemetry-erlang/pull/613)
 - [Check for positive data values in counters and histograms](https://github.com/open-telemetry/opentelemetry-erlang/pull/632)
- [Fix Delta metric export to only include those recorded in collection cycle](https://github.com/open-telemetry/opentelemetry-erlang/pull/677)
- [Cumulative sums
  fix](https://github.com/open-telemetry/opentelemetry-erlang/pull/592)
- [Fix transmitted time units for
  logs](https://github.com/open-telemetry/opentelemetry-erlang/pull/640)
- [don't export unit fields in metrics when the unit is
  undefined](https://github.com/open-telemetry/opentelemetry-erlang/pull/669)

## SDK 1.3.1 - 2023-08-15

### Added

- [Add support for OTEL_SDK_DISABLED environment variable and sdk_disabled
  application environment
  variable](https://github.com/open-telemetry/opentelemetry-erlang/pull/574)

### Changes

- [Resource is now an argument to TracerProvider start, but still set
  automatically by SDK startup of the global
  Provider](https://github.com/open-telemetry/opentelemetry-erlang/pull/568)
- [Global Tracer no longer set to no-op on SDK
  shutdown](https://github.com/open-telemetry/opentelemetry-erlang/pull/568)
- [Remove use of `deprecated` module attribute to support
  OTP-22](https://github.com/open-telemetry/opentelemetry-erlang/pull/603)

### Fixes

- [Fixed parsing of key/value list configuration where value has = in
  it](https://github.com/open-telemetry/opentelemetry-erlang/pull/596)

## API 1.2.2 - 2023-08-15

### Changes

- [Remove use of `deprecated` module attribute to support
  OTP-22](https://github.com/open-telemetry/opentelemetry-erlang/pull/603)

## Exporter 1.6.0 - 2023-06-21

### Changes

- [Update OTLP protos to
  0.20.0](https://github.com/open-telemetry/opentelemetry-erlang/pull/598)

## Exporter 1.5.0 - 2023-05-19

### Fixes

- [only start inets httpc profile if it doesn't
  exist](https://github.com/open-telemetry/opentelemetry-erlang/pull/591)

## Exporter 1.4.1 - 2023-04-26

### Fixes

- [Relax version constraints on API and
  SDK](https://github.com/open-telemetry/opentelemetry-erlang/pull/578)

## SDK 1.3.0 - 2023-03-21

### Fixes

- [Fix swapping exporter
  tables](https://github.com/open-telemetry/opentelemetry-erlang/pull/559)

## Exporter 1.4.0 - 2023-02-21

### Fixes

- [bump tls_certificate_check to fix crash on OTP-25](https://github.com/open-telemetry/opentelemetry-erlang/pull/547)

## API v1.2.1 - 2023-02-21

### Fixes

- [Have set_status check is_recording in all cases](https://github.com/open-telemetry/opentelemetry-erlang/pull/540)
- [Using opentelemetry_semantic_conventions for record_exception](https://github.com/open-telemetry/opentelemetry-erlang/pull/537)

## Experimental SDK 0.3.0 - 2023-02-21

### Fixes

- [metrics: fix updating delta timestamps in aggregations](https://github.com/open-telemetry/opentelemetry-erlang/pull/546)
- [Move is_monotonic check to the SDK](https://github.com/open-telemetry/opentelemetry-erlang/pull/544)

## Experimental API 0.3.0 - 2023-02-21

### Fixes

- [Move is_monotonic check to the SDK](https://github.com/open-telemetry/opentelemetry-erlang/pull/544)

## Exporter 1.2.2 - 2022-10-21

### Fixes

- [Allow multiple instances of OTLP grpc
  connections](https://github.com/open-telemetry/opentelemetry-erlang/pull/481)

## Experimental API 0.1.0 - 2022-10-19

### Added

- Erlang Metrics API

### Removed

- [Old experimental metrics
  api](https://github.com/open-telemetry/opentelemetry-erlang/pull/479)
  
## Experimental SDK 0.1.0 - 2022-10-19

### Added

- Metrics SDK
- Logging handler and exporter
- [Delta support for explicit
  histogram](https://github.com/open-telemetry/opentelemetry-erlang/pull/475)
- [Logging
  Handler](https://github.com/open-telemetry/opentelemetry-erlang/pull/468)
- [OTLP Metrics
  exporting](https://github.com/open-telemetry/opentelemetry-erlang/pull/456)
  
### Fixed

- [Fix OTLP Protocol allowed
  values](https://github.com/open-telemetry/opentelemetry-erlang/pull/420)  
  
## API 1.1.1 - 2022-10-19

### Fixed

- Gradualizer cleanup of type specs

## SDK 1.1.2 - 2022-10-19

#### Added

- [Replaced telemetry_library record with library info as resource
  attributes](https://github.com/open-telemetry/opentelemetry-erlang/pull/457)
  
## Exporter 1.2.1 - 2022-09-08

### Fixes

- Gradualizer cleanup
  
## Exporter 1.2.0 - 2022-09-08

### Fixes

- [Fix InstrumentationScope encoding in OTLP
  protobufs](https://github.com/open-telemetry/opentelemetry-erlang/pull/451)
  
### Added
  
- [Exporter now respects top-level `ssl_options` application environment value
  and handles endpoint parse errors](https://github.com/open-telemetry/opentelemetry-erlang/pull/442)

## SDK 1.1.1 - 2022-09-02

### Added

- [Flush Trace Provider on application
  shutdown](https://github.com/open-telemetry/opentelemetry-erlang/pull/470)

### Fixes

- Fix dependency on API to require `~> 1.1`

## Exporter 1.1.1 - 2022-09-02

### Fixes

- Fix dependency on API and SDK to require `~> 1.1`

## Zipkin Exporter 1.1.0 - 2022-09-02

### Fixes

- Support `opentelemetry_sdk ~> 1.1` exporter API

## API 1.1.0 - 2022-8-31

#### Added

- [Span context now set in logger metadata when context is updated in process
  dictionary](https://github.com/open-telemetry/opentelemetry-erlang/pull/394)
- [Instrumentation Scope replaces Instrumentation
  Library](https://github.com/open-telemetry/opentelemetry-erlang/pull/405) --
  If you were using the record directly, please use the function
  `opentelemetry:instrumentation_scope/3` or
  `opentelemetry:instrumentation_library/3` to create an `instrumentation_scope`
  record. 

## SDK 1.1.0 - 2022-8-31

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

## Exporter 1.1.0 - 2022-8-31

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
  dictionaries or non-homogeneous lists/tuples. The one exception we have
  kept is continuing to allow atoms in place of binaries for performance.
- Attribute values of type list/tuple must be homogeneous.
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
