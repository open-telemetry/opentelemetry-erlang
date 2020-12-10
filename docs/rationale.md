Rationale
---

When creating a library, often times designs and decisions are made that get lost over time. This document tries to collect information on design decisions to answer common questions that may come up when you explore.

# Versioning and Releasing

The OTP Applications and OpenTelemetry Spec itself use semver v2.

In this document references to OTP concepts is done by capitalizing the word
(for example Application and Release) while the generic term (like release) is
lowercase).

## Module Prefix

The module prefix for all modules in any of these core Applications is
`otel`. This means modules can move between Applications without their name
changing and because of Erlang's flat namespace there is no code to change for a
user when a stable API graduates from experimental to stable, if the user was
using the latest version of the experimental API.

This also allows flexibility for modules that might be in the SDK but are found
to be better placed in the API, or vice versa. This has happened a few times in
the pre-1.0 world as functionality was floating from SDK to API at times.

## OTP Applications

### Experimental API (`opentelemetry_api_experimental`)

The experimental package is where any API that is not stable when 1.0 is
released [MUST] live. At this time (prior to 1.0) that means Metrics and Logging.

This package will always be 0.x because it is never stable and modules will be
removed when they are moved to the stable API package.

[MUST]: https://tools.ietf.org/html/rfc2119#section-1

### API (`opentelemetry_api`)

The API package is the stable API package that we must provide semver defined
backwards compatibility once a major (1.0) release is made. When an API becomes
stable its modules are moved from `opentelemetry_api_experimental` to
`opentelemetry_api` and a new minor release of both is published.

At the time of 1.0 the following APIs will live in the `opentelemetry_api`
package:

* Tracing
* Baggage
* Context

### Experimental SDK (`opentelemetry_sdk_experimental`)

The experimental SDK contains the implementations for the APIs in the
experimental API of the same version.

Any setup for signals contained in the experimental SDK must be done on startup
of the experimental SDK -- for example, setting the default Meter would be done
in `start/2` of `opentelemetry_sdk_experimental`.

This Application is versioned in lockstep with the experimental API and will
never go above 0.x.

### SDK (`opentelemetry`)

Functionality is implemented in this Application and the API is dynamically
configured to use a particular SDK -- at this time there is only 1 SDK
implementation, the default implementation.

A goal is that the latest SDK can always be used with any version of the API, so
that a user can always pull the latest implementation into their final Release
to run with any API that was used in instrumented Applications within the
Release.

### OTLP Exporter (`opentelemetry_exporter`)

Exporter implementations are tied to the SDK's public API.

## Releases

### Experimental API

As noted in the previous section `opentelemetry_api_experimental` is versioned
separately from the rest and will always remain 0.x.

### API

Additions to the API are released with minor version bumps.

### Experimental SDK

As noted in the previous section `opentelemetry_sdk_experimental` is versioned
separately from the rest, but in lockstep with `opentelemetry_api_experimental`,
and will always remain 0.x.

### SDK

Additions to the SDK are released with minor version bumps.

## Deprecation

Code is only marked as deprecated when the replacement is stable.

Unlikely example: There is a Tracing v2 spec defined. The module will be named
`otel_trace2` and the functions in `otel_trace` marked as deprecated.

Deprecated functions must be marked with `-deprecated` in the module so that
`xref` provides a warning about usage to the user.

## Removal

A major version bump is required to remove a signal or module.

For the unlikely example from deprecation this step would mean removal of the
original module (`otel_trace`) and a major version bump release.

## Examples

Purely for illustration purposes, not intended to represent actual releases:

- v1.0.0 release:
   - `opentelemetry_api` 1.0.0
     - Contains APIs for tracing, baggage, propagators
   - `opentelemetry_api_experimental` 0.2.0
   - `opentelemetry_sdk` 1.0.0
   - `opentelemetry_sdk_experimental` 0.2.0
- v1.15.0 release (with metrics)
   - `opentelemetry_api` 1.15.0
     - Contains APIs for tracing, baggage, propagators, metrics
   - `opentelemetry_sdk` 1.15.0
   
