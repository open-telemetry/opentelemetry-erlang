## OpenTelemetry Erlang/Elixir

---

<p align="center">
[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry_api?label=API&style=for-the-badge)](https://hex.pm/packages/opentelemetry_api)
[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry?label=SDK&style=for-the-badge)](https://hex.pm/packages/opentelemetry)
[![Hex.pm](https://img.shields.io/hexpm/v/opentelemetry_exporter?label=OTLP%20Exporter&style=for-the-badge)](https://hex.pm/packages/opentelemetry_exporter)
[![EEF Observability WG
project](https://img.shields.io/badge/EEF-Observability-black?style=for-the-badge)](https://github.com/erlef/eef-observability-wg)
</p>

<p align="center">
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/open-telemetry/opentelemetry-erlang/Erlang?style=for-the-badge)](https://github.com/open-telemetry/opentelemetry-erlang/actions)
![Codecov](https://img.shields.io/codecov/c/github/open-telemetry/opentelemetry-erlang?style=for-the-badge)
</p>

---

[OpenTelemetry](https://opentelemetry.io/) distributed tracing framework for
Erlang and Elixir.

These applications implement version 1.8.0 of the [OpenTelemetry
Specification](https://github.com/open-telemetry/opentelemetry-specification),
see the [spec compliance
matrix](https://github.com/open-telemetry/opentelemetry-specification/blob/main/spec-compliance-matrix.md)
for a list of features supported.

## Requirements

- Erlang/OTP 22+

If using the Elixir API:

- Elixir 1.11+

## Contacting Us

We hold weekly meetings. See details at [community page](https://github.com/open-telemetry/community#special-interest-groups).

We use [GitHub
Discussions](https://github.com/open-telemetry/opentelemetry-erlang/discussions)
for support or general questions. Feel free to drop us a line.

We are also present in the #otel-erlang-elixir channel in the [CNCF
slack](https://slack.cncf.io/). Please join us for more informal discussions.

You can also find us in the #opentelemetry channel on [Elixir
Slack](https://elixir-slackin.herokuapp.com/).

## Getting Started

You can find a getting started guide on [opentelemetry.io](https://opentelemetry.io/docs/instrumentation/erlang/getting-started/).

To start capturing distributed traces from your application it first needs to be
instrumented. The easiest way to do this is by using an instrumentation library,
there are a number of [officially supported instrumentation
libraries](https://github.com/open-telemetry/opentelemetry-erlang-contrib) for
popular Erlang and Elixir libraries and frameworks.

## Design

The [OpenTelemetry
specification](https://github.com/open-telemetry/opentelemetry-specification)
defines a language library as having 2 components, the API and the SDK. The API
must not only define the interfaces of any implementation in that language but
also be able to function as a noop implementation of the tracer. The SDK is the
default implementation of the API that must be optional.

When instrumenting a project your application should only depend on the
[OpenTelemetry API](https://hex.pm/packages/opentelemetry_api) application,
found in directory `apps/opentelemetry_api` of this repo which is published as
the hex package [opentelemetry_api](https://hex.pm/packages/opentelemetry_api). 

The SDK implementation, found under `apps/opentelemetry` and hex package
[opentelemetry](https://hex.pm/packages/opentelemetry), should be included in an
OTP Release along with an exporter.

Example of Release configuration in `rebar.config`:

``` erlang
{relx, [{release, {my_instrumented_release, "0.1.0"},
         [opentelemetry_exporter,
	      {opentelemetry, temporary},
          my_instrumented_app]},

       ...]}.
```

Example configuration for [mix's Release
task](https://hexdocs.pm/mix/Mix.Tasks.Release.html):

``` elixir
def project do
  [
    releases: [
      my_instrumented_release: [
        applications: [opentelemetry_exporter: :permanent, opentelemetry: :temporary]
      ],

      ...
    ]
  ]
end
```

In the above example `opentelemetry_exporter` is first to ensure all of its
dependencies are booted before `opentelemetry` attempts to start the
exporter. `opentelemetry` is set to `temporary` so that if the `opentelemetry`
application crashes, or is shutdown, it does not terminate the other
applications in the project -- `opentelemetry_exporter` does not not need to be
`temporary` because it does not have a startup and supervision tree. This is
optional, the `opentelemetry` application purposely sticks to `permanent` for
the processes started by the root supervisor to leave it up to the end user
whether they want the crash or shutdown or `opentelemetry` to be ignored or
cause the shutdown of the rest of the applications in the release.

## Git Dependencies

While it is recommended to use the Hex packages for the
[API](https://hex.pm/packages/opentelemetry_api),
[SDK](https://hex.pm/packages/opentelemetry) and [OTLP
exporter](https://hex.pm/packages/opentelemetry_exporter), there are times
depending on the git repo is necessary. Because the OpenTelemetry OTP
Applications are kept in a single repository, under the directory `apps`, either
[rebar3's](https://rebar3.org) `git_subdir` (rebar 3.14 or above is required) or
[mix's](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
`sparse` feature must be used when using as Git dependencies in a project. The
blocks below shows how in rebar3 and mix the git repo for the API and/or SDK
Applications can be used.

``` erlang
{opentelemetry_api, {git_subdir, "http://github.com/open-telemetry/opentelemetry-erlang", {branch, "main"}, "apps/opentelemetry_api"}},
{opentelemetry, {git_subdir, "http://github.com/open-telemetry/opentelemetry-erlang", {branch, "main"},
"apps/opentelemetry"}},
{opentelemetry_exporter, {git_subdir, "http://github.com/open-telemetry/opentelemetry-erlang", {branch, "main"}, "apps/opentelemetry_exporter"}}
```

``` elixir
{:opentelemetry_api, github: "open-telemetry/opentelemetry-erlang", sparse:
"apps/opentelemetry_api", override: true},
{:opentelemetry, github: "open-telemetry/opentelemetry-erlang", sparse:
"apps/opentelemetry", override: true},
{:opentelemetry_exporter, github: "open-telemetry/opentelemetry-erlang", sparse: "apps/opentelemetry_exporter", override: true}
```

The `override: true` is required because the SDK Application, `opentelemetry`, has
the API in its `deps` list of its `rebar.config` as a hex dependency and this will
clash when `mix` tries to resolve the dependencies and fail without the
override. `override: true` is also used on the SDK because the
`opentelemetry_exporter` application depends on it and the API as hex deps so if
it is included the override is necessary.

## Benchmarks

Running benchmarks is done with [benchee](https://github.com/bencheeorg/benchee). Benchmark functions are in modules under `samples/`. To run them open a rebar3 shell in the `bench` profile:

``` shell
$ rebar3 as bench compile

> otel_benchmarks:run().
```

If an Elixir script is wanted for the benchmarks they could be run like:

``` shell
$ ERL_LIBS=_build/bench/lib/ mix run --no-mix-exs samples/run.exs
```

## W3C Trace Context Interop Tests

Start the interop web server in a shell:

``` shell
$ rebar3 as interop shell

> w3c_trace_context_interop:start().
```

Then, clone the [W3C Trace Context repo](https://github.com/w3c/trace-context) and run the tests:

``` shell
$ cd test
$ python3 test.py http://127.0.0.1:5000/test
```

## Contributing

Approvers ([@open-telemetry/erlang-approvers](https://github.com/orgs/open-telemetry/teams/erlang-approvers)):

- [Tristan Sloughter](https://github.com/tsloughter), [Splunk](https://www.splunk.com/en_us/observability/apm-application-performance-monitoring.html)
- [Bryan Naegele](https://github.com/bryannaegele), [Simplebet](https://simplebet.io/)
- [Greg Mefford](https://github.com/GregMefford), [STORD](https://www.stord.com/)
- [Fred Hebert](https://github.com/ferd), [Honeycomb](https://www.honeycomb.io/)
- [Łukasz Jan Niemier](https://github.com/hauleth)
- [Iliia Khaprov](https://github.com/deadtrickster), [VMWare](https://www.vmware.com/)

*Find more about the approver role in [community repository](https://github.com/open-telemetry/community/blob/master/community-membership.md#approver).*

Maintainers ([@open-telemetry/erlang-maintainers](https://github.com/orgs/open-telemetry/teams/erlang-maintainers)):

- [Tristan Sloughter](https://github.com/tsloughter), [Splunk](https://www.splunk.com/en_us/observability/apm-application-performance-monitoring.html)
- [Bryan Naegele](https://github.com/bryannaegele), [Simplebet](https://simplebet.io/)
- [Łukasz Jan Niemier](https://github.com/hauleth)
- [Iliia Khaprov](https://github.com/deadtrickster), [VMWare](https://www.vmware.com/)

*Find more about the maintainer role in [community repository](https://github.com/open-telemetry/community/blob/master/community-membership.md#maintainer).*

### Thanks to all the people who have contributed

[![contributors](https://contributors-img.web.app/image?repo=open-telemetry/opentelemetry-erlang)](https://github.com/open-telemetry/opentelemetry-erlang/graphs/contributors)
