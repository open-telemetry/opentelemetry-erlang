# Development Setup Guide for Erlang Repository

## Introduction
Welcome to the Erlang Repository! 🎉

This guide aims to assist contributors in setting up their local development environment. Feel free to suggest improvements or raise questions in your contributions.

## Pre-requisites

To work with this repository, you'll need the following tools and dependencies:

- **Erlang/OTP Version** :
  
- We support the latest major release plus the prior two releases, aligning with Ericsson’s version support policy for Erlang itself.  
- For a definitive list of supported versions, refer to the [erlang.yml](.github/workflows/erlang.yml) file in this repository.  

If using the Elixir API:  

* Elixir 1.14+

* For a definitive list of supported versions, refer to the [elixir.yml](.github/workflows/elixir.yml) file in this repository.

* Refer to the official [Erlang Installation Guide](https://www.erlang.org/downloads) for setting up Erlang.

## Local Run/Build

### Setting Up and Running the Project Locally

To run the project locally, follow these steps:

Start required services:

```
docker compose up -d
```

Set up Erlang/OTP and Rebar3 and Verify setup (Ensure you have the correct versions as specified in the prerequisites):

Run the compilation step to check for errors:
```
rebar3 compile
```

## Testing

To execute tests, use the following commands:

### Unit Tests (EUnit):

```
rebar3 eunit
```

### Integration and Functional Tests (Common Test):

```
rebar3 ct
```

### Test results and logs can be viewed in a browser by opening:

```
build/test/logs/index.html

```
### Elixir Tests

Elixir tests are run in two places:

1. Inside apps/opentelemetry_api/:

```
mix test
```

2. At the top level:

```
mix test --no-start test/otel_tests.exs test/otel_metric_tests.exs
```

## Contribution Rules
Please review the [OpenTelemetry Specification](https://opentelemetry.io/docs/specs/otel/) for details on coding standards, commit message formatting, and the contribution process.

## Troubleshooting Guide

### Debugging Common Failures

* Test Failures: Check the test logs in _build/test/logs/.

* Compilation Issues: Ensure correct OTP and Rebar3 versions are installed.

* Dialyzer Warnings: Ensure rebar3 compile completes successfully.

## Developer Setup and Language Servers

For improved development experience, consider using the following Language Server Protocols (LSPs):

* Erlang LSP: [Erlang Language Platform](https://whatsapp.github.io/erlang-language-platform/)

* Installing it removes the need for an additional type-checking step:

```
elp eqwalize-all
```

* Install from: [Erlang Language Platform Releases](https://github.com/WhatsApp/erlang-language-platform/releases/)

* Elixir LSP: [Elixir LS](https://github.com/elixir-lsp/elixir-ls)

## Further Help

* If you encounter any issues or have questions, feel free to open an issue or reach out to the maintainers.
* Join our [slack](https://cloud-native.slack.com/archives/C01N75YMZCN) channel also for further help and discussions
