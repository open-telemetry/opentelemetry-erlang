# OpenTelemetry

Erlang libraries for [OpenTelemetry](https://opentelemetry.io/)

[![EEF Observability WG project](https://img.shields.io/badge/EEF-Observability-black)](https://github.com/erlef/eef-observability-wg)
[![Gitter chat](https://badges.gitter.im/open-telemetry/opentelemetry-erlang.svg)](https://gitter.im/open-telemetry/opentelemetry-erlang)

[![Hex.pm - opentelemetry_api](https://img.shields.io/hexpm/v/opentelemetry_api?label=opentelemetry_api)](https://hex.pm/packages/opentelemetry_api)
[![Hex.pm - opentelemetry](https://img.shields.io/hexpm/v/opentelemetry?label=opentelemetry)](https://hex.pm/packages/opentelemetry)
[![Hex.pm - opentelemetry_exporter](https://img.shields.io/hexpm/v/opentelemetry_exporter?label=opentelemetry_exporter)](https://hex.pm/packages/opentelemetry_exporter)
[![License - Apache-2.0](https://img.shields.io/badge/license-Apache--2.0-informational)](LICENSE)

![Build Status - Erlang](https://github.com/open-telemetry/opentelemetry-erlang/workflows/Erlang/badge.svg)
![Build Status - Elixir](https://github.com/open-telemetry/opentelemetry-erlang/workflows/Elixir/badge.svg)

## Overview

This project contains the following components, published at [hex.pm](https://hex.pm)

- [OpenTelemetry API](apps/opentelemetry_api/): [![Hex.pm - opentelemetry_api](https://img.shields.io/hexpm/v/opentelemetry_api?label=opentelemetry_api)](https://hex.pm/packages/opentelemetry_api)

- [OpenTelemetry SDK](apps/opentelemetry/): [![Hex.pm - opentelemetry](https://img.shields.io/hexpm/v/opentelemetry?label=opentelemetry)](https://hex.pm/packages/opentelemetry)
- [OpenTelemetry Exporter](apps/opentelemetry_exporter/): [![Hex.pm - opentelemetry_exporter](https://img.shields.io/hexpm/v/opentelemetry_exporter?label=opentelemetry_exporter)](https://hex.pm/packages/opentelemetry_exporter)

See the [OpenTelemetry Language Library Design Principles](https://github.com/open-telemetry/opentelemetry-specification/blob/v1.0.1/specification/library-guidelines.md) for responsibilities of each component.

## Requirements

All components require OTP 21.3 or above unless otherwise noted.

## Usage

To instrument your project:

- Add and use [OpenTelemetry API](apps/opentelemetry_api/) to add instrumentation manually
- Add and configure instrumentation packages that use [OpenTelemetry API](apps/opentelemetry_api/)
- Add and configure [OpenTelemetry SDK](apps/opentelemetry/) and exporters

To instrument your package or create an instrumentation package for an existing package:

- Add and use [OpenTelemetry API](apps/opentelemetry_api/)

See the documentation of each component for details.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md).

Approvers:

- [Fred Hebert](https://github.com/ferd), Honeycomb

Maintainers:

- [Łukasz Jan Niemier](https://github.com/hauleth), Remote Technology, Inc.
- [Ilya Khaprov](https://github.com/deadtrickster), KOBIL Systems
- [Tristan Sloughter](https://github.com/tsloughter)
