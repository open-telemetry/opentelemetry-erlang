#!/bin/bash
set -e

# Setup:
#
#     # 1. install OTP 24+
#     # 2. install ExDoc:
#     $ mix escript.install github elixir-lang/ex_doc

rebar3 compile
rebar3 edoc
version=1.0.0

ex_doc "opentelemetry" $version "_build/default/lib/opentelemetry/ebin" \
  --source-ref v${version} \
  --config docs.config $@ \
  --output "apps/opentelemetry/doc"

ex_doc "opentelemetry_exporter" $version "_build/default/lib/opentelemetry_exporter/ebin" \
  --source-ref v${version} \
  --config apps/opentelemetry_exporter/docs.config $@ \
  --output "apps/opentelemetry_exporter/doc"
