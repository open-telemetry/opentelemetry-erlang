#!/bin/bash
set -e

# Setup:
#
#     # 1. install OTP 24+
#     # 2. install ExDoc:
#     $ mix escript.install github elixir-lang/ex_doc

rebar3 compile
rebar3 edoc
version=1.0.1

ex_doc "opentelemetry" $version "_build/default/lib/opentelemetry/ebin" \
  --source-ref v${version} \
  --config docs.config $@ \
  --output "apps/opentelemetry/doc"

ex_doc "opentelemetry_exporter" $version "_build/default/lib/opentelemetry_exporter/ebin" \
  --source-ref v${version} \
  --config apps/opentelemetry_exporter/docs.config $@ \
  --output "apps/opentelemetry_exporter/doc"

pushd apps/opentelemetry_api
mix deps.get
mix compile
popd
ex_doc "opentelemetry_api" $version "apps/opentelemetry_api/_build/dev/lib/opentelemetry_api/ebin" \
  --source-ref v${version} \
  --config apps/opentelemetry_api/docs.config $@ \
  --output "apps/opentelemetry_api/doc"
