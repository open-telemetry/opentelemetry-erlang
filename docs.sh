#!/bin/bash
set -e

# Setup:
#
#     # 1. install OTP 24+
#     # 2. install ExDoc:
#     $ mix escript.install github elixir-lang/ex_doc

rebar3 compile
rebar3 edoc
sdk_version=1.5.0
api_version=1.4.0
exp_sdk_version=0.6.0
exp_api_version=0.5.2
otlp_version=1.8.0
zipkin_version=1.1.0

mix escript.install hex ex_doc

ex_doc "opentelemetry" $sdk_version "_build/default/lib/opentelemetry/ebin" \
  --source-ref v${sdk_version} \
  --config apps/opentelemetry/docs.config $@ \
  --output "apps/opentelemetry/doc"

ex_doc "opentelemetry_experimental" $exp_sdk_version "_build/default/lib/opentelemetry_experimental/ebin" \
  --source-ref v${exp_sdk_version} \
  --config apps/opentelemetry_experimental/docs.config $@ \
  --output "apps/opentelemetry_experimental/doc"

ex_doc "opentelemetry_exporter" $otlp_version "_build/default/lib/opentelemetry_exporter/ebin" \
  --source-ref v${otlp_version} \
  --config apps/opentelemetry_exporter/docs.config $@ \
  --output "apps/opentelemetry_exporter/doc"

ex_doc "opentelemetry_zipkin" $zipkin_version "_build/default/lib/opentelemetry_zipkin/ebin" \
  --source-ref v${zipkin_version} \
  --config apps/opentelemetry_zipkin/docs.config $@ \
  --output "apps/opentelemetry_zipkin/doc"

pushd apps/opentelemetry_api
mix deps.get
mix compile
rebar3 edoc
mix docs
popd

pushd apps/opentelemetry_api_experimental
mix deps.get
mix compile
popd
ex_doc "opentelemetry_api_experimental" $exp_api_version "apps/opentelemetry_api_experimental/_build/dev/lib/opentelemetry_api_experimental/ebin" \
  --source-ref v${exp_api_version} \
  --config apps/opentelemetry_api_experimental/docs.config $@ \
  --output "apps/opentelemetry_api_experimental/doc"

