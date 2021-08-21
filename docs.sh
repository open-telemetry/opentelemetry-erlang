#!/bin/bash
set -e

# Setup:
#
#     # 1. install OTP 24+
#     # 2. install ExDoc:
#     $ mix escript.install github elixir-lang/ex_doc

rebar3 compile
rebar3 as docs edoc
version=1.0.0-rc.2

ex_doc "opentelemetry" $version "_build/default/lib/opentelemetry/ebin" \
  --source-ref v${version} \
  --config docs.config $@ \
  --output "apps/opentelemetry/doc"

