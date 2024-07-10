-module(opentelemetry_semconv_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("incubating/db_attributes.hrl").

all() ->
  [registry_macros].

registry_macros(_Config) ->
  ?assertEqual('postgresql', ?'db_system.postgresql'),
  ?assertEqual('custom', ?db_system('custom')),
  ok.