-module(opentelemetry_semconv_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("incubating/attributes/db_attributes.hrl").
-include("incubating/attributes/http_attributes.hrl").

all() ->
  [registry_macros].

registry_macros(_Config) ->
  ?assertEqual('db.system', ?'DB_SYSTEM'),
  ?assertEqual('postgresql', ?'DB_SYSTEM_VALUES_POSTGRESQL'),
  ?assertEqual('http.route', ?'HTTP_ROUTE'),
  ok.
