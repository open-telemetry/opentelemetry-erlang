-module(otel_baggage_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opentelemetry.hrl").
-include("otel_tracer.hrl").

all() ->
    [pdict_context, explicit_context].

init_per_suite(Config) ->
    application:load(opentelemetry_api),
    Config.

end_per_suite(_Config) ->
    ok.

pdict_context(_Config) ->
    otel_baggage:set(<<"key-1">>, <<"value-1">>),
    otel_baggage:set([{<<"key-2">>, <<"value-2">>}]),

    ?assertEqual(#{<<"key-1">> => <<"value-1">>,<<"key-2">> => <<"value-2">>}, otel_baggage:get_all()),

    otel_baggage:set(#{<<"key-1">> => <<"value-3">>}),

    ?assertEqual(#{<<"key-1">> => <<"value-3">>,<<"key-2">> => <<"value-2">>}, otel_baggage:get_all()),

    otel_baggage:clear(),
    ?assert(maps:size(otel_baggage:get_all()) =:= 0),

    ok.

explicit_context(_Config) ->
    Ctx = otel_ctx:new(),

    Ctx1 = otel_baggage:set(Ctx, <<"key-1">>, <<"value-1">>),
    Ctx2 = otel_baggage:set(Ctx1, [{<<"key-2">>, <<"value-2">>}]),

    ?assertEqual(#{<<"key-1">> => <<"value-1">>,<<"key-2">> => <<"value-2">>}, otel_baggage:get_all(Ctx2)),

    Ctx3 = otel_baggage:clear(Ctx2),
    ?assert(maps:size(otel_baggage:get_all(Ctx3)) =:= 0),

    ok.

