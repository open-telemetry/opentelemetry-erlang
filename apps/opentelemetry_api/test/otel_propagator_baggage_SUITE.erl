-module(otel_propagator_baggage_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opentelemetry.hrl").

-define(BAGGAGE_HEADER, <<"baggage">>).
-define(MAX_BAGGAGE_BYTES, 8192).
-define(MAX_BAGGAGE_ENTRIES, 180).

all() ->
    [extract_simple,
     extract_drops_oversized_header,
     extract_caps_entry_count,
     extract_within_caps,
     extract_skips_malformed_pair,
     extract_missing_header].

init_per_suite(Config) ->
    application:load(opentelemetry_api),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TC, Config) ->
    otel_ctx:clear(),
    Config.

end_per_testcase(_TC, _Config) ->
    otel_ctx:clear(),
    ok.

extract_simple(_Config) ->
    Header = <<"k1=v1,k2=v2">>,
    Ctx = extract(Header),
    Baggage = otel_baggage:get_all(Ctx),
    ?assertEqual(2, maps:size(Baggage)),
    ?assertEqual({<<"v1">>, []}, maps:get(<<"k1">>, Baggage)),
    ?assertEqual({<<"v2">>, []}, maps:get(<<"k2">>, Baggage)),
    ok.

extract_drops_oversized_header(_Config) ->
    Header = build_header(2000),
    ?assert(byte_size(Header) > ?MAX_BAGGAGE_BYTES),
    Ctx = extract(Header),
    ?assertEqual(#{}, otel_baggage:get_all(Ctx)),
    ok.

extract_caps_entry_count(_Config) ->
    %% Build a header that stays under the byte cap but carries more entries
    %% than `?MAX_BAGGAGE_ENTRIES`. Single-character keys/values keep each
    %% pair small enough that 300 entries are well under 8 KiB.
    Header = build_short_header(300),
    ?assert(byte_size(Header) =< ?MAX_BAGGAGE_BYTES),
    Ctx = extract(Header),
    Baggage = otel_baggage:get_all(Ctx),
    ?assertEqual(?MAX_BAGGAGE_ENTRIES, maps:size(Baggage)),
    ok.

extract_within_caps(_Config) ->
    Header = build_short_header(50),
    Ctx = extract(Header),
    Baggage = otel_baggage:get_all(Ctx),
    ?assertEqual(50, maps:size(Baggage)),
    ok.

extract_skips_malformed_pair(_Config) ->
    Header = <<"k1=v1,malformed,k2=v2">>,
    Ctx = extract(Header),
    Baggage = otel_baggage:get_all(Ctx),
    ?assertEqual(2, maps:size(Baggage)),
    ?assertEqual({<<"v1">>, []}, maps:get(<<"k1">>, Baggage)),
    ?assertEqual({<<"v2">>, []}, maps:get(<<"k2">>, Baggage)),
    ok.

extract_missing_header(_Config) ->
    Ctx0 = otel_ctx:new(),
    Ctx1 = otel_propagator_baggage:extract(Ctx0, #{},
                                           fun(C) -> maps:keys(C) end,
                                           fun(K, C) -> maps:get(K, C, undefined) end,
                                           []),
    ?assertEqual(Ctx0, Ctx1),
    ok.

%% helpers

extract(Header) ->
    Carrier = #{?BAGGAGE_HEADER => Header},
    Ctx = otel_ctx:new(),
    otel_propagator_baggage:extract(Ctx, Carrier,
                                    fun(C) -> maps:keys(C) end,
                                    fun(K, C) -> maps:get(K, C, undefined) end,
                                    []).

build_header(N) ->
    Pairs = [io_lib:format("k~B=v~B", [I, I]) || I <- lists:seq(1, N)],
    iolist_to_binary(lists:join(<<",">>, Pairs)).

build_short_header(N) ->
    Pairs = [["k", integer_to_list(I), "=v"] || I <- lists:seq(1, N)],
    iolist_to_binary(lists:join(<<",">>, Pairs)).
