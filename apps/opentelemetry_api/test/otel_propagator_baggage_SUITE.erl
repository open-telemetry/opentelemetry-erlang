-module(otel_propagator_baggage_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [enforces_max_entries, enforces_max_entry_length,
     enforces_max_entry_length_is_bytes_not_graphemes, enforces_max_total_length,
     charges_a_separator_byte_between_accepted_entries].

init_per_suite(Config) ->
    application:load(opentelemetry_api),
    Config.

end_per_suite(_Config) ->
    ok.

extract(Header) ->
    Carrier = [{<<"baggage">>, Header}],
    Ctx = otel_propagator_baggage:extract(otel_ctx:new(), Carrier,
                                          fun otel_propagator_text_map:default_carrier_keys/1,
                                          fun otel_propagator_text_map:default_carrier_get/2, []),
    otel_baggage:get_all(Ctx).

enforces_max_entries(_Config) ->
    Header = iolist_to_binary(lists:join(<<",">>,
                 [ [<<"k">>, integer_to_binary(I), <<"=v">>] || I <- lists:seq(1, 200) ])),
    Baggage = extract(Header),
    ?assertEqual(180, maps:size(Baggage)),
    ?assert(maps:is_key(<<"k1">>, Baggage)),
    ?assertNot(maps:is_key(<<"k181">>, Baggage)),
    ok.

enforces_max_entry_length(_Config) ->
    Big = binary:copy(<<"x">>, 5000),
    Header = <<"ok=1,big=", Big/binary>>,
    Baggage = extract(Header),
    ?assertEqual(1, maps:size(Baggage)),
    ?assert(maps:is_key(<<"ok">>, Baggage)),
    ?assertNot(maps:is_key(<<"big">>, Baggage)),
    ok.

enforces_max_entry_length_is_bytes_not_graphemes(_Config) ->
    Multibyte = unicode:characters_to_binary(lists:duplicate(2100, $\x{e9})), %% 2100 graphemes, 4200 bytes
    Header = <<"ok=1,u=", Multibyte/binary>>,
    Baggage = extract(Header),
    ?assertEqual(1, maps:size(Baggage)),
    ?assert(maps:is_key(<<"ok">>, Baggage)),
    ?assertNot(maps:is_key(<<"u">>, Baggage)),
    ok.

enforces_max_total_length(_Config) ->
    Header = iolist_to_binary(lists:join(<<",">>,
                 [ [<<"k">>, integer_to_binary(I), <<"=">>, binary:copy(<<"y">>, 200)]
                   || I <- lists:seq(1, 100) ])),
    Baggage = extract(Header),
    Size = maps:size(Baggage),
    ?assert(Size > 0 andalso Size < 100),
    ?assert(maps:is_key(<<"k1">>, Baggage)),
    ?assertNot(maps:is_key(<<"k100">>, Baggage)),
    ok.

charges_a_separator_byte_between_accepted_entries(_Config) ->
    FillerAPrefix = <<"filler_a=">>,
    FillerBPrefix = <<"filler_b=">>,
    SmallPair = <<"small=">>,
    FillerA = <<FillerAPrefix/binary, (binary:copy(<<"x">>, 4096 - byte_size(FillerAPrefix)))/binary>>,
    FillerBBytes = 8192 - byte_size(SmallPair) - byte_size(FillerA) - 1,
    FillerB = <<FillerBPrefix/binary, (binary:copy(<<"x">>, FillerBBytes - byte_size(FillerBPrefix)))/binary>>,
    Header = iolist_to_binary(lists:join(<<",">>, [FillerA, FillerB, SmallPair])),
    Baggage = extract(Header),
    ?assert(maps:is_key(<<"filler_a">>, Baggage)),
    ?assert(maps:is_key(<<"filler_b">>, Baggage)),
    ?assertNot(maps:is_key(<<"small">>, Baggage)),
    ok.
