-module(otel_propagators_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-define(assertListsMatch(List1, List2), ?assertEqual(lists:sort(List1), lists:sort(List2))).

all() ->
    [custom_propagator].

init_per_suite(Config) ->
    application:load(opentelemetry_api),
    {Extractors, Injectors} = custom_propagator:propagators(),
    opentelemetry:set_text_map_extractors([Extractors]),
    opentelemetry:set_text_map_injectors([Injectors]),

    Config.

end_per_suite(_Config) ->
    ok.

custom_propagator(_Config) ->
    Something = <<"hello">>,
    custom_propagator:add_to_context(Something),

    Headers = otel_propagator:text_map_inject([{<<"existing-header">>, <<"I exist">>}]),

    ?assertListsMatch([{<<"something-header-id">>, Something},
                       {<<"existing-header">>, <<"I exist">>}], Headers),

    %% clear context to test extraction
    otel_ctx:clear(),

    %% make header keys uppercase to validate the extractor is case insensitive
    BinaryHeaders = [{string:uppercase(Key), iolist_to_binary(Value)} || {Key, Value} <- Headers],
    otel_propagator:text_map_extract(BinaryHeaders),

    ?assertEqual(Something, custom_propagator:context_content()),

    ok.
