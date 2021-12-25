-module(opentelemetry_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").
-include("otel_tracer.hrl").
-include("otel_span.hrl").
-include("otel_test_utils.hrl").
-include("otel_sampler.hrl").
-include("otel_span_ets.hrl").


all() ->
    [%% no need to include tests that don't export any spans with the simple/batch groups
     disable_auto_creation,
     old_disable_auto_creation,
     application_tracers,
     %% force flush is a test of flushing the batch processor's table
     force_flush,

     %% all other tests are run with both the simple and batch processor
     {group, otel_simple_processor},
     {group, otel_batch_processor}].

all_cases() ->
    [with_span, macros, child_spans,
     update_span_data, tracer_instrumentation_library, tracer_previous_ctx, stop_temporary_app,
     reset_after, attach_ctx, default_sampler, non_recording_ets_table,
     root_span_sampling_always_on, root_span_sampling_always_off,
     record_but_not_sample, record_exception_works, record_exception_with_message_works,
     propagator_configuration, propagator_configuration_with_os_env, force_flush,
     dropped_attributes, too_many_attributes].

groups() ->
    [{otel_simple_processor, [], all_cases()},
     {otel_batch_processor, [], all_cases()}].

init_per_suite(Config) ->
    application:load(opentelemetry),
    Config.

end_per_suite(_Config) ->
    application:unload(opentelemetry),
    ok.

init_per_group(Processor, Config) ->
    [{processor, Processor} | Config].

end_per_group(_, _Config) ->
    ok.

init_per_testcase(disable_auto_creation, Config) ->
    application:set_env(opentelemetry, create_application_tracers, false),
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config;
init_per_testcase(old_disable_auto_creation, Config) ->
    application:set_env(opentelemetry, register_loaded_applications, false),
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config;
init_per_testcase(application_tracers, Config) ->
    %% if both are set then the new one, `create_application_tracers', is used
    application:set_env(opentelemetry, register_loaded_applications, false),
    application:set_env(opentelemetry, create_application_tracers, true),
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config;
init_per_testcase(propagator_configuration, Config) ->
    os:unsetenv("OTEL_PROPAGATORS"),
    application:set_env(opentelemetry, text_map_propagators, [b3multi, baggage]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config;
init_per_testcase(propagator_configuration_with_os_env, Config) ->
    os:putenv("OTEL_PROPAGATORS", "tracecontext"),
    application:set_env(opentelemetry, text_map_propagators, [b3multi, baggage]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    Config;
init_per_testcase(force_flush, Config) ->
    application:set_env(opentelemetry, processors, [{otel_batch_processor, #{scheduled_delay_ms => 1000000}}]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    %% adds an exporter for a new table
    %% spans will be exported to a separate table for each of the test cases
    Tid = ets:new(exported_spans, [public, bag]),
    otel_batch_processor:set_exporter(otel_exporter_tab, Tid),
    [{tid, Tid} | Config];
init_per_testcase(dropped_attributes, Config) ->
    application:set_env(opentelemetry, processors, [{otel_batch_processor, #{scheduled_delay_ms => 1}}]),
    application:set_env(opentelemetry, attribute_value_length_limit, 2),
    {ok, _} = application:ensure_all_started(opentelemetry),
    %% adds an exporter for a new table
    %% spans will be exported to a separate table for each of the test cases
    Tid = ets:new(exported_spans, [public, bag]),
    otel_batch_processor:set_exporter(otel_exporter_tab, Tid),
    [{tid, Tid} | Config];
init_per_testcase(too_many_attributes, Config) ->
    application:set_env(opentelemetry, processors, [{otel_batch_processor, #{scheduled_delay_ms => 1}}]),
    application:set_env(opentelemetry, attribute_count_limit, 2),
    {ok, _} = application:ensure_all_started(opentelemetry),
    %% adds an exporter for a new table
    %% spans will be exported to a separate table for each of the test cases
    Tid = ets:new(exported_spans, [public, bag]),
    otel_batch_processor:set_exporter(otel_exporter_tab, Tid),
    [{tid, Tid} | Config];
init_per_testcase(tracer_instrumentation_library, Config) ->
    application:set_env(opentelemetry, processors, [{otel_batch_processor, #{scheduled_delay_ms => 1}}]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    %% adds an exporter for a new table
    %% spans will be exported to a separate table for each of the test cases
    Tid = ets:new(exported_spans, [public, bag]),
    otel_batch_processor:set_exporter(otel_exporter_tab, Tid),
    [{tid, Tid} | Config];
init_per_testcase(_, Config) ->
    Processor = ?config(processor, Config),
    application:set_env(opentelemetry, processors, [{Processor, #{scheduled_delay_ms => 1}}]),
    {ok, _} = application:ensure_all_started(opentelemetry),
    %% adds an exporter for a new table
    %% spans will be exported to a separate table for each of the test cases
    Tid = ets:new(exported_spans, [public, bag]),
    Processor:set_exporter(otel_exporter_tab, Tid),
    [{tid, Tid} | Config].

end_per_testcase(disable_auto_creation, _Config) ->
    _ = application:stop(opentelemetry),
    _ = application:unload(opentelemetry),
    ok;
end_per_testcase(old_disable_auto_creation, _Config) ->
    _ = application:stop(opentelemetry),
    _ = application:unload(opentelemetry),
    ok;
end_per_testcase(propagator_configuration_with_os_env, _Config) ->
    os:unsetenv("OTEL_PROPAGATORS"),
    _ = application:stop(opentelemetry),
    ok;
end_per_testcase(_, _Config) ->
    application:unset_env(opentelemetry, attribute_value_length_limit),
    _ = application:stop(opentelemetry),
    ok.

disable_auto_creation(_Config) ->
    {_, #tracer{instrumentation_library=Library}} = opentelemetry:get_tracer(
                                                      opentelemetry:get_application(kernel)),
    ?assertEqual(undefined, Library),
    ok.

old_disable_auto_creation(_Config) ->
    {_, #tracer{instrumentation_library=Library}} = opentelemetry:get_tracer(
                                                      opentelemetry:get_application(kernel)),
    ?assertEqual(undefined, Library),
    ok.

application_tracers(_Config) ->
    {_, #tracer{instrumentation_library=Library}} = opentelemetry:get_tracer(
                                                      opentelemetry:get_application(kernel)),
    ?assertEqual(<<"kernel">>, Library#instrumentation_library.name),

    %% tracers are unique by name/version/schema_url
    NewKernelTracer = opentelemetry:get_tracer(kernel, <<"fake-version">>, undefined),
    {_, #tracer{instrumentation_library=NewLibrary}} = NewKernelTracer,
    ?assertEqual(<<"kernel">>, NewLibrary#instrumentation_library.name),
    ?assertEqual(<<"fake-version">>, NewLibrary#instrumentation_library.version),
    ?assertEqual(undefined, NewLibrary#instrumentation_library.schema_url),

    Tracer = opentelemetry:get_tracer(kernel, <<"fake-version">>, <<"http://schema.org/myschema">>),
    {_, #tracer{instrumentation_library=NewLibrary1}} = Tracer,
    ?assertEqual(<<"kernel">>, NewLibrary1#instrumentation_library.name),
    ?assertEqual(<<"fake-version">>, NewLibrary1#instrumentation_library.version),
    ?assertEqual(<<"http://schema.org/myschema">>, NewLibrary1#instrumentation_library.schema_url),

    ok.

propagator_configuration(_Config) ->
    ?assertEqual({otel_propagator_text_map_composite,
                  [{otel_propagator_b3, b3multi}, otel_propagator_baggage]},
                 opentelemetry:get_text_map_extractor()),
    ?assertEqual({otel_propagator_text_map_composite,
                  [{otel_propagator_b3, b3multi}, otel_propagator_baggage]},
                 opentelemetry:get_text_map_injector()),

    opentelemetry:set_text_map_extractor({otel_propagator_baggage, []}),

    ?assertEqual({otel_propagator_baggage, []}, opentelemetry:get_text_map_extractor()),
    ?assertEqual({otel_propagator_text_map_composite,
                  [{otel_propagator_b3, b3multi}, otel_propagator_baggage]},
                 opentelemetry:get_text_map_injector()),

    opentelemetry:set_text_map_injector({{otel_propagator_b3, b3multi}, []}),

    ?assertEqual({otel_propagator_baggage, []}, opentelemetry:get_text_map_extractor()),
    ?assertEqual({{otel_propagator_b3, b3multi}, []}, opentelemetry:get_text_map_injector()),

    opentelemetry:set_text_map_injector({{otel_propagator_b3, b3single}, []}),

    ?assertEqual({otel_propagator_baggage, []}, opentelemetry:get_text_map_extractor()),
    ?assertEqual({{otel_propagator_b3, b3single}, []}, opentelemetry:get_text_map_injector()),

    ok.

propagator_configuration_with_os_env(_Config) ->
    ?assertEqual({otel_propagator_text_map_composite,
                  [otel_propagator_trace_context]}, opentelemetry:get_text_map_extractor()),
    ?assertEqual({otel_propagator_text_map_composite,
                  [otel_propagator_trace_context]}, opentelemetry:get_text_map_injector()),

    opentelemetry:set_text_map_extractor({otel_propagator_baggage, []}),

    ?assertEqual({otel_propagator_baggage, []}, opentelemetry:get_text_map_extractor()),
    ?assertEqual({otel_propagator_text_map_composite,
                  [otel_propagator_trace_context]}, opentelemetry:get_text_map_injector()),

    opentelemetry:set_text_map_injector({{otel_propagator_b3, b3multi}, []}),

    ?assertEqual({otel_propagator_baggage, []}, opentelemetry:get_text_map_extractor()),
    ?assertEqual({{otel_propagator_b3, b3multi}, []}, opentelemetry:get_text_map_injector()),

    opentelemetry:set_text_map_injector({{otel_propagator_b3, b3single}, []}),

    ?assertEqual({otel_propagator_baggage, []}, opentelemetry:get_text_map_extractor()),
    ?assertEqual({{otel_propagator_b3, b3single}, []}, opentelemetry:get_text_map_injector()),

    ok.

force_flush(Config) ->
    Tid = ?config(tid, Config),
    SpanCtx1 = ?start_span(<<"span-1">>),

    %% start_span does not modify the context
    ?assertMatch(undefined, ?current_span_ctx),
    ?set_current_span(SpanCtx1),

    %% since SpanCtx1 was set to the current span it will be the parent
    SpanCtx2 = ?start_span(<<"span-2">>),
    ?set_current_span(SpanCtx2),

    ?assertMatch(SpanCtx2, ?current_span_ctx),
    otel_span:end_span(SpanCtx2),

    ?set_current_span(SpanCtx1),
    ?assertMatch(SpanCtx1, ?current_span_ctx),

    Attr1 = <<"attr-1">>,
    AttrValue1 = <<"attr-value-1">>,
    ?set_attribute(Attr1, AttrValue1),

    otel_span:end_span(SpanCtx1),

    otel_tracer_provider:force_flush(),

    %% wouldn't be exported at this point unless force flush worked
    [Span1] = assert_exported(Tid, SpanCtx1),

    ?assertEqual(#{Attr1 => AttrValue1}, otel_attributes:map(Span1#span.attributes)),

    ok.


macros(Config) ->
    Tid = ?config(tid, Config),

    SpanCtx1 = ?start_span(<<"span-1">>),

    %% start_span does not modify the context
    ?assertMatch(undefined, ?current_span_ctx),
    ?set_current_span(SpanCtx1),

    %% since SpanCtx1 was set to the current span it will be the parent
    SpanCtx2 = ?start_span(<<"span-2">>),
    ?set_current_span(SpanCtx2),

    ?assertMatch(SpanCtx2, ?current_span_ctx),
    otel_span:end_span(SpanCtx2),

    ?set_current_span(SpanCtx1),
    ?assertMatch(SpanCtx1, ?current_span_ctx),

    Attr1 = <<"attr-1">>,
    AttrValue1 = <<"attr-value-1">>,
    ?set_attribute(Attr1, AttrValue1),

    otel_span:end_span(SpanCtx1),

    [Span1] = assert_exported(Tid, SpanCtx1),

    ?assertEqual(#{Attr1 => AttrValue1}, otel_attributes:map(Span1#span.attributes)),

    ok.

with_span(Config) ->
    Tid = ?config(tid, Config),

    Tracer = opentelemetry:get_tracer(),

    SpanCtx1 = ?start_span(<<"span-1">>),
    otel_tracer:set_current_span(SpanCtx1),

    Result = some_result,
    ?assertMatch(Result, otel_tracer:with_span(Tracer, <<"with-span-2">>, #{},
                                               fun(SpanCtx2) ->
                                                       ?assertNotEqual(SpanCtx1, SpanCtx2),
                                                       ?assertEqual(SpanCtx2, ?current_span_ctx),
                                                       Result
                                               end)),

    ?assertMatch(SpanCtx1, ?current_span_ctx),

    otel_span:end_span(SpanCtx1),
    [_Span1] = assert_exported(Tid, SpanCtx1),

    ok.

child_spans(Config) ->
    Tid = ?config(tid, Config),

    EarlierTimestamp = opentelemetry:timestamp(),

    %% start a span and 2 children
    SpanCtx1 = ?start_span(<<"span-1">>),
    ?set_current_span(SpanCtx1),
    SpanCtx2 = ?start_span(<<"span-2">>),
    ?set_current_span(SpanCtx2),
    SpanCtx3 = ?start_span(<<"span-3">>),
    ?set_current_span(SpanCtx3),

    %% end the 3rd span
    ?assertMatch(SpanCtx3, ?current_span_ctx),
    otel_span:end_span(SpanCtx3),

    assert_exported(Tid, SpanCtx3),

    %% 3rd span should still be the current span ctx
    %% even though it is ended and not able to be updated
    ?assertMatch(SpanCtx3, ?current_span_ctx),

    ?set_current_span(SpanCtx2),

    %% start another child of the 2nd span
    %% with a timestamp sent as part of the start opts
    SpanCtx4 = ?start_span(<<"span-4">>, #{start_time => EarlierTimestamp}),
    ?set_current_span(SpanCtx4),
    ?assertMatch(SpanCtx4, ?current_span_ctx),

    %% end 4th span and 2nd should be current
    otel_span:end_span(SpanCtx4),

    ?set_current_span(SpanCtx2),
    ?assertMatch(SpanCtx2, ?current_span_ctx),

    %% end 2th span and 1st should be current
    otel_span:end_span(SpanCtx2),
    ?set_current_span(SpanCtx1),
    ?assertMatch(SpanCtx1, ?current_span_ctx),

    %% end first and no span should be current ctx
    otel_span:end_span(SpanCtx1),
    ?set_current_span(undefined),
    ?assertMatch(undefined, ?current_span_ctx),

    assert_all_exported(Tid, [SpanCtx1, SpanCtx2, SpanCtx3]),

    [Span4] = assert_exported(Tid, SpanCtx4),

    ?assertEqual(EarlierTimestamp, Span4#span.start_time).

update_span_data(Config) ->
    Tid = ?config(tid, Config),

    Links = [#link{trace_id=0,
                   span_id=0,
                   attributes=[],
                   tracestate=[]}],

    SpanCtx1=#span_ctx{trace_id=TraceId,
                       span_id=SpanId} = ?start_span(<<"span-1">>, #{links => Links}),
    ?set_current_span(SpanCtx1),
    ?set_attribute(<<"key-1">>, <<"value-1">>),

    Events = opentelemetry:events([{erlang:system_time(nanosecond),
                                    <<"event-name">>, []}]),
    Status = opentelemetry:status(0, <<"status">>),

    otel_span:set_status(SpanCtx1, Status),

    otel_span:add_events(SpanCtx1, Events),

    ?assertMatch(SpanCtx1, ?current_span_ctx),
    otel_span:end_span(SpanCtx1),

    ?UNTIL_NOT_EQUAL([], ets:match(Tid, #span{trace_id=TraceId,
                                              span_id=SpanId,
                                              %% TODO: compare this another way since
                                              %% attributes are now a record hidden behind a module
                                              %% attributes=[{<<"key-1">>, <<"value-1">>}],
                                              %% links=Links,
                                              status=Status,
                                              %% events=Events,
                                              _='_'})).

tracer_instrumentation_library(Config) ->
    Tid = ?config(tid, Config),

    TracerName = tracer1,
    TracerVsn = <<"1.0.0">>,
    Tracer = {_, #tracer{instrumentation_library=IL}} =
        opentelemetry:get_tracer(TracerName, TracerVsn, "http://schema.org/myschema"),

    ?assertMatch({instrumentation_library,<<"tracer1">>,<<"1.0.0">>,<<"http://schema.org/myschema">>},
                 IL),

    SpanCtx1 = otel_tracer:start_span(Tracer, <<"span-1">>, #{}),

    otel_span:end_span(SpanCtx1),

    [Span1] = assert_exported(Tid, SpanCtx1),

    ?assertMatch({instrumentation_library,<<"tracer1">>,<<"1.0.0">>,<<"http://schema.org/myschema">>},
                 Span1#span.instrumentation_library).

%% check that ending a span results in the tracer setting the previous tracer context
%% as the current active and not use the parent span ctx of the span being ended --
%% though at times those will be the same.
tracer_previous_ctx(Config) ->
    Tid = ?config(tid, Config),

    Tracer = opentelemetry:get_tracer(),

    SpanCtx1 = otel_tracer:start_span(Tracer, <<"span-1">>, #{}),
    ?set_current_span(SpanCtx1),
    ?assertMatch(SpanCtx1, ?current_span_ctx),

    %% create a span that is not on the current context and with no parent
    SpanCtx2 = otel_tracer:start_span(Tracer, <<"span-2">>, #{}),
    Ctx = otel_tracer:set_current_span(otel_ctx:new(), SpanCtx2),
    %% start a new span with SpanCtx2 as the parent
    SpanCtx3 = otel_tracer:start_span(Ctx, Tracer, <<"span-3">>, #{}),

    %% end SpanCtx3, even though it isn't the parent SpanCtx1
    otel_span:end_span(SpanCtx3),

    ?assertEqual(SpanCtx1, ?current_span_ctx),

    otel_span:end_span(SpanCtx1),

    ?set_current_span(SpanCtx2),
    otel_span:end_span(SpanCtx2),

    assert_all_exported(Tid, [SpanCtx3, SpanCtx1, SpanCtx2]),

    ok.

attach_ctx(Config) ->
    Tid = ?config(tid, Config),

    Tracer = opentelemetry:get_tracer(),

    SpanCtx1 = otel_tracer:start_span(Tracer, <<"span-1">>, #{}),
    ?set_current_span(SpanCtx1),
    ?assertMatch(SpanCtx1, ?current_span_ctx),

    %% create a span that is not set to active and with no parent
    SpanCtx2 = otel_tracer:start_span(otel_ctx:new(), Tracer, <<"span-2">>, #{}),
    Ctx = otel_ctx:get_current(),

    erlang:spawn(fun() ->
                         otel_ctx:attach(Ctx),
                         ?set_current_span(SpanCtx2),
                         otel_span:end_span(SpanCtx2)
                 end),

    otel_span:end_span(SpanCtx1),

    assert_all_exported(Tid, [SpanCtx1, SpanCtx2]),

    ok.

reset_after(Config) ->
    Tid = ?config(tid, Config),

    Tracer = opentelemetry:get_tracer(),

    SpanCtx1 = otel_tracer:start_span(Tracer, <<"span-1">>, #{}),
    ?set_current_span(SpanCtx1),
    ?assertMatch(SpanCtx1, ?current_span_ctx),

    Ctx = otel_ctx:get_current(),

    try
        %% start but don't end a span
        _SpanCtx2 = otel_tracer:start_span(Tracer, <<"span-2">>, #{})
    after
        otel_ctx:attach(Ctx)
    end,

    otel_span:end_span(SpanCtx1),

    assert_all_exported(Tid, [SpanCtx1]),

    ok.

stop_temporary_app(_Config) ->
    SpanCtx1 = ?start_span(<<"span-1">>),
    ?assertNotMatch(#span_ctx{trace_id=0,
                              span_id=0}, SpanCtx1),

    ok = application:stop(opentelemetry),

    %% stopping opentelemetry resets the tracer to a noop
    SpanCtx2 = ?start_span(<<"span-2">>),
    ?assertMatch(#span_ctx{trace_id=0,
                           span_id=0}, SpanCtx2),
    ok.

default_sampler(_Config) ->
    Tracer = opentelemetry:get_tracer(),

    %% root span should be sampled by default sampler
    SpanCtx1 = otel_tracer:start_span(Tracer, <<"span-1">>, #{}),
    ?assertMatch(true, SpanCtx1#span_ctx.is_recording),

    %% hack to set the created span as local not sampled
    ?set_current_span(SpanCtx1#span_ctx{is_remote=false,
                                        is_recording=false,
                                        trace_flags=0}),

    %% local not sampled should default to not sampled
    SpanCtx2 = otel_tracer:start_span(Tracer, <<"span-2">>, #{}),
    ?assertMatch(false, SpanCtx2#span_ctx.is_recording),

    %% hack to set the created span as local not sampled
    ?set_current_span(SpanCtx1#span_ctx{is_remote=false,
                                        is_recording=true,
                                        trace_flags=1}),

    %% local not sampled should default to not sampled
    SpanCtx3 = otel_tracer:start_span(Tracer, <<"span-3">>, #{}),
    ?assertMatch(true, SpanCtx3#span_ctx.is_recording),

    %% hack to set the created span as remote not sampled
    ?set_current_span(SpanCtx1#span_ctx{is_remote=true,
                                        is_recording=false,
                                        trace_flags=0}),

    %% remote not sampled should default to not sampled
    SpanCtx4 = otel_tracer:start_span(Tracer, <<"span-4">>, #{}),
    ?assertMatch(false, SpanCtx4#span_ctx.is_recording),

    %% hack to set the created span as remote not sampled
    ?set_current_span(SpanCtx1#span_ctx{is_remote=true,
                                        is_recording=true,
                                        trace_flags=1}),

    %% remote not sampled should default to not sampled
    SpanCtx5 = otel_tracer:start_span(Tracer, <<"span-5">>, #{}),
    ?assertMatch(true, SpanCtx5#span_ctx.is_recording),

    %% hack to set the created span as local not sampled but recording
    ?set_current_span(SpanCtx1#span_ctx{is_remote=false,
                                        is_recording=true,
                                        trace_flags=0}),

    %% local not sampled but is recorded should default to sampled
    SpanCtx6 = otel_tracer:start_span(Tracer, <<"span-6">>, #{}),
    ?assertMatch(false, SpanCtx6#span_ctx.is_recording),
    ok.

non_recording_ets_table(_Config) ->
    Tracer={TracerModule, TracerConfig} = opentelemetry:get_tracer(),

    SpanCtx1 = otel_tracer:start_span(Tracer, <<"span-1">>, #{}),
    ?assertMatch(true, SpanCtx1#span_ctx.is_recording),

    AlwaysOff = otel_sampler:new(always_off),
    Tracer1 = {TracerModule, TracerConfig#tracer{sampler=AlwaysOff}},
    SpanCtx2 = otel_tracer:start_span(Tracer1, <<"span-2">>, #{}),
    ?assertMatch(false, SpanCtx2#span_ctx.is_recording),

    %% verify that ETS table only contains the recording span <<"span-1">>
    ?assertMatch([#span{name = <<"span-1">>}], ets:tab2list(?SPAN_TAB)),
    ok.

root_span_sampling_always_off(_Config) ->
    Tracer={TracerModule, TracerConfig} = opentelemetry:get_tracer(),

    Sampler = otel_sampler:new(always_off),
    Tracer1 = {TracerModule, TracerConfig#tracer{sampler=Sampler}},

    SpanCtx1 = otel_tracer:start_span(Tracer1, <<"span-1">>, #{}),
    ?assertMatch(false, SpanCtx1#span_ctx.is_recording),
    ?assertMatch(0, SpanCtx1#span_ctx.trace_flags),

    otel_tracer:set_current_span(SpanCtx1),
    SpanCtx2 = otel_tracer:start_span(Tracer, <<"span-2">>, #{}),
    ?assertMatch(false, SpanCtx2#span_ctx.is_recording),
    ?assertMatch(0, SpanCtx2#span_ctx.trace_flags),

    ok.

root_span_sampling_always_on(_Config) ->
    Tracer={TracerModule, TracerConfig} = opentelemetry:get_tracer(),

    Sampler = otel_sampler:new(always_on),
    Tracer1 = {TracerModule, TracerConfig#tracer{sampler=Sampler}},

    SpanCtx1 = otel_tracer:start_span(Tracer1, <<"span-1">>, #{}),
    ?assertMatch(true, SpanCtx1#span_ctx.is_recording),
    ?assertMatch(1, SpanCtx1#span_ctx.trace_flags),

    otel_tracer:set_current_span(SpanCtx1),
    SpanCtx2 = otel_tracer:start_span(Tracer, <<"span-2">>, #{}),
    ?assertMatch(true, SpanCtx2#span_ctx.is_recording),
    ?assertMatch(1, SpanCtx1#span_ctx.trace_flags),

    ok.

record_but_not_sample(Config) ->
    ct:comment("Test that a Span that the sampler returns RECORD_ONLY for gets created"
               "as a valid recorded span but is not sent to the exporter."),
    Tid = ?config(tid, Config),

    Sampler = otel_sampler:new({static_sampler, #{<<"span-record-and-sample">> => ?RECORD_AND_SAMPLE,
                                                    <<"span-record">> => ?RECORD_ONLY}}),

    {Module, Tracer0}  = opentelemetry:get_tracer(),
    Tracer = {Module, Tracer0#tracer{sampler=Sampler}},

    SpanCtx1 = otel_tracer:start_span(Tracer, <<"span-record-and-sample">>, #{}),
    ?assertEqual(true, SpanCtx1#span_ctx.is_recording),
    ?assertEqual(1, SpanCtx1#span_ctx.trace_flags),

    ?set_current_span(SpanCtx1),
    ?assertMatch(SpanCtx1, ?current_span_ctx),

    SpanCtx2 = otel_tracer:start_span(Tracer, <<"span-record">>, #{}),
    ?assertEqual(true, SpanCtx2#span_ctx.is_recording),
    ?assertEqual(0, SpanCtx2#span_ctx.trace_flags),

    ?set_current_span(SpanCtx2),
    otel_span:end_span(SpanCtx2),

    otel_span:end_span(SpanCtx1),

    assert_all_exported(Tid, [SpanCtx1]),

    %% span-2 is recorded but not sampled, so should not show up in the export table
    assert_not_exported(Tid, SpanCtx2),

    ok.

record_exception_works(Config) ->
    Tid = ?config(tid, Config),
    SpanCtx = ?start_span(<<"span-1">>),
    try
        throw(my_error)
    catch
        Class:Term:Stacktrace ->
            otel_span:record_exception(SpanCtx, Class, Term, Stacktrace, [{<<"some-attribute">>, <<"value">>}]),
            otel_span:end_span(SpanCtx),
            [Span] = assert_exported(Tid, SpanCtx),
            [Event] = otel_events:list(Span#span.events),
            ?assertEqual(<<"exception">>, Event#event.name),
            ?assertEqual(#{<<"exception.type">> => <<"throw:my_error">>,
                           <<"exception.stacktrace">> => list_to_binary(io_lib:format("~p", [Stacktrace], [{chars_limit, 50}])),
                           <<"some-attribute">> => <<"value">>},
                         otel_attributes:map(Event#event.attributes)),
            ok
    end.

record_exception_with_message_works(Config) ->
    Tid = ?config(tid, Config),
    SpanCtx = ?start_span(<<"span-1">>),
    try
        throw(my_error)
    catch
        Class:Term:Stacktrace ->
            otel_span:record_exception(SpanCtx, Class, Term, <<"My message">>, Stacktrace, [{<<"some-attribute">>, <<"value">>}]),
            otel_span:end_span(SpanCtx),
            [Span] = assert_exported(Tid, SpanCtx),
            [Event] = otel_events:list(Span#span.events),
            ?assertEqual(<<"exception">>, Event#event.name),
            ?assertEqual(#{<<"exception.type">> => <<"throw:my_error">>,
                           <<"exception.stacktrace">> => list_to_binary(io_lib:format("~p", [Stacktrace], [{chars_limit, 50}])),
                           <<"exception.message">> => <<"My message">>,
                           <<"some-attribute">> => <<"value">>},
                         otel_attributes:map(Event#event.attributes)
                        ),
            ok
    end.

dropped_attributes(Config) ->
    Tid = ?config(tid, Config),
    SpanCtx = ?start_span(<<"span-1">>),

    ?set_current_span(SpanCtx),

    ?set_attribute(<<"attr-1">>, <<"attr-value-1">>),
    ?set_attribute(<<"attr-2">>, {not_allowed, in, attributes}),

    otel_span:end_span(SpanCtx),
    [Span] = assert_exported(Tid, SpanCtx),

    ?assertEqual(#{<<"attr-1">> => <<"at">>}, otel_attributes:map(Span#span.attributes)),

    ok.

too_many_attributes(Config) ->
    Tid = ?config(tid, Config),
    SpanCtx = ?start_span(<<"span-1">>),

    ?set_current_span(SpanCtx),

    ?set_attribute(<<"attr-1">>, <<"attr-value-1">>),

    %% dropped because of tuple as value
    ?set_attribute(<<"attr-2-dropped">>, {not_allowed, in, attributes}),

    ?set_attribute(<<"attr-3">>, <<"attr-value-3">>),

    %% dropped because count limit was set to 2
    ?set_attribute(<<"attr-4">>, <<"attr-value-4">>),
    %% won't be dropped because attr-1 already exists so it overrides the value
    ?set_attribute(<<"attr-1">>, <<"attr-value-5">>),

    otel_span:end_span(SpanCtx),
    [Span] = assert_exported(Tid, SpanCtx),

    ?assertEqual(#{<<"attr-1">> => <<"attr-value-5">>,
                   <<"attr-3">> => <<"attr-value-3">>}, otel_attributes:map(Span#span.attributes)),
    ?assertEqual(1, otel_attributes:dropped(Span#span.attributes)),

    %% test again using the `set_attributes' macro
    SpanCtx2 = ?start_span(<<"span-2">>),

    ?set_current_span(SpanCtx2),

    ?set_attributes(#{<<"attr-1">> => <<"attr-value-1">>,
                      <<"attr-2">> => {not_allowed, in, attributes},
                      <<"attr-3">> => <<"attr-value-3">>,
                      <<"attr-4">> => <<"attr-value-4">>}),

    %% won't be dropped because attr-1 already exists so it overrides the value
    ?set_attributes(#{<<"attr-1">> => <<"attr-value-5">>}),

    otel_span:end_span(SpanCtx2),
    [Span2] = assert_exported(Tid, SpanCtx2),


    %% order isn't guaranteed so just verify the number dropped is right
    ?assertEqual(1, otel_attributes:dropped(Span2#span.attributes)),

    ok.

%%

assert_all_exported(Tid, SpanCtxs) ->
    [assert_exported(Tid, SpanCtx) || SpanCtx <- SpanCtxs].

assert_exported(Tid, #span_ctx{trace_id=TraceId,
                               span_id=SpanId}) ->
    ?UNTIL_NOT_EQUAL([], ets:match_object(Tid, #span{trace_id=TraceId,
                                                     span_id=SpanId,
                                                     _='_'})).

assert_not_exported(Tid, #span_ctx{trace_id=TraceId,
                                   span_id=SpanId}) ->
    %% sleep so exporter has run before we check
    %% since we can't do like when checking it exists with UNTIL
    timer:sleep(100),
    ?assertMatch([], ets:match(Tid, #span{trace_id=TraceId,
                                          span_id=SpanId,
                                          _='_'})).

