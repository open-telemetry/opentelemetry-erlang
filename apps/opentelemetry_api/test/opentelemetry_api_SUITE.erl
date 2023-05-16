%% sets the API without an SDK installed
%% basic propagation must work without an SDK and this is not yet implemented
-module(opentelemetry_api_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

-include("opentelemetry.hrl").
-include("otel_tracer.hrl").

all() ->
    [noop_tracer,
     validations,
     update_span_data,
     noop_with_span,
     can_create_link_from_span,
     hex_trace_ids].

init_per_suite(Config) ->
    application:load(opentelemetry_api),
    %% this used to be done in the SDK `stop'
    %% need it here in case SDK tests were run before these
    opentelemetry:set_default_tracer({otel_tracer_noop, []}),
    Config.

end_per_suite(_Config) ->
    ok.

can_create_link_from_span(_Config) ->
    %% start a span to create a link to
    SpanCtx = ?start_span(<<"span-1">>),

    %% extract individual values from span context
    TraceId = otel_span:trace_id(SpanCtx),
    SpanId = otel_span:span_id(SpanCtx),
    Tracestate = otel_span:tracestate(SpanCtx),

    %% end span, so there's no current span set
    ?end_span(opentelemetry:timestamp()),

    Attributes = #{<<"attr-1">> => <<"value-1">>},

    ?assertMatch(undefined, opentelemetry:link(undefined)),
    ?assertMatch(undefined, opentelemetry:link(undefined, Attributes)),

    ?assertMatch(#{trace_id := TraceId,
                   span_id := SpanId,
                   attributes := Attributes,
                   tracestate := Tracestate},
                 opentelemetry:link(TraceId, SpanId, Attributes, Tracestate)),

    ?assertMatch(#{trace_id := TraceId,
                   span_id := SpanId,
                   attributes := #{},
                   tracestate := Tracestate},
                 opentelemetry:link(SpanCtx)),

    ?assertMatch(#{trace_id := TraceId,
                   span_id := SpanId,
                   attributes := Attributes,
                   tracestate := Tracestate},
                 opentelemetry:link(SpanCtx, Attributes)),

    ?assertMatch([#{trace_id := TraceId,
                    span_id := SpanId,
                    attributes := Attributes,
                    tracestate := Tracestate},
                  #{trace_id := TraceId,
                    span_id := SpanId,
                    attributes := #{},
                    tracestate := Tracestate}],
                 opentelemetry:links([undefined, {SpanCtx, Attributes}, SpanCtx])).

validations(_Config) ->
    InvalidAttributesArg = undefined,
    ?assertMatch(#{}, otel_span:process_attributes(InvalidAttributesArg)),

    Attributes = [
                  {<<"key-1">>, <<"value-1">>},
                  {key2, 1},
                  {key3, true},
                  {<<"key-3">>, #{<<"value-2">> => <<"maps-unallowed">>}},
                  {<<"key-4">>, 1.0},
                  {key5, {t, <<"value-5">>}},
                  {key6, {1,2,3}},
                  {<<"atom-list">>, [a, b]},
                  {<<"binary-list">>, {<<"a">>, <<"b">>}},
                  {<<"boolean-list">>, [true, false]},
                  {<<"float-list">>, [1.1, 2.0]},
                  {<<"int-list">>, [1,2,3]},
                  {<<"unallowed-type">>, [self()]},
                  {<<"atom-list-invalid">>, [a, false]},
                  {<<"binary-list-invalid">>, [<<"a">>, a]},
                  {<<"boolean-list-invalid">>, [true, 1.1]},
                  {<<"float-list-invalid">>, [1.1, 2]},
                  {<<"int-list-invalid">>, [1, 2.0]}],
    Links = [{<<0:128>>, <<0:64>>, Attributes, []}],
    Events = [{opentelemetry:timestamp(), <<"timed-event-name">>, Attributes},
              {untimed_event, Attributes},
              {<<"">>, Attributes},
              {123, Attributes}],
    ProcessedAttributes = otel_span:process_attributes(Attributes),

    ?assertMatch(#{key2 := 1,
                   key3 := true,
                   key6 := {1,2,3},
                   <<"atom-list">> := [a, b],
                   <<"binary-list">> := {<<"a">>, <<"b">>},
                   <<"boolean-list">> := [true, false],
                   <<"float-list">> := [1.1, 2.0],
                   <<"int-list">> := [1,2,3],
                   <<"key-1">> := <<"value-1">>,
                   <<"key-4">> := 1.0},
                ProcessedAttributes),

    ?assertMatch([key2,
                  key3,
                  key6,
                  <<"atom-list">>,
                  <<"binary-list">>,
                  <<"boolean-list">>,
                  <<"float-list">>,
                  <<"int-list">>,
                  <<"key-1">>,
                  <<"key-4">>], lists:sort(maps:keys(ProcessedAttributes))),

    ?assertMatch([#{name := <<"timed-event-name">>,
                   attributes := ProcessedAttributes},
                  #{name := untimed_event,
                   attributes := ProcessedAttributes}],
                 opentelemetry:events(Events)),

    ?assertMatch([#{trace_id := <<0:128>>,
                   span_id := <<0:64>>,
                   attributes := ProcessedAttributes,
                   tracestate := []}],
                opentelemetry:links(Links)),

    StartOpts = #{attributes => Attributes,
                 links => opentelemetry:links(Links)},
    ?assertMatch(#{attributes := ProcessedAttributes,
                  links := [#{trace_id := <<0:128>>, span_id := <<0:64>>, attributes := ProcessedAttributes, tracestate := []}]},
                otel_span:validate_start_opts(StartOpts)),

    %% names
    ?assert(otel_span:is_valid_name(name)),
    ?assert(otel_span:is_valid_name(<<"name">>)),
    ?assertNot(otel_span:is_valid_name(<<"">>)),
    ?assertNot(otel_span:is_valid_name(undefined)),
    ?assertNot(otel_span:is_valid_name(123)),
    ok.

noop_tracer(_Config) ->
    %% start a span and 2 children
    SpanCtx1 = ?start_span(<<"span-1">>),
    SpanCtx2 = ?start_span(<<"span-2">>),
    SpanCtx3 = ?start_span(<<"span-3">>),

    %% set to current and then end the 3rd span
    ?set_current_span(SpanCtx3),
    ?assertMatch(SpanCtx3, ?current_span_ctx),
    otel_span:end_span(SpanCtx3),

    ?set_current_span(SpanCtx2),
    ?assertMatch(SpanCtx2, ?current_span_ctx),

    %% start another child of the 2nd span
    SpanCtx4 = ?start_span(<<"span-4">>),

    ?set_current_span(SpanCtx4),
    ?assertMatch(SpanCtx4, ?current_span_ctx),

    %% end 4th span
    otel_span:end_span(SpanCtx4),

    ?set_current_span(SpanCtx2),
    ?assertMatch(SpanCtx2, ?current_span_ctx),

    %% end 2th span
    ?end_span(opentelemetry:timestamp()),

    ?set_current_span(SpanCtx1),
    ?assertMatch(SpanCtx1, ?current_span_ctx),

    %% end first and no span should be current ctx
    ?end_span(opentelemetry:timestamp()),

    %% 1st span is ended but still current
    ?assertMatch(SpanCtx1, ?current_span_ctx).

%% just shouldn't crash
update_span_data(_Config) ->
    Links = opentelemetry:links([#{trace_id => 0,
               span_id => 0,
               attributes => [],
               tracestate => []}]),

    SpanCtx1 = ?start_span(<<"span-1">>, #{links => Links}),
    ?set_current_span(SpanCtx1),

    ?set_attribute(<<"key-1">>, <<"value-1">>),
    ?add_event(<<"event-1">>, [{<<"attr-1">>, <<"value-1">>}]),

    Events = opentelemetry:events([{opentelemetry:timestamp(),
                                    <<"timed-event-name">>, []}]),
    ErrorStatus = opentelemetry:status(?OTEL_STATUS_ERROR, <<"This is an error!">>),
    ?assertMatch(#status{code = ?OTEL_STATUS_ERROR, message = <<"This is an error!">>}, ErrorStatus),

    %% for unset and ok status status/2 and status/1 should return the same record
    UnsetStatus = opentelemetry:status(?OTEL_STATUS_UNSET, <<"This is a message">>),
    UnsetStatus = opentelemetry:status(?OTEL_STATUS_UNSET),
    ?assertMatch(#status{code = ?OTEL_STATUS_UNSET, message = <<"">>}, UnsetStatus),

    OkStatus = opentelemetry:status(?OTEL_STATUS_OK, <<"This is Ok">>),
    OkStatus = opentelemetry:status(?OTEL_STATUS_OK),
    ?assertMatch(#status{code = ?OTEL_STATUS_OK, message = <<"">>}, OkStatus),

    otel_span:set_status(SpanCtx1, UnsetStatus),
    otel_span:set_status(SpanCtx1, undefined),
    otel_span:set_status(SpanCtx1, ?OTEL_STATUS_ERROR, <<"this is not ok">>),
    otel_span:set_status(SpanCtx1, ?OTEL_STATUS_OK),
    otel_span:add_events(SpanCtx1, Events),

    ?assertMatch(SpanCtx1, ?current_span_ctx),
    ?end_span(opentelemetry:timestamp()),

    ?assertMatch(#span_ctx{is_recording=false}, ?current_span_ctx),

    ok.

noop_with_span(_Config) ->
    Attributes = #{<<"attr-1">> => <<"value-1">>},
    Links = opentelemetry:links([#{trace_id => 0,
               span_id => 0,
               attributes => [],
               tracestate => []}]),
    StartOpts = #{attributes => Attributes, links => Links},

    Tracer = opentelemetry:get_tracer(),
    ?assertMatch({otel_tracer_noop, _}, Tracer),

    Result = some_result,
    ?assertEqual(Result, otel_tracer:with_span(Tracer, <<"span1">>, StartOpts, fun(_) -> Result end)),
    ok.

hex_trace_ids(_Config) ->
    SpanCtx=#span_ctx{trace_id = <<19,25,104,206,49,198,60,63,69,245,232,137,234,183,74,97>>,
                      span_id = <<161,20,190,33,18,16,115,223>>},
    ?assertEqual(<<"131968CE31C63C3F45F5E889EAB74A61">>, otel_span:hex_trace_id(SpanCtx)),
    ?assertEqual(<<"A114BE21121073DF">>, otel_span:hex_span_id(SpanCtx)),
    ok.
