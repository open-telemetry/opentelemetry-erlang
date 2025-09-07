-module(opentelemetry_zipkin_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").

all() ->
    [verify_export].

%% insert a couple spans and export to locally running zipkin
verify_export(_Config) ->
    Address = "http://localhost:9411/api/v2/spans",
    Resource = otel_resource:create([{"service.name",
                                      "my-test-service"}]),
    {ok, State} = opentelemetry_zipkin:init(#{address => Address,
                                             local_endpoint => #{service_name => my_service,
                                                                 ip4 => {1,2,3,4},
                                                                 port => 8000}}),
    Tid = ets:new(span_tab, [{keypos, #span.span_id}]),

    ?assertMatch(ok, opentelemetry_zipkin:export(traces, Tid, Resource, State)),

    TraceId = otel_id_generator:generate_trace_id(),
    SpanId = otel_id_generator:generate_span_id(),

    Events = otel_events:new(128, 128, 128),

    ParentSpan =
        #span{name = <<"span-1">>,
              trace_id = TraceId,
              span_id = SpanId,
              kind = ?SPAN_KIND_CLIENT,
              start_time = opentelemetry:timestamp(),
              end_time = opentelemetry:timestamp(),
              links = otel_links:new([], 128, 128, 128),
              events = otel_events:add([#event{system_time_native=opentelemetry:timestamp(),
                                               name = <<"event-1">>,
                                               attributes = [{<<"attr-1">>, <<"value-1">>}]},
                                        #event{system_time_native=opentelemetry:timestamp(),
                                               name = event_2,
                                               attributes = [{<<"attr-3">>, <<"value-3">>}]}], Events),
              attributes = otel_attributes:new([{<<"attr-2">>, <<"value-2">>},
                                                {attr_3, true},
                                                {<<"map-key-1">>, #{<<"map-key-1">> => 123}},
                                                {<<"list-key-1">>, [3.14, 9.345]}
                                                ], 128, 128),
              status=opentelemetry:status(?SPAN_KIND_INTERNAL, <<"some message about status">>),
              parent_span_is_remote = undefined},
    true = ets:insert(Tid, ParentSpan),

    ChildSpan = #span{name = <<"span-2">>,
                      trace_id = TraceId,
                      span_id = otel_id_generator:generate_span_id(),
                      parent_span_id = SpanId,
                      kind = ?SPAN_KIND_SERVER,
                      start_time = opentelemetry:timestamp(),
                      end_time = opentelemetry:timestamp(),
                      events = otel_events:add([#event{system_time_native=opentelemetry:timestamp(),
                                                       name = <<"event-1">>,
                                                       attributes = [{<<"attr-1">>, <<"value-1">>}]},
                                                #event{system_time_native=opentelemetry:timestamp(),
                                                       name = event_2,
                                                       attributes = [{<<"attr-3">>, <<"value-3">>}]}], Events),
                      attributes = otel_attributes:new([{<<"attr-2">>, <<"value-2">>},
                                                        {attr_3, true},
                                                        {<<"map-key-1">>, #{<<"map-key-1">> => 123}},
                                                        {<<"list-key-1">>, [3.14, 9.345]}
                                                       ], 128, 128),
                      parent_span_is_remote = false},
    true = ets:insert(Tid, ChildSpan),

    ?assertMatch(ok, opentelemetry_zipkin:export(traces, Tid, Resource, State)),
    ok.
