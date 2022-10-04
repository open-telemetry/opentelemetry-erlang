-module(opentelemetry_zipkin).

-export([init/1,
         export/4,
         shutdown/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").
-include("opentelemetry_zipkin_pb.hrl").
-include_lib("gradualizer/include/gradualizer.hrl").

-define(DEFAULT_ZIPKIN_ADDRESS, "http://localhost:9411/api/v2/spans").
-define(DEFAULT_LOCAL_ENDPOINT, #{service_name => node()}).

-record(state, {address :: string(),
                endpoint :: #zipkin_endpoint{}}).

init(Opts) ->
    Address = zipkin_address(Opts),
    LocalEndpoint = local_endpoint(Opts),
    {ok, #state{address=Address,
                endpoint=LocalEndpoint}}.

export(traces, Tab, Resource, #state{address=Address,
                                     endpoint=LocalEndpoint}) ->
    Attributes = otel_resource:attributes(Resource),
    LocalEndpoint1 = local_endpoint_from_resource(Attributes, LocalEndpoint),
    ZSpans = ets:foldl(fun(Span, Acc) ->
                               try zipkin_span(Span, LocalEndpoint1) of
                                   ZipkinSpan ->
                                       [ZipkinSpan | Acc]
                               catch
                                   C:T:S ->
                                       %% failed to encode
                                       ?LOG_DEBUG("failed to encode span to Zipkin format ~p:~p ~p", [C, T, S]),
                                       Acc
                               end
                       end, [], Tab),

    case ZSpans of
        [] ->
            %% nothing to send
            ok;
        _ ->
            Proto = opentelemetry_zipkin_pb:encode_msg(#zipkin_list_of_spans{spans=ZSpans}),
            case httpc:request(post, {Address, [], "application/x-protobuf", Proto}, [], []) of
                {ok, {{_, Code, _}, _, _}} when Code >= 200 andalso Code =< 202 ->
                    ok;
                {ok, {{_, Code, _}, _, Message}} ->
                    ?LOG_INFO("error response from service exported to status=~p ~p",
                            [Code, Message]),
                    error;
                {error, Reason} ->
                    ?LOG_INFO("client error exporting spans ~p", [Reason]),
                    error
            end
    end;
export(Type, _Tab, _Resource, _State) ->
    ?LOG_INFO("Unable to export data of type ~p with the Zipkin exporter. "
              "Zipkin only supports traces", [Type]),
    error.


shutdown(_) ->
    ok.

%%


zipkin_span(Span, LocalEndpoint) ->
    StartTime = ?assert_type(Span#span.start_time, opentelemetry:timestamp()),
    EndTime = ?assert_type(Span#span.end_time, non_neg_integer()),
    Timestamp = ?assert_type(opentelemetry:convert_timestamp(StartTime, microsecond), non_neg_integer()),
    Duration = ?assert_type(erlang:convert_time_unit(EndTime - StartTime, native, microsecond), non_neg_integer()),
    #zipkin_span{
       trace_id = <<(?assert_type(Span#span.trace_id, opentelemetry:trace_id())):128>>,
       name=to_binary_string(Span#span.name),
       id = <<(?assert_type(Span#span.span_id, opentelemetry:span_id())):64>>,
       timestamp=Timestamp,
       duration=Duration,
       %% debug=false, %% TODO: get from attributes?
       %% shared=false, %% TODO: get from attributes?
       kind=to_kind(Span#span.kind),
       parent_id=to_parent_id(Span#span.parent_span_id),
       annotations=to_annotations(otel_events:list(Span#span.events)),
       tags=to_tags(Span),
       local_endpoint=LocalEndpoint
       %% remote_endpoint %% TODO: get from attributes?
     }.

to_annotations(TimeEvents) ->
    to_annotations(TimeEvents, []).

to_annotations([], Annotations) ->
    Annotations;
to_annotations([#event{system_time_native=Timestamp,
                       name=Name,
                       attributes=Attributes} | Rest], Annotations) ->
    to_annotations(Rest, [#zipkin_annotation{timestamp=?assert_type(erlang:convert_time_unit(Timestamp, native, microsecond), non_neg_integer()),
                                             value=annotation_value(Name, Attributes)} | Annotations]).

annotation_value(Name, Attributes) ->
    Annos = maps:fold(fun(Key, Value, Acc) ->
                              [Key, ": ", to_string(Value) | Acc]
                      end, [], otel_attributes:map(Attributes)),
    AttrString = lists:join(", ", Annos),
    iolist_to_binary([Name, ": {", AttrString, "}"]).

to_string(Value) when is_function(Value) ->
    to_string(Value());
to_string(Value) when is_list(Value) ;
                      is_binary(Value) ->
    Value;
to_string(Value) ->
    io_lib:format("~tp", [Value]).

to_binary_string(Value) when is_function(Value) ->
    to_binary_string(Value());
to_binary_string(Value) when is_integer(Value) ->
    integer_to_binary(Value);
to_binary_string(Value) when is_float(Value) ->
    float_to_binary(Value);
to_binary_string(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_binary_string(Value) when is_binary(Value) ->
    Value;
to_binary_string(Value) ->
    %% attributes already check that the value is a binary string
    %% and not a list string, so any other term, like a list,
    %% can just be turned into a string of term
    case unicode:characters_to_binary(io_lib:format("~tp", [Value])) of
        S when is_binary(S) ->
            S;
        _ ->
            %% {error, _, _} or {incomplete, _, _}
            <<>>
    end.

to_tags(Span) ->
    status_to_tags(Span#span.status) ++ attributes_to_tags(Span#span.attributes).

status_to_tags(#status{code=?OTEL_STATUS_ERROR,
                       message=Message}) ->
    [{<<"otel.status_code">>, <<"ERROR">>}, {<<"error">>, Message}];
status_to_tags(#status{code=?OTEL_STATUS_OK}) ->
    [{<<"otel.status_code">>, <<"OK">>}];
status_to_tags(_) ->
    [].

attributes_to_tags(Attributes) ->
    maps:fold(fun(Name, Value, Acc) ->
                     [{to_binary_string(Name), to_binary_string(Value)} | Acc]
              end, [], otel_attributes:map(Attributes)).

to_parent_id(undefined) ->
    undefined;
to_parent_id(ParentId) ->
    <<ParentId:64>>.

to_kind(undefined) ->
    'SPAN_KIND_UNSPECIFIED';
to_kind(?SPAN_KIND_INTERNAL) ->
    'SPAN_KIND_UNSPECIFIED';
to_kind(?SPAN_KIND_PRODUCER) ->
    'PRODUCER';
to_kind(?SPAN_KIND_CONSUMER) ->
    'CONSUMER';
to_kind(?SPAN_KIND_SERVER) ->
    'SERVER';
to_kind(?SPAN_KIND_CLIENT) ->
    'CLIENT'.

zipkin_address(Options) ->
    maps:get(address, Options, ?DEFAULT_ZIPKIN_ADDRESS).

local_endpoint_from_resource(Attributes, LocalEndpoint) ->
    case maps:find(<<"service.name">>, otel_attributes:map(Attributes)) of
        {ok, ServiceName} ->
            LocalEndpoint#zipkin_endpoint{service_name=ServiceName};
        _ ->
            LocalEndpoint
    end.

local_endpoint(Options) ->
    LocalEndpoint = maps:get(local_endpoint, Options, ?DEFAULT_LOCAL_ENDPOINT),
    #zipkin_endpoint{service_name=to_string(maps:get(service_name, LocalEndpoint, node())),
                     ipv4=ip_to_string(maps:get(ipv4, LocalEndpoint, undefined)),
                     ipv6=ip_to_string(maps:get(ipv6, LocalEndpoint, undefined)),
                     port=maps:get(port, LocalEndpoint, 0)}.

ip_to_string(undefined) ->
    undefined;
ip_to_string(IP) when is_tuple(IP) ->
    case inet:ntoa(IP) of
        {error, einval} ->
            to_string(IP);
        IPString ->
            IPString
    end;
ip_to_string(IP) ->
    to_string(IP).
