-module(opentelemetry_zipkin).

-export([init/1,
         export/3,
         shutdown/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/otel_span.hrl").
-include("opentelemetry_zipkin_pb.hrl").

-define(DEFAULT_ZIPKIN_ADDRESS, "http://localhost:9411/api/v2/spans").
-define(DEFAULT_LOCAL_ENDPOINT, #{service_name => node()}).

-record(state, {address :: string(),
                endpoint :: map()}).

init(Opts) ->
    Address = zipkin_address(Opts),
    LocalEndpoint = local_endpoint(Opts),
    {ok, #state{address=Address,
                endpoint=LocalEndpoint}}.

export(Tab, Resource, #state{address=Address,
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
    end.

shutdown(_) ->
    ok.

%%


zipkin_span(Span, LocalEndpoint) ->
    #zipkin_span{
       trace_id = <<(Span#span.trace_id):128>>,
       name=iolist_to_binary(Span#span.name),
       id = <<(Span#span.span_id):64>>,
       timestamp=opentelemetry:convert_timestamp(Span#span.start_time, microsecond),
       duration=erlang:convert_time_unit(Span#span.end_time - Span#span.start_time, native, microsecond),
       %% debug=false, %% TODO: get from attributes?
       %% shared=false, %% TODO: get from attributes?
       kind=to_kind(Span#span.kind),
       parent_id=to_parent_id(Span#span.parent_span_id),
       annotations=to_annotations(Span#span.events),
       tags=to_tags(Span#span.attributes),
       local_endpoint=LocalEndpoint
       %% remote_endpoint %% TODO: get from attributes?
     }.

to_annotations(TimeEvents) ->
    to_annotations(TimeEvents, []).

to_annotations([], Annotations) ->
    Annotations;
to_annotations([#event{system_time_nano=Timestamp,
                       name=Name,
                       attributes=Attributes} | Rest], Annotations) ->
    to_annotations(Rest, [#zipkin_annotation{timestamp=erlang:convert_time_unit(Timestamp, nanosecond, microsecond),
                                             value=annotation_value(Name, Attributes)} | Annotations]).

annotation_value(Name, Attributes) ->
    AttrString = lists:join(", ", [[Key, ": ", to_string(Value)] ||
                                      {Key, Value} <- Attributes]),
    iolist_to_binary([Name, ": {", AttrString, "}"]).

to_string(Value) when is_function(Value) ->
    to_string(Value());
to_string(Value) when is_list(Value) ;
                      is_binary(Value) ->
    Value;
to_string(Value) ->
    io_lib:format("~p", [Value]).

to_binary_string(Value) when is_function(Value) ->
    to_binary_string(Value());
to_binary_string(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary_string(Value) when is_integer(Value) ->
    integer_to_binary(Value);
to_binary_string(Value) when is_float(Value) ->
    float_to_binary(Value);
to_binary_string(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_binary_string(Value) ->
    Value.

to_tags(Attributes) ->
    lists:map(fun({Name, Value}) ->
                     {to_binary_string(Name), to_binary_string(Value)}
              end, Attributes).

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
    case lists:keyfind(<<"service.name">>, 1, Attributes) of
        {<<"service.name">>, ServiceName} ->
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
