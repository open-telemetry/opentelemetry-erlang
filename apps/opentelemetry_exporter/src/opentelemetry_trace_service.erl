%%%-------------------------------------------------------------------
%% @doc Client module for grpc service opentelemetry.proto.collector.trace.v1.TraceService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2020-07-02T14:11:39+00:00 and should not be modified manually

-module(opentelemetry_trace_service).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'opentelemetry.proto.collector.trace.v1.TraceService').
-define(PROTO_MODULE, 'opentelemetry_exporter_trace_service_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec export(opentelemetry_exporter_trace_service_pb:export_trace_service_request()) ->
    {ok, opentelemetry_exporter_trace_service_pb:export_trace_service_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
export(Input) ->
    export(ctx:new(), Input, #{}).

-spec export(ctx:t() | opentelemetry_exporter_trace_service_pb:export_trace_service_request(), opentelemetry_exporter_trace_service_pb:export_trace_service_request() | grpcbox_client:options()) ->
    {ok, opentelemetry_exporter_trace_service_pb:export_trace_service_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
export(Ctx, Input) when ?is_ctx(Ctx) ->
    export(Ctx, Input, #{});
export(Input, Options) ->
    export(ctx:new(), Input, Options).

-spec export(ctx:t(), opentelemetry_exporter_trace_service_pb:export_trace_service_request(), grpcbox_client:options()) ->
    {ok, opentelemetry_exporter_trace_service_pb:export_trace_service_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
export(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/opentelemetry.proto.collector.trace.v1.TraceService/Export">>, Input, ?DEF(export_trace_service_request, export_trace_service_response, <<"opentelemetry.proto.collector.trace.v1.ExportTraceServiceRequest">>), Options).

