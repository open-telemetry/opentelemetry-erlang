%%%------------------------------------------------------------------------
%% Copyright 2019, OpenTelemetry Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc
%% Span behaviour.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_span).

-export([trace_id/1,
         span_id/1,
         hex_span_ctx/1,
         hex_trace_id/1,
         hex_span_id/1,
         tracestate/1,
         is_recording/1,
         is_valid/1,
         is_valid_name/1,
         validate_start_opts/1,
         set_attribute/3,
         set_attributes/2,
         add_event/3,
         add_events/2,
         record_exception/5,
         record_exception/6,
         set_status/2,
         set_status/3,
         update_name/2,
         end_span/1,
         end_span/2]).

-include("opentelemetry.hrl").

-define(is_recording(SpanCtx), SpanCtx =/= undefined andalso SpanCtx#span_ctx.is_recording =:= true).

-type start_opts() :: #{attributes => opentelemetry:attributes_map(),
                        links => [opentelemetry:link()],
                        is_recording => boolean(),
                        start_time => opentelemetry:timestamp(),
                        kind => opentelemetry:span_kind()}.
-type start_config() :: #{attributes := opentelemetry:attributes_map(),
                            links := [opentelemetry:link()],
                            is_recording := boolean(),
                            start_time := opentelemetry:timestamp(),
                            kind := opentelemetry:span_kind()}.
%% Start options for a span.

-export_type([start_opts/0, start_config/0]).

%% @doc Validates the start options for a span and fills in defaults.
-spec validate_start_opts(start_opts()) -> start_config().
validate_start_opts(Opts) when is_map(Opts) ->
    Attributes = maps:get(attributes, Opts, #{}),
    Links = maps:get(links, Opts, []),
    Kind = maps:get(kind, Opts, ?SPAN_KIND_INTERNAL),
    StartTime = maps:get(start_time, Opts, opentelemetry:timestamp()),
    IsRecording = maps:get(is_recording, Opts, true),
    #{
      attributes => otel_attributes:process_attributes(Attributes),
      links => Links,
      kind => Kind,
      start_time => StartTime,
      is_recording => IsRecording
     }.

%% @doc Returns whether the span is recording.
-spec is_recording(SpanCtx) -> boolean() when
      SpanCtx :: opentelemetry:span_ctx() | undefined.
is_recording(SpanCtx) ->
    ?is_recording(SpanCtx).

%% @doc Returns whether the span context is valid.
-spec is_valid(SpanCtx) -> boolean() when
      SpanCtx :: opentelemetry:span_ctx().
is_valid(#span_ctx{trace_id=TraceId,
                   span_id=SpanId}) when TraceId =/= 0 ,
                                         SpanId =/= 0 ->
    true;
is_valid(_) ->
    false.

%% @private
-spec is_valid_name(any()) -> boolean().
is_valid_name(undefined) ->
    false;
is_valid_name(Name) when is_atom(Name) orelse (is_binary(Name) andalso Name =/= <<"">>) ->
    true;
is_valid_name(_) ->
    false.

%% accessors

%% @doc Returns the trace ID of the given span context.
-spec trace_id(opentelemetry:span_ctx()) -> opentelemetry:trace_id().
trace_id(#span_ctx{trace_id=TraceId}) ->
    TraceId.

%% @doc Returns the span ID of the given span context.
-spec span_id(opentelemetry:span_ctx()) -> opentelemetry:span_id().
span_id(#span_ctx{span_id=SpanId}) ->
    SpanId.

%% keys are prefixed with `otel_' because the main use of this function is logger metadata
-spec hex_span_ctx(opentelemetry:span_ctx() | undefined) -> #{otel_trace_id := unicode:charlist(),
                                                              otel_span_id := unicode:charlist(),
                                                              otel_trace_flags := unicode:charlist()} | #{}.
hex_span_ctx(#span_ctx{trace_id=TraceId,
                       span_id=SpanId,
                       trace_flags=TraceFlags}) ->
    #{otel_trace_id => io_lib:format("~32.16.0b", [TraceId]),
      otel_span_id => io_lib:format("~16.16.0b", [SpanId]),
      otel_trace_flags => case TraceFlags band 1 of 1 -> "01"; _ -> "00" end};
hex_span_ctx(_) ->
    #{}.

-spec hex_trace_id(opentelemetry:span_ctx()) -> opentelemetry:hex_trace_id().
hex_trace_id(#span_ctx{trace_id=TraceId}) ->
    case otel_utils:format_binary_string("~32.16.0b", [TraceId]) of
        {ok, Binary} ->
            Binary;
        _ ->
            <<>>
    end.

-spec hex_span_id(opentelemetry:span_ctx()) -> opentelemetry:hex_span_id().
hex_span_id(#span_ctx{span_id=SpanId}) ->
    case otel_utils:format_binary_string("~16.16.0b", [SpanId]) of
        {ok, Binary} ->
            Binary;
        _ ->
            <<>>
    end.

-spec tracestate(opentelemetry:span_ctx() | undefined) -> otel_tracestate:t().
tracestate(#span_ctx{tracestate=Tracestate}) ->
    Tracestate;
tracestate(_) ->
    otel_tracestate:new().

-spec set_attribute(SpanCtx, Key, Value) -> boolean() when
      Key :: opentelemetry:attribute_key(),
      Value :: opentelemetry:attribute_value(),
      SpanCtx :: opentelemetry:span_ctx().
set_attribute(SpanCtx=#span_ctx{span_sdk={Module, _}}, Key, Value) when ?is_recording(SpanCtx) , is_tuple(Value) ->
    List = tuple_to_list(Value),
    case otel_attributes:is_valid_attribute(Key, List) of
        true ->
            Module:set_attribute(SpanCtx, Key, List);
        false ->
            false
    end;
set_attribute(SpanCtx=#span_ctx{span_sdk={Module, _}}, Key, Value) when ?is_recording(SpanCtx) ->
    case otel_attributes:is_valid_attribute(Key, Value) of
        true ->
            Module:set_attribute(SpanCtx, Key, Value);
        false ->
            false
    end;
set_attribute(_, _, _) ->
    false.

-spec set_attributes(SpanCtx, Attributes) -> boolean() when
      Attributes :: opentelemetry:attributes_map(),
      SpanCtx :: opentelemetry:span_ctx().
set_attributes(SpanCtx=#span_ctx{span_sdk={Module, _}}, Attributes) when ?is_recording(SpanCtx),
                                                                         (is_list(Attributes) orelse is_map(Attributes)) ->
    Module:set_attributes(SpanCtx, otel_attributes:process_attributes(Attributes));
set_attributes(_, _) ->
    false.

%% @doc Adds an event to the given span context.
%%
%% Returns `false' if the given span context is not recording, or if the event `Name' is
%% not valid.
-spec add_event(SpanCtx, Name, Attributes) -> boolean() when
      Name :: opentelemetry:event_name(),
      Attributes :: opentelemetry:attributes_map(),
      SpanCtx :: opentelemetry:span_ctx().
add_event(SpanCtx=#span_ctx{span_sdk={Module, _}}, Name, Attributes)
  when ?is_recording(SpanCtx) ,
       (is_list(Attributes) orelse is_map(Attributes)) ->
    case is_valid_name(Name) of
        true ->
            Module:add_event(SpanCtx, Name, otel_attributes:process_attributes(Attributes));
        false ->
            false
    end;
add_event(_, _, _) ->
    false.

%% @doc Same as {@link add_event/3}, but takes a list of events.
%%
%% Returns `false' if the given span context is not recording.
-spec add_events(SpanCtx, Events) -> boolean() when
      Events :: [opentelemetry:event()],
      SpanCtx :: opentelemetry:span_ctx().
add_events(SpanCtx=#span_ctx{span_sdk={Module, _}}, Events) when ?is_recording(SpanCtx) , is_list(Events)  ->
    Module:add_events(SpanCtx, Events);
add_events(_, _) ->
    false.

-spec record_exception(SpanCtx, Class, Term, Stacktrace, Attributes) -> boolean() when
      SpanCtx :: opentelemetry:span_ctx(),
      Class :: atom(),
      Term :: term(),
      Stacktrace :: list(any()),
      Attributes :: opentelemetry:attributes_map().
record_exception(SpanCtx, Class, Term, Stacktrace, Attributes) when is_list(Attributes) ->
    record_exception(SpanCtx, Class, Term, Stacktrace, maps:from_list(Attributes));
record_exception(SpanCtx, Class, Term, Stacktrace, Attributes) when is_map(Attributes) ->
    {ok, ExceptionType} = otel_utils:format_binary_string("~0tP:~0tP", [Class, 10, Term, 10], [{chars_limit, 50}]),
    {ok, StacktraceString} = otel_utils:format_binary_string("~0tP", [Stacktrace, 10], [{chars_limit, 50}]),
    ExceptionAttributes = #{'exception.type' => ExceptionType,
                            'exception.stacktrace' => StacktraceString},
    add_event(SpanCtx, 'exception', maps:merge(ExceptionAttributes, Attributes));
record_exception(_, _, _, _, _) ->
    false.

-spec record_exception(SpanCtx, Class, Term,  Message, Stacktrace, Attributes) -> boolean() when
      SpanCtx :: opentelemetry:span_ctx(),
      Class :: atom(),
      Term :: term(),
      Message :: unicode:unicode_binary(),
      Stacktrace :: list(any()),
      Attributes :: opentelemetry:attributes_map().
record_exception(SpanCtx, Class, Term, Message, Stacktrace, Attributes) when is_list(Attributes) ->
    record_exception(SpanCtx, Class, Term, Message, Stacktrace, maps:from_list(Attributes));
record_exception(SpanCtx, Class, Term, Message, Stacktrace, Attributes) when is_map(Attributes) ->
    {ok, ExceptionType} = otel_utils:format_binary_string("~0tP:~0tP", [Class, 10, Term, 10], [{chars_limit, 50}]),
    {ok, StacktraceString} = otel_utils:format_binary_string("~0tP", [Stacktrace, 10], [{chars_limit, 50}]),
    ExceptionAttributes = #{'exception.type' => ExceptionType,
                            'exception.stacktrace' => StacktraceString,
                            'exception.message' => Message},
    add_event(SpanCtx, 'exception', maps:merge(ExceptionAttributes, Attributes));
record_exception(_, _, _, _, _, _) ->
    false.

-spec set_status(SpanCtx, StatusOrCode) -> boolean() when
      StatusOrCode :: opentelemetry:status() | undefined | opentelemetry:status_code(),
      SpanCtx :: opentelemetry:span_ctx().
set_status(SpanCtx=#span_ctx{span_sdk={Module, _}}, Code) when ?is_recording(SpanCtx) andalso
                                                               (Code =:= ?OTEL_STATUS_UNSET orelse
                                                                Code =:= ?OTEL_STATUS_OK orelse
                                                                Code =:= ?OTEL_STATUS_ERROR)->
    Module:set_status(SpanCtx, opentelemetry:status(Code));
set_status(SpanCtx=#span_ctx{span_sdk={Module, _}}, undefined) when ?is_recording(SpanCtx) ->
    Module:set_status(SpanCtx, opentelemetry:status(?OTEL_STATUS_UNSET));
set_status(SpanCtx=#span_ctx{span_sdk={Module, _}}, Status) when ?is_recording(SpanCtx) ->
    Module:set_status(SpanCtx, Status);
set_status(_, _) ->
    false.

-spec set_status(SpanCtx, Code, Message) -> boolean() when
      Code :: opentelemetry:status_code(),
      Message :: unicode:unicode_binary(),
      SpanCtx :: opentelemetry:span_ctx().
set_status(SpanCtx, Code, Message) ->
    set_status(SpanCtx, opentelemetry:status(Code, Message)).

%% @doc Updates the name of the given span context to `Name'.
%%
%% Returns `false' if the given span context is not recording, or if the name `Name' is not valid.
-spec update_name(SpanCtx, Name) -> boolean() when
      Name :: opentelemetry:span_name(),
      SpanCtx :: opentelemetry:span_ctx().
update_name(SpanCtx=#span_ctx{span_sdk={Module, _}}, SpanName) when ?is_recording(SpanCtx) ->
    case is_valid_name(SpanName) of
        true ->
            Module:update_name(SpanCtx, SpanName);
        false ->
            false
    end;
update_name(_, _) ->
    false.

%% @doc Ends the given span context.
%%
%% If `SpanCtx' is not recording, this function doesn't do anything.
%% Returns the updated span context.
-spec end_span(SpanCtx) -> SpanCtx when
      SpanCtx :: opentelemetry:span_ctx().
end_span(SpanCtx=#span_ctx{span_sdk={Module, _}}) when ?is_recording(SpanCtx) ->
    _ = Module:end_span(SpanCtx, undefined),
    SpanCtx#span_ctx{is_recording=false};
end_span(SpanCtx) ->
    SpanCtx.

%% @doc Ends the given span context with the given timestamp.
%%
%% If `SpanCtx' is not recording, this function doesn't do anything.
%% If `Timestamp' is `undefined', this is equivalent to {@link end_span/1}.
%% Returns the updated span context.
-spec end_span(SpanCtx, Timestamp) -> SpanCtx when
    SpanCtx :: opentelemetry:span_ctx(),
    Timestamp :: integer() | undefined.
end_span(SpanCtx=#span_ctx{span_sdk={Module, _}}, Timestamp) when ?is_recording(SpanCtx)
                                                                  , is_integer(Timestamp) ->
    _ = Module:end_span(SpanCtx, Timestamp),
    SpanCtx#span_ctx{is_recording=false};
end_span(SpanCtx=#span_ctx{span_sdk={Module, _}}, _Timestamp) when ?is_recording(SpanCtx) ->
    _ = Module:end_span(SpanCtx),
    SpanCtx#span_ctx{is_recording=false};
end_span(SpanCtx, _) ->
    SpanCtx.
