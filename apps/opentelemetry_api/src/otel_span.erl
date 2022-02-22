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
         hex_trace_id/1,
         hex_span_id/1,
         tracestate/1,
         is_recording/1,
         is_valid/1,
         is_valid_name/1,
         process_attributes/1,
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
-define(is_allowed_key(Key), is_atom(Key) orelse (is_binary(Key) andalso Key =/= <<"">>)).
-define(is_allowed_value(Value), is_atom(Value) orelse
                                 is_boolean(Value) orelse
                                 is_number(Value) orelse
                                 is_binary(Value) orelse
                                 is_list(Value)).

-type start_opts() :: #{attributes => opentelemetry:attributes_map(),
                        links => [opentelemetry:link()],
                        is_recording => boolean(),
                        start_time => opentelemetry:timestamp(),
                        kind => opentelemetry:span_kind()}.

-export_type([start_opts/0]).

-spec validate_start_opts(start_opts()) -> start_opts().
validate_start_opts(Opts) when is_map(Opts) ->
    Attributes = maps:get(attributes, Opts, #{}),
    Links = maps:get(links, Opts, []),
    Kind = maps:get(kind, Opts, ?SPAN_KIND_INTERNAL),
    StartTime = maps:get(start_time, Opts, opentelemetry:timestamp()),
    IsRecording = maps:get(is_recording, Opts, true),
    #{
      attributes => process_attributes(Attributes),
      links => Links,
      kind => Kind,
      start_time => StartTime,
      is_recording => IsRecording
     }.

-spec is_recording(SpanCtx) -> boolean() when
      SpanCtx :: opentelemetry:span_ctx().
is_recording(SpanCtx) ->
    ?is_recording(SpanCtx).

-spec is_valid(SpanCtx) -> boolean() when
      SpanCtx :: opentelemetry:span_ctx().
is_valid(#span_ctx{trace_id=TraceId,
                   span_id=SpanId}) when TraceId =/= 0 ,
                                         SpanId =/= 0 ->
    true;
is_valid(_) ->
    false.

-spec is_valid_attribute(opentelemetry:attribute_key(), opentelemetry:attribute_value()) -> boolean().
is_valid_attribute(Key, Value) when is_tuple(Value) , ?is_allowed_key(Key) ->
    is_valid_attribute(Key, tuple_to_list(Value));
%% lists as attribute values must be primitive types and homogeneous
is_valid_attribute(Key, [Value1 | _Rest] = Values) when is_binary(Value1) , ?is_allowed_key(Key) ->
    lists:all(fun is_binary/1, Values);
is_valid_attribute(Key, [Value1 | _Rest] = Values) when is_boolean(Value1) , ?is_allowed_key(Key) ->
    lists:all(fun is_boolean/1, Values);
is_valid_attribute(Key, [Value1 | _Rest] = Values) when is_atom(Value1) , ?is_allowed_key(Key) ->
    lists:all(fun is_valid_atom_value/1, Values);
is_valid_attribute(Key, [Value1 | _Rest] = Values) when is_integer(Value1) , ?is_allowed_key(Key) ->
    lists:all(fun is_integer/1, Values);
is_valid_attribute(Key, [Value1 | _Rest] = Values) when is_float(Value1) , ?is_allowed_key(Key) ->
    lists:all(fun is_float/1, Values);
is_valid_attribute(_Key, Value) when is_list(Value) ->
    false;
is_valid_attribute(Key, []) when ?is_allowed_key(Key) ->
    true;
is_valid_attribute(Key, Value) when ?is_allowed_key(Key) , ?is_allowed_value(Value) ->
    true;
is_valid_attribute(_, _) ->
    false.

is_valid_atom_value(undefined) ->
    false;
is_valid_atom_value(nil) ->
    false;
is_valid_atom_value(Value) ->
    is_atom(Value) andalso (is_boolean(Value) == false).

-spec process_attributes(any()) -> opentelemetry:attributes_map().
process_attributes(Attributes) when is_map(Attributes) ->
    maps:fold(fun process_attribute/3, #{}, Attributes);
process_attributes([]) -> #{};
process_attributes(Attributes) when is_list(Attributes) ->
    process_attributes(maps:from_list(Attributes));
process_attributes(_) ->
    #{}.

process_attribute(Key, Value, Map) when is_tuple(Value) ->
    List = tuple_to_list(Value),
    case is_valid_attribute(Key, List) of
        true ->
            maps:put(Key, List, Map);
        false ->
            Map
    end;
process_attribute(Key, Value, Map) ->
    case is_valid_attribute(Key, Value) of
        true ->
            maps:put(Key, Value, Map);
        false ->
            Map
    end.
-spec is_valid_name(any()) -> boolean().
is_valid_name(undefined) ->
    false;
is_valid_name(Name) when is_atom(Name) orelse (is_binary(Name) andalso Name =/= <<"">>) ->
    true;
is_valid_name(_) ->
    false.

%% accessors
-spec trace_id(opentelemetry:span_ctx()) -> opentelemetry:trace_id().
trace_id(#span_ctx{trace_id=TraceId }) ->
    TraceId.

-spec span_id(opentelemetry:span_ctx()) -> opentelemetry:span_id().
span_id(#span_ctx{span_id=SpanId }) ->
    SpanId.

-spec hex_trace_id(opentelemetry:span_ctx()) -> opentelemetry:hex_trace_id().
hex_trace_id(#span_ctx{trace_id=TraceId}) ->
    iolist_to_binary(io_lib:format("~32.16.0b", [TraceId])).

-spec hex_span_id(opentelemetry:span_ctx()) -> opentelemetry:hex_span_id().
hex_span_id(#span_ctx{span_id=SpanId}) ->
    iolist_to_binary(io_lib:format("~16.16.0b", [SpanId])).

-spec tracestate(opentelemetry:span_ctx() | undefined) -> opentelemetry:tracestate().
tracestate(#span_ctx{tracestate=Tracestate}) ->
    Tracestate;
tracestate(_) ->
    [].

-spec set_attribute(SpanCtx, Key, Value) -> boolean() when
      Key :: opentelemetry:attribute_key(),
      Value :: opentelemetry:attribute_value(),
      SpanCtx :: opentelemetry:span_ctx().
set_attribute(SpanCtx=#span_ctx{span_sdk={Module, _}}, Key, Value) when ?is_recording(SpanCtx) , is_tuple(Value) ->
    List = tuple_to_list(Value),
    case is_valid_attribute(Key, List) of
        true ->
            Module:set_attribute(SpanCtx, Key, List);
        false ->
            false
    end;
set_attribute(SpanCtx=#span_ctx{span_sdk={Module, _}}, Key, Value) when ?is_recording(SpanCtx) ->
    case is_valid_attribute(Key, Value) of
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
    Module:set_attributes(SpanCtx, process_attributes(Attributes));
set_attributes(_, _) ->
    false.

-spec add_event(SpanCtx, Name, Attributes) -> boolean() when
      Name :: opentelemetry:event_name(),
      Attributes :: opentelemetry:attributes_map(),
      SpanCtx :: opentelemetry:span_ctx().
add_event(SpanCtx=#span_ctx{span_sdk={Module, _}}, Name, Attributes)
  when ?is_recording(SpanCtx) ,
       (is_list(Attributes) orelse is_map(Attributes)) ->
    case is_valid_name(Name) of
        true ->
            Module:add_event(SpanCtx, Name, process_attributes(Attributes));
        false ->
            false
    end;
add_event(_, _, _) ->
    false.

%% todo - validate
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
record_exception(SpanCtx, Class, Term, Stacktrace, Attributes) ->
    ExceptionAttributes = [{<<"exception.type">>, iolist_to_binary(io_lib:format("~0tP:~0tP", [Class, 10, Term, 10], [{chars_limit, 50}]))},
                           {<<"exception.stacktrace">>, iolist_to_binary(io_lib:format("~0tP", [Stacktrace, 10], [{chars_limit, 50}]))}],
    add_event(SpanCtx, <<"exception">>, ExceptionAttributes ++ Attributes).

-spec record_exception(SpanCtx, Class, Term,  Message, Stacktrace, Attributes) -> boolean() when
      SpanCtx :: opentelemetry:span_ctx(),
      Class :: atom(),
      Term :: term(),
      Message :: unicode:unicode_binary(),
      Stacktrace :: list(any()),
      Attributes :: opentelemetry:attributes_map().
record_exception(SpanCtx, Class, Term, Message, Stacktrace, Attributes) ->
    ExceptionAttributes = [{<<"exception.type">>, iolist_to_binary(io_lib:format("~0tP:~0tP", [Class, 10, Term, 10], [{chars_limit, 50}]))},
                           {<<"exception.stacktrace">>, iolist_to_binary(io_lib:format("~0tP", [Stacktrace, 10], [{chars_limit, 50}]))},
                           {<<"exception.message">>, Message}],
    add_event(SpanCtx, <<"exception">>, ExceptionAttributes ++ Attributes).

-spec set_status(SpanCtx, Status) -> boolean() when
      Status :: opentelemetry:status(),
      SpanCtx :: opentelemetry:span_ctx().
set_status(SpanCtx=#span_ctx{span_sdk={Module, _}}, Status) when ?is_recording(SpanCtx) ->
    Module:set_status(SpanCtx, Status);
set_status(SpanCtx=#span_ctx{span_sdk={Module, _}}, Code) when ?is_recording(SpanCtx) andalso
                                                               (Code =:= ?OTEL_STATUS_UNSET orelse
                                                                Code =:= ?OTEL_STATUS_OK orelse
                                                                Code =:= ?OTEL_STATUS_ERROR)->
    Module:set_status(SpanCtx, opentelemetry:status(Code));
set_status(_, _) ->
    false.

-spec set_status(SpanCtx, Code, Message) -> boolean() when
      Code :: opentelemetry:status_code(),
      Message :: unicode:unicode_binary(),
      SpanCtx :: opentelemetry:span_ctx().
set_status(SpanCtx, Code, Message) ->
    set_status(SpanCtx, opentelemetry:status(Code, Message)).

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

-spec end_span(SpanCtx) -> SpanCtx when
      SpanCtx :: opentelemetry:span_ctx().
end_span(SpanCtx=#span_ctx{span_sdk={Module, _}}) when ?is_recording(SpanCtx) ->
    _ = Module:end_span(SpanCtx, undefined),
    SpanCtx#span_ctx{is_recording=false};
end_span(SpanCtx) ->
    SpanCtx.

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
