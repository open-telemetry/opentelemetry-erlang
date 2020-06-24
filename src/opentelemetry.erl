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
%% @doc The types defined here, and referencing records in opentelemetry.hrl
%% are used to store trace information while being collected on the
%% Erlang node.
%%
%% Thus, while the types are based on protos found in the opentelemetry-proto
%% repo: src/opentelemetry/proto/trace/v1/trace.proto,
%% they are not exact translations because further processing is done after
%% the span has finished and can be vendor specific. For example, there is
%% no count of the number of dropped attributes in the span record. And
%% an attribute's value can be a function to only evaluate the value if it
%% is actually used (at the time of exporting). And the stacktrace is a
%% regular Erlang stack trace.
%% @end
%%%-------------------------------------------------------------------------
-module(opentelemetry).

-export([set_default_tracer/1,
         set_tracer/2,
         set_meter/2,
         set_default_meter/1,
         register_tracer/2,
         register_application_tracer/1,
         register_meter/2,
         register_application_meter/1,
         get_tracer/0,
         get_tracer/1,
         get_meter/0,
         get_meter/1,
         set_http_extractor/1,
         get_http_extractor/0,
         set_http_injector/1,
         get_http_injector/0,
         timestamp/0,
         timestamp_to_nano/1,
         convert_timestamp/2,
         links/1,
         link/4,
         event/2,
         event/3,
         events/1,
         status/2,
         generate_trace_id/0,
         generate_span_id/0]).

-include("opentelemetry.hrl").
-include_lib("kernel/include/logger.hrl").

-export_type([tracer/0,
              meter/0,
              trace_id/0,
              span_id/0,
              timestamp/0,
              span_name/0,
              span_ctx/0,
              span/0,
              span_kind/0,
              link/0,
              links/0,
              attribute_key/0,
              attribute_value/0,
              attributes/0,
              event/0,
              events/0,
              stack_trace/0,
              tracestate/0,
              status/0,
              resource/0,
              http_headers/0]).

-type tracer()             :: {module(), term()}.
-type meter()              :: {module(), term()}.

-type trace_id()           :: non_neg_integer().
-type span_id()            :: non_neg_integer().

-type timestamp() :: integer().

-type span_ctx()           :: #span_ctx{}.
-type span()               :: term().
-type span_name()          :: unicode:unicode_binary().

-type attribute_key()      :: unicode:unicode_binary().
-type attribute_value()    :: any().
-type attribute()          :: {unicode:unicode_binary(), attribute_value()}.
-type attributes()         :: [attribute()].

-type span_kind()          :: ?SPAN_KIND_INTERNAL    |
                              ?SPAN_KIND_SERVER      |
                              ?SPAN_KIND_CLIENT      |
                              ?SPAN_KIND_PRODUCER    |
                              ?SPAN_KIND_CONSUMER.
-type event()              :: #event{}.
-type events()             :: [#event{}].
-type link()               :: #link{}.
-type links()              :: [#link{}].
-type status()             :: #status{}.

%% The key must begin with a lowercase letter, and can only contain
%% lowercase letters 'a'-'z', digits '0'-'9', underscores '_', dashes
%% '-', asterisks '*', and forward slashes '/'.
%% The value is opaque string up to 256 characters printable ASCII
%% RFC0020 characters (i.e., the range 0x20 to 0x7E) except ',' and '='.
%% Note that this also excludes tabs, newlines, carriage returns, etc.
-type tracestate()         :: [{unicode:latin1_chardata(), unicode:latin1_chardata()}].

-type stack_trace()        :: [erlang:stack_item()].

-type resource()           :: #{unicode:unicode_binary() => unicode:unicode_binary()}.

-type http_headers()       :: [{unicode:unicode_binary(), unicode:unicode_binary()}].

-spec set_default_tracer(tracer()) -> boolean().
set_default_tracer(Tracer) ->
    verify_and_set_term(Tracer, default_tracer, ot_tracer).

-spec set_tracer(atom(), tracer()) -> boolean().
set_tracer(Name, Tracer) ->
    verify_and_set_term(Tracer, Name, ot_tracer).

-spec set_default_meter(meter()) -> boolean().
set_default_meter(Meter) ->
    verify_and_set_term(Meter, default_meter, ot_meter).

-spec set_meter(atom(), meter()) -> boolean().
set_meter(Name, Meter) ->
    verify_and_set_term(Meter, Name, ot_meter).

-spec register_tracer(atom(), string()) -> boolean().
register_tracer(Name, Vsn) ->
    ot_tracer_provider:register_tracer(Name, Vsn).

-spec register_application_tracer(atom()) -> boolean().
register_application_tracer(Name) ->
    ot_tracer_provider:register_application_tracer(Name).

-spec register_meter(atom(), string()) -> boolean().
register_meter(Name, Vsn) ->
    ot_meter_provider:register_meter(Name, Vsn).

-spec register_application_meter(atom()) -> boolean().
register_application_meter(Name) ->
    ot_meter_provider:register_application_meter(Name).

-spec get_tracer() -> tracer().
get_tracer() ->
    persistent_term:get({?MODULE, default_tracer}, {ot_tracer_noop, []}).

-spec get_tracer(atom()) -> tracer().
get_tracer(Name) ->
    persistent_term:get({?MODULE, Name}, get_tracer()).

-spec get_meter() -> meter().
get_meter() ->
    persistent_term:get({?MODULE, default_meter}, {ot_meter_noop, []}).

-spec get_meter(atom()) -> meter().
get_meter(Name) ->
    persistent_term:get({?MODULE, Name}, get_meter()).

set_http_extractor(List) when is_list(List) ->
    persistent_term:put({?MODULE, http_extractor}, List);
set_http_extractor(_) ->
    ok.

set_http_injector(List) when is_list(List) ->
    persistent_term:put({?MODULE, http_injector}, List);
set_http_injector(_) ->
    ok.

get_http_extractor() ->
    persistent_term:get({?MODULE, http_extractor}, []).

get_http_injector() ->
    persistent_term:get({?MODULE, http_injector}, []).

-spec timestamp() -> integer().
timestamp() ->
    erlang:monotonic_time().

-spec timestamp_to_nano(timestamp()) -> integer().
timestamp_to_nano(Timestamp) ->
    convert_timestamp(Timestamp, nanosecond).

-spec convert_timestamp(timestamp(), erlang:time_unit()) -> integer().
convert_timestamp(Timestamp, Unit) ->
    Offset = erlang:time_offset(),
    erlang:convert_time_unit(Timestamp + Offset, native, Unit).

-spec links([{TraceId, SpanId, Attributes, TraceState}]) -> links() when
      TraceId :: trace_id(),
      SpanId :: span_id(),
      Attributes :: attributes(),
      TraceState :: tracestate().
links(List) ->
    [link(TraceId, SpanId, Attributes, TraceState) || {TraceId, SpanId, Attributes, TraceState} <- List,
                                                      is_integer(TraceId),
                                                      is_integer(SpanId),
                                                      is_list(Attributes),
                                                      is_list(TraceState)].

-spec link(TraceId, SpanId, Attributes, TraceState) -> link() | undefined when
      TraceId :: trace_id(),
      SpanId :: span_id(),
      Attributes :: attributes(),
      TraceState :: tracestate().
link(TraceId, SpanId, Attributes, TraceState) when is_integer(TraceId),
                                                   is_integer(SpanId),
                                                   is_list(Attributes),
                                                   is_list(TraceState) ->
    #link{trace_id=TraceId,
          span_id=SpanId,
          attributes=Attributes,
          tracestate=TraceState};
link(_, _, _, _) ->
    undefined.

-spec event(Name, Attributes) -> event() | undefined when
      Name :: unicode:unicode_binary(),
      Attributes :: attributes().
event(Name, Attributes) when is_binary(Name),
                             is_list(Attributes) ->
    event(erlang:system_time(nanosecond), Name, Attributes);
event(_, _) ->
    undefined.

-spec event(Timestamp, Name, Attributes) -> event() | undefined when
      Timestamp :: non_neg_integer(),
      Name :: unicode:unicode_binary(),
      Attributes :: attributes().
event(Timestamp, Name, Attributes) when is_integer(Timestamp),
                                        is_binary(Name),
                                        is_list(Attributes) ->
    #event{system_time_nano=Timestamp,
           name=Name,
           attributes=Attributes};
event(_, _, _) ->
    undefined.

events(List) ->
    Timestamp = timestamp(),
    lists:filtermap(fun({Time, Name, Attributes}) when is_binary(Name),
                                                       is_list(Attributes)  ->
                            case event(Time, Name, Attributes) of
                                undefined ->
                                    false;
                                Event ->
                                    {true, Event}
                            end;
                       ({Name, Attributes}) when is_binary(Name),
                                                 is_list(Attributes) ->
                            case event(Timestamp, Name, Attributes) of
                                undefined ->
                                    false;
                                Event ->
                                    {true, Event}
                            end;
                       (_) ->
                            false
                    end, List).

-spec status(Code, Message) -> status() | undefined when
      Code :: atom(),
      Message :: unicode:unicode_binary().
status(Code, Message) when is_atom(Code),
                           is_binary(Message) ->
    #status{code=Code,
            message=Message};
status(_, _) ->
    undefined.

%%--------------------------------------------------------------------
%% @doc
%% Generates a 128 bit random integer to use as a trace id.
%% @end
%%--------------------------------------------------------------------
-spec generate_trace_id() -> trace_id().
generate_trace_id() ->
    uniform(2 bsl 127 - 1). %% 2 shifted left by 127 == 2 ^ 128

%%--------------------------------------------------------------------
%% @doc
%% Generates a 64 bit random integer to use as a span id.
%% @end
%%--------------------------------------------------------------------
-spec generate_span_id() -> span_id().
generate_span_id() ->
    uniform(2 bsl 63 - 1). %% 2 shifted left by 63 == 2 ^ 64

uniform(X) ->
    rand:uniform(X).

%% internal functions

-spec verify_and_set_term(module() | {module(), term()}, term(), atom()) -> boolean().
verify_and_set_term(Module, TermKey, Behaviour) ->
    case verify_behaviour(Module, Behaviour) of
        true ->
            persistent_term:put({?MODULE, TermKey}, Module),
            true;
        false ->
            ?LOG_WARNING("Module ~p does not implement behaviour ~p. "
                         "A noop ~p will be used until a module implementing "
                         "the behaviour is configured.",
                         [Module, Behaviour, Behaviour]),
            false
    end.

-spec verify_behaviour(module() | {module(), term()}, atom()) -> boolean().
verify_behaviour({Module, _}, Behaviour) ->
    verify_behaviour(Module, Behaviour);
verify_behaviour(Module, Behaviour) ->
    try Module:module_info(attributes) of
        Attributes ->
            case lists:keyfind(behaviour, 1, Attributes) of
                {behaviour, Behaviours} ->
                    lists:member(Behaviour, Behaviours);
                _ ->
                    false
            end
    catch
        error:undef ->
            false
    end.
