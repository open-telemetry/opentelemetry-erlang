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
         set_default_context_manager/1,
         get_context_manager/0,
         get_tracer/0,
         get_tracer/1,
         timestamp/0,
         links/1,
         link/4,
         event/2,
         timed_event/2,
         timed_event/3,
         timed_events/1,
         status/2,
         generate_trace_id/0,
         generate_span_id/0]).

-include("opentelemetry.hrl").

-export_type([tracer/0,
              trace_id/0,
              span_id/0,
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
              timed_event/0,
              timed_events/0,
              stack_trace/0,
              tracestate/0,
              status/0,
              resource/0,
              http_headers/0]).

-type tracer()             :: {module(), term()}.

-type trace_id()           :: non_neg_integer().
-type span_id()            :: non_neg_integer().

-type span_ctx()           :: #span_ctx{}.
-type span()               :: #span{}.
-type span_name()          :: unicode:unicode_binary().

-type attribute_key()      :: unicode:unicode_binary().
-type attribute_value()    :: any().
-type attribute()          :: {unicode:unicode_binary(), attribute_value()}.
-type attributes()         :: [attribute()].

-type span_kind()          :: ?SPAN_KIND_INTERNAL |
                              ?SPAN_KIND_SERVER   |
                              ?SPAN_KIND_CLIENT.
-type event()              :: #event{}.
-type timed_event()        :: #timed_event{}.
-type timed_events()       :: [#timed_event{}].
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

set_default_tracer(Tracer) ->
    persistent_term:put({?MODULE, default_tracer}, Tracer).

set_default_context_manager(ContextModule) ->
    persistent_term:put({?MODULE, context_manager}, ContextModule).

get_context_manager() ->
    persistent_term:get({?MODULE, context_manager}, {ot_ctx_noop, []}).

get_tracer() ->
    persistent_term:get({?MODULE, default_tracer}, {ot_tracer_noop, []}).

get_tracer(_Name) ->
    persistent_term:get({?MODULE, default_tracer}, {ot_tracer_noop, []}).

-spec timestamp() -> integer().
timestamp() ->
    erlang:system_time(nanosecond).

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
    #event{name=Name,
           attributes=Attributes};
event(_, _) ->
    undefined.

-spec timed_event(TimeUnixNano, Name, Attributes) -> timed_event() | undefined when
      TimeUnixNano :: integer(),
      Name :: unicode:unicode_binary(),
      Attributes :: attributes().
timed_event(TimeUnixNano, Name, Attributes) when is_integer(TimeUnixNano),
                                                 is_binary(Name),
                                                 is_list(Attributes) ->
    timed_event(TimeUnixNano, event(Name, Attributes));
timed_event(_, _, _) ->
    undefined.

-spec timed_event(TimeUnixNano, Event) -> timed_event() | undefined when
      TimeUnixNano :: integer(),
      Event :: event().
timed_event(TimeUnixNano, Event) when
      is_integer(TimeUnixNano),
      is_record(Event, event) ->
    #timed_event{time_unixnano=TimeUnixNano,
                 event=Event};
timed_event(_, _) ->
    undefined.

timed_events(List) ->
    [timed_event(TimeUnixNano, Name, Attributes) || {TimeUnixNano, Name, Attributes} <- List,
                                                    is_integer(TimeUnixNano),
                                                    is_binary(Name),
                                                    is_list(Attributes)].

-spec status(Code, Message) -> status() | undefined when
      Code :: integer(),
      Message :: unicode:unicode_binary().
status(Code, Message) when is_integer(Code),
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
