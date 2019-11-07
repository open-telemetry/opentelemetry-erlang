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

-export([set_default_tracer_registry/1,
         get_tracer/0,
         get_tracer/1,
         generate_trace_id/0,
         generate_span_id/0]).

-include("opentelemetry.hrl").

-export_type([trace_id/0,
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
              annotation/0,
              time_events/0,
              message_event/0,
              message_event_type/0,
              stack_trace/0,
              tracestate/0,
              status/0,
              resource/0,
              http_headers/0]).

-type trace_id()           :: non_neg_integer().
-type span_id()            :: non_neg_integer().

-type span_ctx()           :: #span_ctx{}.
-type span()               :: #span{}.
-type span_name()          :: unicode:unicode_binary().

-type attribute_key()      :: unicode:unicode_binary().
-type attribute_value()    :: any().
-type attribute()          :: {unicode:unicode_binary(), attribute_value()}.
-type attributes()         :: [attribute()].

-type annotation()         :: #annotation{}.
-type span_kind()          :: ?SPAN_KIND_INTERNAL |
                              ?SPAN_KIND_SERVER   |
                              ?SPAN_KIND_CLIENT.
-type message_event()      :: #message_event{}.
-type message_event_type() :: ?MESSAGE_EVENT_TYPE_UNSPECIFIED |
                              ?MESSAGE_EVENT_TYPE_SENT        |
                              ?MESSAGE_EVENT_TYPE_RECEIVED.
-type time_events()        :: [{wts:timestamp(), annotation() | message_event()}].
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

set_default_tracer_registry(TracerRegistry) ->
    persistent_term:put({?MODULE, default_tracer_registry}, TracerRegistry).

get_tracer() ->
    (persistent_term:get({?MODULE, default_tracer_registry}, ot_registry_api)):get().

get_tracer(Name) ->
    (persistent_term:get({?MODULE, default_tracer_registry}, ot_registry_api)):get(Name).

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
