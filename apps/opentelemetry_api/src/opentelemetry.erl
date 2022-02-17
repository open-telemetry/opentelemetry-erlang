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
         create_application_tracers/1,
         get_tracer/0,
         get_tracer/1,
         get_tracer/3,
         set_tracer/2,
         get_application/1,
         get_application_tracer/1,
         set_text_map_propagator/1,
         set_text_map_extractor/1,
         get_text_map_extractor/0,
         set_text_map_injector/1,
         get_text_map_injector/0,
         instrumentation_library/3,
         timestamp/0,
         timestamp_to_nano/1,
         convert_timestamp/2,
         link/1,
         link/2,
         link/4,
         links/1,
         event/2,
         event/3,
         events/1,
         status/1,
         status/2,
         verify_and_set_term/3]).

-include("opentelemetry.hrl").
-include_lib("kernel/include/logger.hrl").

-export_type([tracer/0,
              instrumentation_library/0,
              trace_id/0,
              span_id/0,
              hex_trace_id/0,
              hex_span_id/0,
              trace_flags/0,
              timestamp/0,
              span_name/0,
              span_ctx/0,
              span/0,
              span_kind/0,
              link/0,
              attribute_key/0,
              attribute_value/0,
              attribute/0,
              attributes_map/0,
              event/0,
              event_name/0,
              tracestate/0,
              status/0,
              status_code/0,
              resource/0,
              text_map/0]).

-type tracer()             :: {module(), term()}.

-type instrumentation_library() :: #instrumentation_library{}.

-type trace_id()           :: non_neg_integer().
-type span_id()            :: non_neg_integer().

-type hex_trace_id()       :: binary().
-type hex_span_id()        :: binary().

-type trace_flags()        :: non_neg_integer().

-type timestamp()          :: integer().

-type span_ctx()           :: #span_ctx{}.
-type span()               :: term().
-type span_name()          :: unicode:unicode_binary() | atom().

-type attribute_key()      :: unicode:unicode_binary() | atom().
-type attribute_value()    :: unicode:unicode_binary()   |
                              atom()                     |
                              number()                   |
                              boolean()                  |
                              [unicode:unicode_binary() | atom() | float() | integer() | boolean()] |
                              {unicode:unicode_binary() | atom() | float() | integer() | boolean()}.
-type attribute()          :: {attribute_key(), attribute_value()}.
-type attributes_map()     :: #{attribute_key() => attribute_value()} |
                              [attribute()].

-type span_kind()          :: ?SPAN_KIND_INTERNAL    |
                              ?SPAN_KIND_SERVER      |
                              ?SPAN_KIND_CLIENT      |
                              ?SPAN_KIND_PRODUCER    |
                              ?SPAN_KIND_CONSUMER.
-type event()              :: #{system_time_nano => non_neg_integer(),
                                name            := event_name(),
                                attributes      := attributes_map()}.
-type event_name()         :: unicode:unicode_binary() | atom().
-type link()               :: #{trace_id   := trace_id(),
                                span_id    := span_id(),
                                attributes := attributes_map(),
                                tracestate := tracestate()}.
-type status()             :: #status{}.
-type status_code()        :: ?OTEL_STATUS_UNSET | ?OTEL_STATUS_OK | ?OTEL_STATUS_ERROR.

%% The key must begin with a lowercase letter, and can only contain
%% lowercase letters 'a'-'z', digits '0'-'9', underscores '_', dashes
%% '-', asterisks '*', and forward slashes '/'.
%% The value is opaque string up to 256 characters printable ASCII
%% RFC0020 characters (i.e., the range 0x20 to 0x7E) except ',' and '='.
%% Note that this also excludes tabs, newlines, carriage returns, etc.
-type tracestate()         :: [{unicode:latin1_chardata(), unicode:latin1_chardata()}].

-type resource()           :: #{unicode:unicode_binary() => unicode:unicode_binary()}.

-type text_map()       :: [{unicode:unicode_binary(), unicode:unicode_binary()}].

-define(TRACER_KEY(Name), {?MODULE, tracer, Name}).
-define(DEFAULT_TRACER_KEY, ?TRACER_KEY('$__default_tracer')).
-define(MODULE_TO_APPLICATION_KEY, {?MODULE, otel_module_to_application_key}).
-define(TEXT_MAP_EXTRACTOR_KEY, {?MODULE, text_map_extractor}).
-define(TEXT_MAP_INJECTOR_KEY, {?MODULE, text_map_injector}).

-spec set_default_tracer(tracer()) -> boolean().
set_default_tracer(Tracer) ->
    verify_and_set_term(Tracer, ?DEFAULT_TRACER_KEY, otel_tracer).

-spec set_tracer(atom(), tracer()) -> boolean().
set_tracer(Name, Tracer) ->
    set_tracer(Name, <<>>, undefined, Tracer).

-spec set_tracer(Name, Vsn, SchemaUrl, Tracer) -> boolean() when
      Name :: atom(),
      Vsn :: unicode:chardata() | undefined,
      SchemaUrl :: uri_string:uri_string() | undefined,
      Tracer:: opentelemetry:tracer().
set_tracer(Name, Vsn, SchemaUrl, Tracer) ->
    verify_and_set_term(Tracer, ?TRACER_KEY({Name, Vsn, SchemaUrl}), otel_tracer).

-spec create_application_tracers([{Application, Description, Vsn}]) -> ok when
      Application :: atom(),
      Description :: string(),
      Vsn :: string().
create_application_tracers(Applications) ->
    TracerMap = lists:foldl(fun({Name, _Description, Version}, Acc) ->
                                    Vsn = vsn_to_binary(Version),
                                    SchemaUrl = application:get_env(Name, otel_schema_url, undefined),
                                    _ = get_tracer(Name, Version, SchemaUrl),
                                    maps:merge(Acc, module_to_application(Name, Vsn, SchemaUrl))
                            end, #{}, Applications),
    persistent_term:put(?MODULE_TO_APPLICATION_KEY, TracerMap).

%% creates a map of modules to application name, version and schema_url tuple
module_to_application(Name, Version, SchemaUrl) ->
    {ok, Modules} = application:get_key(Name, modules),
    lists:foldl(fun(M, Acc) ->
                        Acc#{M => {Name, Version, SchemaUrl}}
                end, #{}, Modules).

-spec get_tracer() -> tracer().
get_tracer() ->
    persistent_term:get(?DEFAULT_TRACER_KEY, {otel_tracer_noop, []}).

-spec get_tracer(Name) -> Tracer when
      Name :: atom() | {Name, Vsn, SchemaUrl},
      Vsn :: unicode:chardata() | undefined,
      SchemaUrl :: uri_string:uri_string() | undefined,
      Tracer:: opentelemetry:tracer().
get_tracer('$__default_tracer') ->
    get_tracer();
get_tracer({Name, Vsn, SchemaUrl}) ->
    get_tracer(Name, Vsn, SchemaUrl);
get_tracer(Name) ->
    get_tracer(Name, undefined, undefined).

-spec get_tracer(Name, Vsn, SchemaUrl) -> Tracer when
      Name :: atom(),
      Vsn :: unicode:chardata() | undefined,
      SchemaUrl :: uri_string:uri_string() | undefined,
      Tracer:: opentelemetry:tracer().
get_tracer(Name, Vsn, SchemaUrl) ->
    %% check cache and then use provider to get the tracer if it isn't cached yet
    case persistent_term:get(?TRACER_KEY({Name, Vsn, SchemaUrl}), undefined) of
        undefined ->
            VsnBin = vsn_to_binary(Vsn),
            Tracer = otel_tracer_provider:get_tracer(Name, VsnBin, SchemaUrl),

            %% cache the tracer
            _ = set_tracer(Name, Vsn, SchemaUrl, Tracer),

            Tracer;
        Tracer ->
            Tracer
    end.

-spec get_application_tracer(module()) -> tracer().
get_application_tracer(ModuleName) ->
    get_tracer(get_application(ModuleName)).

%% looks up the name, version and schema_url used to create a Trace for the OTP
%% Application a module is in. This name is used to look up a Tracer to use so
%% if none is found for the ModuleName the key used for the default tracer.
-spec get_application(module()) -> ApplicationTuple when
      ApplicationTuple :: {Name, Vsn, SchemaUrl} | atom(),
      Name :: atom(),
      Vsn :: unicode:unicode_binary() | undefined,
      SchemaUrl :: uri_string:uri_string() | undefined.
get_application(ModuleName) ->
    Map = persistent_term:get(?MODULE_TO_APPLICATION_KEY, #{}),
    maps:get(ModuleName, Map, '$__default_tracer').

%% setting the propagator is the same as setting the same injector and extractor
set_text_map_propagator(Propagator) ->
    set_text_map_injector(Propagator),
    set_text_map_extractor(Propagator).

set_text_map_extractor(Propagator) ->
    persistent_term:put(?TEXT_MAP_EXTRACTOR_KEY, Propagator).

set_text_map_injector(Propagator) ->
    persistent_term:put(?TEXT_MAP_INJECTOR_KEY, Propagator).

get_text_map_extractor() ->
    persistent_term:get(?TEXT_MAP_EXTRACTOR_KEY, otel_propagator_text_map_noop).

get_text_map_injector() ->
    persistent_term:get(?TEXT_MAP_INJECTOR_KEY, otel_propagator_text_map_noop).

%% @doc A monotonically increasing time provided by the Erlang runtime system in the native time unit.
%% This value is the most accurate and precise timestamp available from the Erlang runtime and
%% should be used for finding durations or any timestamp that can be converted to a system
%% time before being sent to another system.

%% Use {@link convert_timestamp/2} or {@link timestamp_to_nano/1} to convert a native monotonic time to a
%% system time of either nanoseconds or another {@link erlang:time_unit()}.

%% Using these functions allows timestamps to be accurate, used for duration and be exportable
%% as POSIX time when needed.
%% @end
-spec timestamp() -> integer().
timestamp() ->
    erlang:monotonic_time().

%% @doc Convert a native monotonic timestamp to nanosecond POSIX time. Meaning the time since Epoch.
%% Epoch is defined to be 00:00:00 UTC, 1970-01-01.
%% @end
-spec timestamp_to_nano(timestamp()) -> integer().
timestamp_to_nano(Timestamp) ->
    convert_timestamp(Timestamp, nanosecond).

%% @doc Convert a native monotonic timestamp to POSIX time of any {@link erlang:time_unit()}.
%% Meaning the time since Epoch. Epoch is defined to be 00:00:00 UTC, 1970-01-01.
%% @end
-spec convert_timestamp(timestamp(), erlang:time_unit()) -> integer().
convert_timestamp(Timestamp, Unit) ->
    Offset = erlang:time_offset(),
    erlang:convert_time_unit(Timestamp + Offset, native, Unit).

-spec links([{TraceId, SpanId, Attributes, TraceState} | span_ctx() | {span_ctx(), Attributes}]) -> [link()] when
      TraceId :: trace_id(),
      SpanId :: span_id(),
      Attributes :: attributes_map(),
      TraceState :: tracestate().
links(List) when is_list(List) ->
    lists:filtermap(fun({TraceId, SpanId, Attributes, TraceState}) when is_integer(TraceId) ,
                                                                        is_integer(SpanId) ,
                                                                        is_list(TraceState) ->
                            link_or_false(TraceId, SpanId, otel_span:process_attributes(Attributes), TraceState);
                       ({#span_ctx{trace_id=TraceId,
                                   span_id=SpanId,
                                   tracestate=TraceState}, Attributes}) when is_integer(TraceId) ,
                                                                             is_integer(SpanId) ,
                                                                             is_list(TraceState) ->
                            link_or_false(TraceId, SpanId, otel_span:process_attributes(Attributes), TraceState);
                       (#span_ctx{trace_id=TraceId,
                                  span_id=SpanId,
                                  tracestate=TraceState}) when is_integer(TraceId) ,
                                                               is_integer(SpanId) ,
                                                               is_list(TraceState) ->
                            link_or_false(TraceId, SpanId, [], TraceState);
                       (_) ->
                            false
              end, List);
links(_) ->
    [].

-spec link(span_ctx() | undefined) -> link().
link(SpanCtx) ->
    link(SpanCtx, []).

-spec link(span_ctx() | undefined, attributes_map()) -> link().
link(#span_ctx{trace_id=TraceId,
               span_id=SpanId,
               tracestate=TraceState}, Attributes) ->
    ?MODULE:link(TraceId, SpanId, otel_span:process_attributes(Attributes), TraceState);
link(_, _) ->
    undefined.

-spec link(TraceId, SpanId, Attributes, TraceState) -> link() | undefined when
      TraceId :: trace_id(),
      SpanId :: span_id(),
      Attributes :: attributes_map(),
      TraceState :: tracestate().
link(TraceId, SpanId, Attributes, TraceState) when is_integer(TraceId),
                                                   is_integer(SpanId),
                                                   (is_list(Attributes) orelse is_map(Attributes)),
                                                   is_list(TraceState) ->
    #{trace_id => TraceId,
      span_id => SpanId,
      attributes => otel_span:process_attributes(Attributes),
      tracestate => TraceState};
link(_, _, _, _) ->
    undefined.

-spec event(Name, Attributes) -> event() | undefined when
      Name :: event_name(),
      Attributes :: attributes_map().
event(Name, Attributes) ->
    event(opentelemetry:timestamp(), Name, Attributes).

-spec event(Timestamp, Name, Attributes) -> event() | undefined when
      Timestamp :: non_neg_integer(),
      Name :: event_name(),
      Attributes :: attributes_map().
event(Timestamp, Name, Attributes) when is_integer(Timestamp),
                                        (is_list(Attributes) orelse is_map(Attributes)) ->

    case otel_span:is_valid_name(Name) of
        true ->
            #{system_time_nano => Timestamp,
              name => Name,
              attributes => otel_span:process_attributes(Attributes)};
        false ->
            undefined
    end;
event(_, _, _) ->
    undefined.

events(List) ->
    Now = opentelemetry:timestamp(),
    lists:filtermap(fun({Time, Name, Attributes}) ->
                            case event(Time, Name, Attributes) of
                                undefined ->
                                    false;
                                Event ->
                                    {true, Event}
                            end;
                       ({Name, Attributes}) ->
                            case event(Now, Name, Attributes) of
                                undefined ->
                                    false;
                                Event ->
                                    {true, Event}
                            end;
                       (_) ->
                            false
                    end, List).

-spec status(Code) -> status() | undefined when
      Code :: status_code().
status(Code) ->
    status(Code, <<>>).

-spec status(Code, Message) -> status() | undefined when
      Code :: status_code(),
      Message :: unicode:unicode_binary().
status(?OTEL_STATUS_ERROR, Message) when is_binary(Message) ->
    #status{code=?OTEL_STATUS_ERROR, message=Message};
status(?OTEL_STATUS_OK, _Message) ->
    #status{code=?OTEL_STATUS_OK};
status(?OTEL_STATUS_UNSET, _Message) ->
    #status{code=?OTEL_STATUS_UNSET};
status(_, _) ->
    undefined.

%% internal functions

-spec verify_and_set_term(module() | {module(), term()}, term(), atom()) -> boolean().
verify_and_set_term(Module, TermKey, Behaviour) ->
    case verify_module_exists(Module) of
        true ->
            persistent_term:put(TermKey, Module),
            true;
        false ->
            ?LOG_WARNING("Module ~p does not exist. "
                         "A noop ~p will be used until a module is configured.",
                         [Module, Behaviour]),
            false
    end.

-spec verify_module_exists(module() | {module(), term()}) -> boolean().
verify_module_exists({Module, _}) ->
    verify_module_exists(Module);
verify_module_exists(Module) ->
    try Module:module_info() of
        _ ->
          true
    catch
        error:undef ->
            false
    end.

%% for use in a filtermap
%% return {true, Link} if a link is returned or return false
link_or_false(TraceId, SpanId, Attributes, TraceState) ->
    case link(TraceId, SpanId, Attributes, TraceState) of
        Link=#{}->
            {true, Link};
        _ ->
            false
    end.

instrumentation_library(Name, Vsn, SchemaUrl) ->
    case name_to_binary(Name) of
        undefined ->
            undefined;
        BinaryName ->
            #instrumentation_library{name=BinaryName,
                                     version=vsn_to_binary(Vsn),
                                     schema_url=schema_url_to_binary(SchemaUrl)}
    end.

%% schema_url is option, so set to undefined if its not a string
schema_url_to_binary(SchemaUrl) when is_binary(SchemaUrl) ; is_list(SchemaUrl) ->
    unicode:characters_to_binary(SchemaUrl);
schema_url_to_binary(_) ->
    undefined.

%% Vsn can't be an atom or anything but a list or binary
%% so return empty binary string if it isn't a list or binary.
vsn_to_binary(Vsn) when is_binary(Vsn) ; is_list(Vsn) ->
    unicode:characters_to_binary(Vsn);
vsn_to_binary(_) ->
    <<>>.

%% Instrumentation name can be atom, list or binary. But atom `undefined'
%% must stay as `undefined' atom.
name_to_binary(undefined)->
    undefined;
name_to_binary(T) when is_atom(T) ->
    atom_to_binary(T, utf8);
name_to_binary(T) when is_list(T) ->
    list_to_binary(T);
name_to_binary(T) when is_binary(T) ->
    T.
