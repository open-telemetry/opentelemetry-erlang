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
%% @end
%%%-------------------------------------------------------------------------
-module(otel).

-export([start_span/1,
         start_span/2,
         with_span/1,
         with_span/2,
         current_span_ctx/0,
         end_span/0%% ,
         %% get_binary_format/0,
         %% get_http_text_format/0
        ]).

-export([get_ctx/1,
         is_recording_events/0,
         set_attribute/2,
         set_attributes/1,
         add_events/1,
         add_links/1,
         set_status/1,
         update_name/1]).

-include("opentelemetry.hrl").
%% -include("ot_tracer.hrl").

-type start_opts() :: #{parent => undefined | opentelemetry:span() | opentelemetry:span_ctx(),
                        sampler => ot_sampler:sampler(),
                        links => opentelemetry:links(),
                        is_recorded => boolean(),
                        kind => opentelemetry:span_kind()}.

-export_type([start_opts/0]).

-callback start_span(opentelemetry:span_name(), start_opts()) -> opentelemetry:span_ctx().
-callback end_span(opentelemetry:span_ctx()) -> boolean() | {error, term()}.
-callback get_ctx(opentelemetry:span()) -> opentelemetry:span_ctx().
-callback is_recording_events(opentelemetry:span_ctx()) -> boolean().
-callback set_attribute(opentelemetry:span_ctx(),
                        opentelemetry:attribute_key(),
                        opentelemetry:attribute_value()) -> boolean().
-callback set_attributes(opentelemetry:span_ctx(), opentelemetry:attributes()) -> boolean().
-callback add_events(opentelemetry:span_ctx(), opentelemetry:time_events()) -> boolean().
-callback set_status(opentelemetry:span_ctx(), opentelemetry:status()) -> boolean().
-callback update_name(opentelemetry:span_ctx(), opentelemetry:span_name()) -> boolean().

%% handy macros so we don't have function name typos
-define(DO(Args), do_span_function(?FUNCTION_NAME, Args)).

-spec start_span(opentelemetry:span_name()) -> opentelemetry:span_ctx().
start_span(Name) ->
    start_span(Name, #{}).

-spec start_span(opentelemetry:span_name(), ot_span:start_opts()) -> opentelemetry:span_ctx().
start_span(SpanName, Opts) ->
    Tracer = opentelemetry:get_tracer(),
    ot_tracer:start_span(Tracer, SpanName, Opts).

-spec with_span(opentelemetry:span_ctx()) -> ok.
with_span(Span) ->
    Tracer = opentelemetry:get_tracer(),
    ot_tracer:with_span(Tracer, Span).

-spec with_span(opentelemetry:span_ctx(), opentelemetry:span_ctx() | fun()) -> ok.
with_span(Span=#span_ctx{}, Fun) ->
    Tracer = opentelemetry:get_tracer(),
    ot_tracer:with_span(Tracer, Span, Fun).

-spec end_span() -> ok.
end_span() ->
    Tracer = opentelemetry:get_tracer(),
    ot_tracer:end_span(Tracer).

current_span_ctx() ->
    Tracer = opentelemetry:get_tracer(),
    ot_tracer:current_span_ctx(Tracer).

%% access span functions

get_ctx(Span) ->
    ?DO([Span]).

is_recording_events() ->
    ?DO([]).

%% manipulate span functions

set_attribute(Key, Value) ->
    ?DO([Key, Value]).

set_attributes(Attributes) ->
    ?DO([Attributes]).

add_events(Events) ->
    ?DO([Events]).

add_links(Links) ->
    ?DO([Links]).

set_status(Status) ->
    ?DO([Status]).

update_name(Name) ->
    ?DO([Name]).

%% internal functions

do_span_function(Function, Args) ->
    Tracer = opentelemetry:get_tracer(),
    SpanCtx = ot_tracer:current_span_ctx(Tracer),
    SpanModule = ot_tracer:span_module(Tracer),
    apply_span_function(SpanModule, Function, [SpanCtx | Args]).

apply_span_function(ot_span_noop, _Function, _Args) ->
    ok;
apply_span_function(SpanModule, Function, Args) ->
    erlang:apply(SpanModule, Function, Args).
