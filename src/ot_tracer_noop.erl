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
-module(ot_tracer_noop).

-behaviour(ot_tracer).

-export([setup/1,
         start_span/2,
         with_span/1,
         with_span/2,
         finish/0,
         current_span_ctx/0,
         get_binary_format/0,
         get_http_text_format/0]).

-include("opentelemetry.hrl").

-define(NOOP_SPAN_CTX, #span_ctx{trace_id=0,
                                 span_id=0,
                                 trace_flags=0,
                                 tracestate=[],
                                 is_valid=false}).

-spec setup(map()) -> [supervisor:child_spec()].
setup(_Opts) ->
    [].

-spec start_span(opentelemetry:span_name(), ot_span:start_opts()) -> opentelemetry:span_ctx().
start_span(_Name, _) ->
    ?NOOP_SPAN_CTX.

-spec with_span(opentelemetry:span_ctx()) -> ok.
with_span(_SpanCtx) ->
    ok.

-spec with_span(opentelemetry:span_ctx(), fun()) -> ok.
with_span(_SpanCtx, _) ->
    ok.

-spec current_span_ctx() -> opentelemetry:span_ctx().
current_span_ctx() ->
    ?NOOP_SPAN_CTX.

-spec finish() -> ok.
finish() ->
    ok.

-spec get_binary_format() -> binary().
get_binary_format() ->
    <<>>.

-spec get_http_text_format() -> opentelemetry:http_headers().
get_http_text_format() ->
    [].
