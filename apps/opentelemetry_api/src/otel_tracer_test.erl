%%%------------------------------------------------------------------------
%% Copyright 2024, OpenTelemetry Authors
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
%% This module is used for testing the API by arranging for the calls that would
%% have been made to the SDK instead be send to the current process as
%% messages. This allows the test process to inspect and assert on them.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_tracer_test).

-behaviour(otel_tracer).

-export([set_default/0,
         start_span/4,
         with_span/5,
         end_span/2]).

-include("opentelemetry.hrl").
-include("otel_tracer.hrl").

%% @doc This helper acts as a default tracer provider that can be used in tests
%% to point to this tracer, either using Elixir or Erlang, since Elixir won't
%% have access to the necessary Erlang macro.
set_default() ->
    opentelemetry:set_default_tracer(?GLOBAL_TRACER_PROVIDER_NAME, {?MODULE, []}),
    ok.

-spec start_span(otel_ctx:t(), opentelemetry:tracer(), opentelemetry:span_name(),
                 otel_span:start_config()) -> opentelemetry:span_ctx().
start_span(Ctx, Tracer, Name, Opts) ->
    self() ! {?FUNCTION_NAME, Ctx, Tracer, Name, Opts},
    #span_ctx{trace_id=rand:uniform(2 bsl 127 - 1), %% 2 shifted left by 127 == 2 ^ 128
              span_id=rand:uniform(2 bsl 63 - 1), %% 2 shifted left by 63 == 2 ^ 64
              trace_flags=0,
              tracestate=otel_tracestate:new(),
              is_valid=true,
              is_recording=true,
              %% The critical functionality here is to set the span_sdk to the
              %% one that sends messages to the process mailbox.
              span_sdk={otel_span_mailbox, []}}.

-spec with_span(otel_ctx:t(), opentelemetry:tracer(), opentelemetry:span_name(),
                otel_span:start_config(), otel_tracer:traced_fun(T)) -> T.
with_span(Ctx, Tracer, SpanName, Opts, Fun) ->
    self() ! {?FUNCTION_NAME, Ctx, Tracer, SpanName, Opts, Fun},
    SpanCtx = start_span(Ctx, Tracer, SpanName, Opts),
    Ctx1 = otel_tracer:set_current_span(Ctx, SpanCtx),
    otel_ctx:attach(Ctx1),
    try
        Fun(SpanCtx)
    after
        otel_ctx:attach(Ctx)
    end.

-spec end_span(opentelemetry:tracer(), opentelemetry:span_ctx())
              -> boolean() | {error, term()}.
end_span(Tracer, Ctx) ->
    self() ! {?FUNCTION_NAME, Tracer, Ctx},
    true.
