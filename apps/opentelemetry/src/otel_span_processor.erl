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
%% @doc Behaviour each Span Processor must implement.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_span_processor).

-export([start_link/2]).

-type processor_config() :: term().

-export_type([processor_config/0]).

-callback processor_init(pid(), processor_config()) -> processor_config().

-callback on_start(otel_ctx:t(), opentelemetry:span(), processor_config()) -> opentelemetry:span().

-callback on_end(opentelemetry:span(), processor_config()) -> true |
                                                              dropped |
                                                              {error, invalid_span} |
                                                              {error, no_export_buffer}.

-callback force_flush(processor_config()) -> ok |
                                             {error, term()}.

-optional_callbacks([processor_init/2]).

%% @doc Starts a span processor.
%% 
%% `Module' must implement the `otel_span_processor' behaviour. This function
%% calls `Module:start_link/1' with `Config' as the argument.
%% @end
-spec start_link(module(), Config) -> {ok, pid(), Config} | {error, term()} when
      Config :: processor_config().
start_link(Module, Config) ->
    case Module:start_link(Config) of
        {ok, Pid} ->
            {ok, Pid, Config};
        {ok, Pid, Config1} ->
            {ok, Pid, Config1};
        {error, _}=Error ->
            Error
    end.
