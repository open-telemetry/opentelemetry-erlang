%%%------------------------------------------------------------------------
%% Copyright 2023, OpenTelemetry Authors
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
-module(otel_metric_producer).

-export([init/2,
         produce_batch/1]).

-include("otel_metrics.hrl").
-include_lib("kernel/include/logger.hrl").

-callback init(module(), term()) -> {ok, t()} | ignore.
-callback produce_batch(t()) -> [#metric{}].

-record(producer, {module :: module(),
                   state  :: term()}).
-type t() :: #producer{}.

-export_type([t/0]).

-spec init(module(), term()) -> t() | false.
init(Module, Config) ->
    try Module:init(Config) of
        ignore ->
            false;
        {ok, State} ->
            #producer{module=Module,
                      state=State}
    catch
        Kind:Reason:StackTrace ->
            %% any exception during init should just make the Producer be ignored
            ?LOG_INFO("MetricsProducer with module ~p and config ~p failed to initialize with exception: ~p", [Module, Config, otel_utils:format_exception(Kind, Reason, StackTrace)]),
            false
    end.

-spec produce_batch(t()) -> [#metric{}].
produce_batch(#producer{module=Module,
                        state=State}) ->
    Module:produce_batch(State).
