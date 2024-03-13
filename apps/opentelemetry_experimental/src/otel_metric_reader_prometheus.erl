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
%% @doc TODO
%% @end
%%%-------------------------------------------------------------------------
-module(otel_metric_reader_prometheus).

-export([start_link/3,
         collect/0, collect/1,
         shutdown/1]).

-ignore_xref(?MODULE).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").

-define(TEMPORALITY_MAPPING, #{
                               ?KIND_COUNTER => ?TEMPORALITY_CUMULATIVE,
                               ?KIND_OBSERVABLE_COUNTER => ?TEMPORALITY_CUMULATIVE,
                               ?KIND_HISTOGRAM => ?TEMPORALITY_CUMULATIVE,
                               ?KIND_OBSERVABLE_GAUGE => ?TEMPORALITY_CUMULATIVE,
                               ?KIND_UPDOWN_COUNTER => ?TEMPORALITY_CUMULATIVE,
                               ?KIND_OBSERVABLE_UPDOWNCOUNTER => ?TEMPORALITY_CUMULATIVE
                              }).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ReaderId, ProviderSup, Config0) ->
    % TODO warning if default_temporality_mapping, export_interval_ms, exporter
    %      are present in the configuration
    Config1 = maps:remove(export_interval_ms, Config0),
    Config2 = Config1#{default_temporality_mapping => ?TEMPORALITY_MAPPING},
    Config = Config2#{exporter => {otel_metric_exporter_prometheus, Config2}},

    case Config of
        #{server_name := ServerName} ->
            gen_server:start_link(ServerName, otel_metric_reader,
                                  [ReaderId, ProviderSup, Config], []);
        _ ->
            gen_server:start_link(otel_metric_reader,
                                  [ReaderId, ProviderSup, Config], [])
    end.

collect() ->
    collect(?SERVER).

collect(ReaderPid) ->
    otel_metric_reader:collect(ReaderPid).

shutdown(ReaderPid) ->
    otel_metric_reader:shutdown(ReaderPid).
