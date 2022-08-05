%%%------------------------------------------------------------------------
%% Copyright 2022, OpenTelemetry Authors
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

-module(otel_metric_exporter_pid).

-export([init/1,
         export/2,
         force_flush/0,
         shutdown/0]).

-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_view.hrl").
-include("otel_metrics.hrl").

init({Tag, Pid}) ->
    {ok, {Tag, Pid}};
init(Pid) ->
    {ok, {otel_metric, Pid}}.

export(Metrics, {Tag, Pid}) ->
    lists:map(fun(Metric) ->
                  Pid ! {Tag, Metric}
              end, Metrics),
    ok.

force_flush() ->
    ok.

shutdown() ->
    ok.

%%
