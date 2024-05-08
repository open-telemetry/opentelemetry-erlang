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

-module(otel_metric_exporter_console).

-export([init/1,
         export/2,
         force_flush/0,
         shutdown/0]).

-include_lib("opentelemetry_api_experimental/include/otel_metrics.hrl").
-include("otel_view.hrl").
-include("otel_metrics2.hrl").

init(_) ->
    {ok, []}.

export(Metrics, _) ->
    io:format("** METRICS FOR DEBUG **~n"),
    lists:map(fun(#metric{name=Name,
                          data=Data}) ->
                      print_metric(Name, Data)
              end, Metrics),
    ok.

force_flush() ->
    ok.

shutdown() ->
    ok.

%%

print_metric(Name, #sum{datapoints=Datapoints}) ->
    lists:map(fun(Datapoint) -> print_datapoint(Name, Datapoint) end, Datapoints);
print_metric(Name, #gauge{datapoints=Datapoints}) ->
    lists:map(fun(Datapoint) -> print_datapoint(Name, Datapoint) end, Datapoints);
print_metric(Name, #histogram{datapoints=Datapoints}) ->
    lists:map(fun(Datapoint) -> print_histogram_datapoint(Name, Datapoint) end, Datapoints).

print_datapoint(Name, #datapoint{
                         attributes=Attributes,
                         start_time_unix_nano=_StartTimeUnixNano,
                         time_unix_nano=_CollectionStartNano,
                         value=Value,
                         exemplars=_,
                         flags=_
                        }) ->
    AttributesString = attributes_string(Attributes),
    io:format("~s{~s} ~p~n", [Name, AttributesString, Value]).

print_histogram_datapoint(Name, #histogram_datapoint{
                                   attributes=Attributes,
                                   start_time_unix_nano=_StartTimeUnixNano,
                                   time_unix_nano=_CollectionStartNano,
                                   count=_Count,
                                   sum=_Sum,
                                   bucket_counts=Buckets,
                                   explicit_bounds=_Boundaries,
                                   exemplars=[],
                                   flags=0,
                                   min=_Min,
                                   max=_Max
                                  }) ->
    AttributesString = attributes_string(Attributes),
    io:format("~s{~s} ~p~n", [Name, AttributesString, Buckets]).

%% need to handle non-string values
attributes_string(Attributes) ->
    lists:join(", ", maps:fold(fun(K, V, Acc) ->
                                       [[K, "=", V] | Acc]
                               end, [], Attributes)).
