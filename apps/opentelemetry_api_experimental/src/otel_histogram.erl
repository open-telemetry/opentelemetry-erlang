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
%% @doc Histogram is a synchronous Instrument which can be used to report
%% arbitrary values that are likely to be statistically meaningful. It is
%% intended for statistics such as histograms, summaries, and percentile.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_histogram).

-export([create/3,
         record/3,
         record/4]).

-include("otel_metrics.hrl").
-include_lib("kernel/include/logger.hrl").

-spec create(Meter, Name, Opts) -> otel_instrument:t() when
      Meter :: otel_meter:t(),
      Name :: otel_instrument:name(),
      Opts :: otel_meter:opts().
create(Meter, Name, Opts) ->
    otel_meter:create_histogram(Meter, Name, Opts).

-spec record(otel_meter:t(), otel_instrument:name(), number(), opentelemetry:attributes_map()) -> ok.
record(Meter, Name, Number, Attributes) ->
    otel_meter:record(Meter, Name, Number, Attributes).

-spec record(otel_instrument:t(), number(), opentelemetry:attributes_map()) -> ok.
record(Instrument=#instrument{module=Module}, Number, Attributes) ->
    Module:record(Instrument, Number, Attributes).
