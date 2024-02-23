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
         record/4,
         record/5]).

-include("otel_metrics.hrl").

-spec create(Meter, Name, Opts) -> otel_instrument:t() when
      Meter :: otel_meter:t(),
      Name :: otel_instrument:name(),
      Opts :: otel_instrument:opts().
create(Meter, Name, Opts) ->
    otel_meter:create_histogram(Meter, Name, Opts).

-spec record(otel_ctx:t(), otel_instrument:t(), pos_integer() | float()) -> ok.
record(Ctx, Instrument=#instrument{module=Module}, Number) ->
    Module:record(Ctx, Instrument, Number).

-spec record(
        otel_ctx:t(),
        otel_meter:t() | otel_instrument:t(),
        otel_instrument:name() | pos_integer() | float(),
        pos_integer() | float() | opentelemetry:attributes_map()) -> ok.
record(Ctx, Instrument=#instrument{module=Module}, Number, Attributes) ->
    Module:record(Ctx, Instrument, Number, Attributes);

record(Ctx, Meter, Name, Number) ->
    otel_meter:record(Ctx, Meter, Name, Number).

-spec record(otel_ctx:t(), otel_meter:t(), otel_instrument:name(), pos_integer() | float(), opentelemetry:attributes_map()) -> ok.
record(Ctx, Meter, Name, Number, Attributes) ->
    otel_meter:record(Ctx, Meter, Name, Number, Attributes).
