%%%------------------------------------------------------------------------
%% Copyright 2026, OpenTelemetry Authors
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
%% @doc Gauge is a synchronous Instrument which can be used to record
%% non-additive measurements (e.g. the room temperature - it makes no sense
%% to sum the temperature values from multiple rooms) when the value changes.
%%
%% Note: if the values are additive, use Counter or UpDownCounter. If the
%% value is only available by polling, use Asynchronous Gauge
%% (`otel_observable_gauge').
%% @end
%%%-------------------------------------------------------------------------
-module(otel_gauge).

-export([create/3,
         set/5]).

-spec create(Meter, Name, Opts) -> otel_instrument:t() when
      Meter :: otel_meter:t(),
      Name :: otel_instrument:name(),
      Opts :: otel_instrument:opts().
create(Meter, Name, Opts) ->
    otel_meter:create_gauge(Meter, Name, Opts).

%% @doc Records the current value; `Number', may be negative.
-spec set(otel_ctx:t(), otel_meter:t(), otel_instrument:name(), number(),
          opentelemetry:attributes_map()) -> ok | false.
set(Ctx, Meter, Name, Number, Attributes) ->
    otel_meter:record(Ctx, Meter, Name, Number, Attributes).
