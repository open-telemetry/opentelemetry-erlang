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
%% @doc Counter is a synchronous Instrument which supports non-negative
%% increments.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_counter).

-export([create/3,
         add/5]).

-spec create(Meter, Name, Opts) -> otel_instrument:t() when
      Meter :: otel_meter:t(),
      Name :: otel_instrument:name(),
      Opts :: otel_instrument:opts().
create(Meter, Name, Opts) ->
    otel_meter:create_counter(Meter, Name, Opts).

-spec add(otel_ctx:t(), otel_meter:t(), otel_instrument:name(), pos_integer() | float(), opentelemetry:attributes_map()) -> ok | false.
add(Ctx, Meter, Name, Number, Attributes) ->
    otel_meter:record(Ctx, Meter, Name, Number, Attributes).
