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
%% @doc UpDownCounter is a synchronous Instrument which supports increments
%% and decrements.
%% @end
%%%-------------------------------------------------------------------------
-module(otel_updown_counter).

-export([create/3,
         add/4]).

-include("otel_metrics.hrl").

-spec create(Meter, Name, Opts) -> otel_instrument:t() when
      Meter :: otel_meter:t(),
      Name :: otel_instrument:name(),
      Opts :: otel_instrument:opts().
create(Meter, Name, Opts) ->
    otel_meter:create_updown_counter(Meter, Name, Opts).

-spec add(otel_ctx:t(), otel_instrument:t() | otel_instrument:alias(), number(),
          opentelemetry:attributes_map()) -> ok | false.
add(Ctx, Instrument=#instrument{kind=?KIND_UPDOWN_COUNTER}, Number, Attributes) ->
    otel_instrument:record(Ctx, Instrument, Number, Attributes);
add(Ctx, Alias, Number, Attributes) when is_atom(Alias) ->
    case otel_instrument:lookup_alias(Alias) of
        {ok, Instrument} -> add(Ctx, Instrument, Number, Attributes);
        error -> false
    end;
add(_, _, _, _) ->
    false.
